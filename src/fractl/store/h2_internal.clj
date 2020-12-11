(ns fractl.store.h2-internal
  (:require [next.jdbc :as jdbc]
            [next.jdbc.prepare :as jdbcp])
  (:import [java.sql PreparedStatement]))

<<<<<<< HEAD
(defn- create-entity-table-sql
  "Given a database-type, entity-table-name and identity-attribute name,
  return the DML statement to create that table."
  [tabname ident-attr]
  (str dbi/create-table-prefix " " tabname " "
       (if ident-attr
         (str "(" (dbi/db-ident ident-attr) " UUID, ")
         "(")
       "instance_json JSON)"))

(defn- create-index-table-sql
  "Given a database-type, entity-table-name and attribute-column name, return the
  DML statements for creating an index table and the index for its 'id' column."
  [entity-table-name colname coltype unique?]
  (let [index-tabname (dbi/index-table-name entity-table-name colname)]
    [(str dbi/create-table-prefix " " index-tabname " "
          ;; `id` is not a foreign key reference to the main table,
          ;; because insert is fully controlled by the V8 runtime and
          ;; we get an index for free.
          "(id UUID PRIMARY KEY, "
          ;; Storage and search can be optimized by inferring a more appropriate
          ;; SQL type for `colname`, see the issue https://ventur8.atlassian.net/browse/V8DML-117.
          colname " " coltype
          (if unique? (str ",UNIQUE(" colname "))") ")"))
     (dbi/create-index-sql index-tabname colname unique?)]))

(defn- create-identity-index-sql [entity-table-name colname]
  (str dbi/create-unique-index-prefix
       " " (dbi/index-name entity-table-name)
       " ON " entity-table-name "(" colname ")"))

(defn- create-identity-index! [connection entity-table-name ident-attr]
  (let [sql (create-identity-index-sql entity-table-name (dbi/db-ident ident-attr))]
    (if (jdbc/execute! connection [sql])
      entity-table-name
      (u/throw-ex (str "Failed to create index table for identity column - "
                       [entity-table-name ident-attr])))))

(defn- create-entity-table!
  "Create a table to store instances of an entity. As 'identity-attribute' is
  specified to be used as the primary-key in the table."
  [connection tabname ident-attr]
  (let [sql (create-entity-table-sql tabname ident-attr)]
    (if (jdbc/execute! connection [sql])
      tabname
      (u/throw-ex (str "Failed to create table for " tabname)))))

(defn- create-index-table! [connection entity-schema entity-table-name attrname idxattr]
  (let [[tabsql idxsql] (create-index-table-sql
                         entity-table-name attrname
                         (sql/sql-index-type (cn/attribute-type entity-schema idxattr))
                         (cn/unique-attribute? entity-schema idxattr))]
    (when-not (and (jdbc/execute! connection [tabsql])
                   (jdbc/execute! connection [idxsql]))
      (u/throw-ex (str "Failed to create lookup table for " [entity-table-name attrname])))))

(defn- create-tables!
  "Create the main entity tables and lookup tables for the indexed attributes."
  [connection entity-schema entity-table-name ident-attr indexed-attrs]
  (create-entity-table! connection entity-table-name ident-attr)
  (when ident-attr
    (create-identity-index! connection entity-table-name ident-attr))
  (let [cit (partial create-index-table! connection entity-schema entity-table-name)]
    (doseq [idxattr indexed-attrs]
      (let [attrname (dbi/db-ident idxattr)]
        (cit attrname idxattr)))
    entity-table-name))

(defn- create-db-schema!
  "Create a new schema (a logical grouping of tables), if it does not already exist."
  [connection db-schema-name]
  (if (seq (jdbc/execute! connection [(dbi/create-schema-sql db-schema-name)]))
    db-schema-name
    (u/throw-ex (str "Failed to create schema - " db-schema-name))))

(defn- drop-db-schema! [connection db-schema-name]
  (if (seq (jdbc/execute! connection [(dbi/drop-schema-sql db-schema-name)]))
    db-schema-name
    (u/throw-ex (str "Failed to drop schema - " db-schema-name))))

(defn create-schema
  "Create the schema, tables and indexes for the component."
  [datasource component-name]
  (let [scmname (dbi/db-schema-for-component component-name)]
    (with-open [conn (jdbc/get-connection datasource)]
      (create-db-schema! conn scmname)
      (doseq [ename (cn/entity-names component-name)]
        (let [tabname (dbi/table-for-entity ename)
              schema (dbi/find-entity-schema ename)
              indexed-attrs (cn/indexed-attributes schema)]
          (create-tables! conn schema tabname :Id indexed-attrs))))
    component-name))

(defn drop-schema
  "Remove the schema from the database, perform a non-cascading delete."
  [datasource component-name]
  (let [scmname (dbi/db-schema-for-component component-name)]
    (with-open [conn (jdbc/get-connection datasource)]
      (drop-db-schema! conn scmname))
    component-name))

(defn- upsert-index-statement [conn table-name colname id attrval]
  (let [sql (str "MERGE INTO " table-name " KEY (ID) VALUES (?, ?)")
=======
(defn upsert-index-statement [conn table-name _ id attrval]
  (let [sql (str "INSERT INTO " table-name " VALUES (?, ?)")
>>>>>>> 4b96cde645b378a7bbac7b635d3672c4361b9a41
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [id attrval]]))

(defn upsert-inst-statement [conn table-name id obj]
  (let [sql (str "MERGE INTO " table-name " KEY (ID) VALUES (?, ? FORMAT JSON)")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [id obj]]))

(defn validate-ref-statement [conn index-tabname colname ref]
  (let [sql (str "SELECT 1 FROM " index-tabname " WHERE " colname " = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [ref]]))

(defn delete-index-statement [conn table-name _ id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [id]]))

(defn delete-inst-statement [conn table-name id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [id]]))

(defn do-query-statement [conn query-sql query-params]
  (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
    [pstmt query-params]))

(defn query-by-id-statement [conn query-sql id]
  (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
    (.setString pstmt 1 (str id))
    [pstmt nil]))

(defn transact! [datasource f]
  (with-open [conn (jdbc/get-connection datasource)]
      (jdbc/with-transaction [txn conn]
        (f txn))))

(defn execute-sql! [conn sql]
  (jdbc/execute! conn sql))

(defn execute-stmt! [_ stmt params]
  (if params
    (jdbc/execute! (jdbcp/set-parameters stmt params))
    (jdbc/execute! stmt)))