(ns fractl.store.h2-internal
  (:require [next.jdbc :as jdbc]
            [next.jdbc.prepare :as jdbcp]
            [cheshire.core :as json]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.store.util :as su]
            [fractl.store.sql :as sql]
            [fractl.store.db-internal :as dbi])
  (:import [java.sql PreparedStatement]))

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
          "(id UUID, "
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
              schema (cn/entity-schema ename)
              indexed-attrs (dbi/find-indexed-attributes ename schema)]
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
  (let [sql (str "INSERT INTO " table-name " VALUES (?, ?)")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    (jdbcp/set-parameters pstmt [id attrval])
    pstmt))

(defn- upsert-indices!
  "Insert or update new index entries relevant for an entity instance.
  The index values are available in the `attrs` parameter."
  [conn entity-table-name indexed-attrs instance]
  (let [id (:Id instance)]
    (doseq [[attrname tabname] (dbi/index-table-names entity-table-name indexed-attrs)]
      (let [pstmt (upsert-index-statement conn tabname (dbi/db-ident attrname)
                                          id (attrname instance))]
        (jdbc/execute! pstmt)))))

(defn- upsert-inst-statement [conn table-name id obj]
  (let [sql (str "MERGE INTO " table-name " KEY (ID) VALUES (?, ? FORMAT JSON)")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    (jdbcp/set-parameters pstmt [id obj])
    pstmt))

(defn- upsert-inst!
  "Insert or update an entity instance."
  [conn table-name inst]
  (let [attrs (cn/serializable-attributes inst)
        id (:Id attrs)
        obj (json/generate-string (dissoc attrs :Id))
        pstmt (upsert-inst-statement conn table-name id obj)]
    (jdbc/execute! pstmt)))

(defn upsert-instance [datasource entity-name instance]
  (let [tabname (dbi/table-for-entity entity-name)
        indexed-attrs (dbi/find-indexed-attributes entity-name)]
    (with-open [conn (jdbc/get-connection datasource)]
      (jdbc/with-transaction [txn conn]
        (upsert-inst! txn tabname instance)
        (upsert-indices! txn tabname indexed-attrs instance)))
    instance))

(defn- delete-index-statement [conn table-name colname id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    (jdbcp/set-parameters pstmt [id])
    pstmt))

(defn- delete-indices!
  "Delete index entries relevant for an entity instance."
  [conn entity-table-name indexed-attrs id]
  (let [index-tabnames (dbi/index-table-names entity-table-name indexed-attrs)]
    (doseq [[attrname tabname] index-tabnames]
      (let [pstmt (delete-index-statement
                   conn tabname
                   (dbi/db-ident attrname) id)]
        (jdbc/execute! pstmt)))))

(defn- delete-inst-statement [conn table-name id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    (jdbcp/set-parameters pstmt [id])
    pstmt))

(defn- delete-inst!
  "Delete an entity instance."
  [conn tabname id]
  (let [pstmt (delete-inst-statement conn tabname id)]
    (jdbc/execute! pstmt)))

(defn delete-instance [datasource entity-name instance]
  (let [id (:Id instance)
        tabname (dbi/table-for-entity entity-name)
        indexed-attrs (dbi/find-indexed-attributes entity-name)]
    (with-open [conn (jdbc/get-connection datasource)]
      (jdbc/with-transaction [txn conn]
        (delete-indices! txn tabname indexed-attrs id)
        (delete-inst! txn tabname id)))
    id))

(defn- query-by-id-statement [conn query-sql id]
  (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
    (.setString pstmt 1 (str id))
    pstmt))

(defn query-by-id [datasource entity-name query-sql ids]
  (with-open [conn (jdbc/get-connection datasource)]
    (let [[id-key json-key] (su/make-result-keys entity-name)]
      ((partial su/results-as-instances entity-name id-key json-key)
       (flatten (map #(let [pstmt (query-by-id-statement conn query-sql %)]
                        (jdbc/execute! pstmt))
                     (set ids)))))))

(defn do-query [datasource query-sql query-params]
  (with-open [conn (jdbc/get-connection datasource)]
    (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
      (jdbcp/set-parameters pstmt query-params)
      (jdbc/execute! pstmt))))

(def compile-to-indexed-query (partial sql/compile-to-indexed-query
                                       dbi/table-for-entity
                                       dbi/index-table-name))
