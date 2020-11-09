(ns fractl.store.h2-internal
  (:require [clojure.string :as string]
            [next.jdbc :as jdbc]
            [next.jdbc.prepare :as jdbcp]
            [cheshire.core :as json]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.component :as cn]
            [fractl.store.sql :as sql])
  (:import [java.sql PreparedStatement]))

(defn- db-ident [k]
  (if (keyword? k)
    (string/lower-case (name k))
    k))

(defn- db-schema-for-model [model-name]
  (string/lower-case (string/replace (name model-name) #"\." "_")))

(defn- table-for-entity
  ([entity-name db-schema-name]
   (let [[model-name r] (li/split-path entity-name)
         scmname (or db-schema-name (db-schema-for-model model-name))]
     (str scmname "." (db-ident r))))
  ([entity-name] (table-for-entity entity-name nil)))

(defn- indexed-attributes [entity-schema]
  (set (remove #{:Id} (cn/indexed-attributes entity-schema))))

(defn- find-indexed-attributes
  ([entity-name entity-schema]
   (if entity-schema
     (indexed-attributes entity-schema)
     (u/throw-ex (str "no schema for " entity-name))))
  ([entity-name]
   (find-indexed-attributes entity-name (cn/entity-schema entity-name))))

(defn- index-table-name
  "Construct the lookup table-name for the attribute, from the main entity
  table-name and attribute-name."
  [tabname attrname]
  (let [attrname (db-ident attrname)]
    (str tabname "_" attrname)))

(defn index-name
  "Given a table-name, return its relative index table name."
  [tabname]
  (string/replace (str tabname "_idx") #"\." "_"))

(defn index-table-names
  "Given an entity table-name and its indexed attributes, return a sequence of
  all index table names."
  [entity-table-name indexed-attrs]
  (let [tabnames (map #(index-table-name entity-table-name %) indexed-attrs)]
    (into {} (map vector indexed-attrs tabnames))))

(def ^:private create-table-prefix "CREATE TABLE IF NOT EXISTS")
(def ^:private create-index-prefix "CREATE INDEX IF NOT EXISTS")
(def ^:private create-unique-index-prefix "CREATE UNIQUE INDEX IF NOT EXISTS")

(defn- create-index-sql
  "Given an entity-table-name and an attribute-column-name, return the
  CREATE INDEX sql statement for that attribute."
  [entity-table-name colname unique?]
  (let [tabname (index-table-name entity-table-name colname)]
    (str (if unique? create-unique-index-prefix create-index-prefix)
         " " (index-name tabname) " ON " tabname "(" colname ")")))

(defn- create-entity-table-sql
  "Given a database-type, entity-table-name and identity-attribute name,
  return the DML statement to create that table."
  [tabname ident-attr]
  (str create-table-prefix " " tabname " "
       (if ident-attr
         (str "(" (db-ident ident-attr) " UUID, ")
         "(")
       "instance_json CLOB)"))

(defn- create-index-table-sql
  "Given a database-type, entity-table-name and attribute-column name, return the
  DML statements for creating an index table and the index for its 'id' column."
  [entity-table-name colname unique?]
  (let [index-tabname (index-table-name entity-table-name colname)]
    [(str create-table-prefix " " index-tabname " "
          ;; `id` is not a foreign key reference to the main table,
          ;; because insert is fully controlled by the V8 runtime and
          ;; we get an index for free.
          "(id UUID, "
          ;; Storage and search can be optimized by inferring a more appropriate
          ;; SQL type for `colname`, see the issue https://ventur8.atlassian.net/browse/V8DML-117.
          colname " CLOB"
          (if unique? (str ",UNIQUE(" colname "))") ")"))
     (create-index-sql index-tabname "id" true)]))

(defn- create-identity-index-sql [entity-table-name colname]
  (str create-unique-index-prefix
       " " (index-name entity-table-name)
       " ON " entity-table-name "(" colname ")"))

(defn- create-identity-index! [connection entity-table-name ident-attr]
  (let [sql (create-identity-index-sql entity-table-name (db-ident ident-attr))]
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
      (let [attrname (db-ident idxattr)]
        (cit attrname idxattr)
        (when-not (jdbc/execute! connection [(create-index-sql entity-table-name attrname)])
          (u/throw-ex (str "Failed to create index for " [entity-table-name attrname])))))
    entity-table-name))

(defn- create-schema-sql [schema-name]
  (str "CREATE SCHEMA IF NOT EXISTS " schema-name))

(defn- drop-schema-sql [schema-name]
  (str "DROP SCHEMA IF EXISTS " schema-name))

(defn- create-db-schema!
  "Create a new schema (a logical grouping of tables), if it does not already exist."
  [connection db-schema-name]
  (if (seq (jdbc/execute! connection [(create-schema-sql db-schema-name)]))
    db-schema-name
    (u/throw-ex (str "Failed to create schema - " db-schema-name))))

(defn- drop-db-schema! [connection db-schema-name]
  (if (seq (jdbc/execute! connection [(drop-schema-sql db-schema-name)]))
    db-schema-name
    (u/throw-ex (str "Failed to drop schema - " db-schema-name))))

(defn create-schema
  "Create the schema, tables and indexes for the model."
  [datasource model-name]
  (let [scmname (db-schema-for-model model-name)]
    (with-open [conn (jdbc/get-connection datasource)]
      (create-db-schema! conn scmname)
      (doseq [ename (cn/entity-names model-name)]
        (let [tabname (table-for-entity ename)
              schema (cn/entity-schema ename)
              indexed-attrs (find-indexed-attributes ename schema)]
          (create-tables! conn schema tabname :Id indexed-attrs))))
    model-name))

(defn drop-schema
  "Remove the schema from the database, perform a non-cascading delete."
  [datasource model-name]
  (let [scmname (db-schema-for-model model-name)]
    (with-open [conn (jdbc/get-connection datasource)]
      (drop-db-schema! conn scmname))
    model-name))

(defn- upsert-index-statement [conn table-name colname id attrval]
  (let [sql (str "MERGE INTO " table-name " KEY (id) VALUES (?, ?)")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    (jdbcp/set-parameters pstmt [id attrval])
    pstmt))

(defn- upsert-indices!
  "Insert or update new index entries relevant for an entity instance.
  The index values are available in the `attrs` parameter."
  [conn entity-table-name indexed-attrs id]
  (let [index-tabnames (index-table-names entity-table-name indexed-attrs)]
    (doseq [[attrname tabname] index-tabnames]
      (let [pstmt (upsert-index-statement conn tabname
                                          (db-ident attrname)
                                          id (attrname indexed-attrs))]
        (jdbc/execute! pstmt)))))

(defn- upsert-inst-statement [conn table-name id obj]
  (let [sql (str "MERGE INTO " table-name " KEY (ID) VALUES (?, ?)")
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
  (let [tabname (table-for-entity entity-name)
        indexed-attrs (find-indexed-attributes entity-name)]
    (with-open [conn (jdbc/get-connection datasource)]
      (jdbc/with-transaction [txn conn]
        (upsert-inst! txn tabname instance)
        (upsert-indices! txn tabname indexed-attrs (:Id instance))))
    instance))

(defn- delete-index-statement [conn table-name colname id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    (jdbcp/set-parameters pstmt [id])
    pstmt))

(defn- delete-indices!
  "Delete index entries relevant for an entity instance."
  [conn entity-table-name indexed-attrs id]
  (let [index-tabnames (index-table-names entity-table-name indexed-attrs)]
    (doseq [[attrname tabname] index-tabnames]
      (let [pstmt (delete-index-statement
                   conn tabname
                   (db-ident attrname) id)]
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
        tabname (table-for-entity entity-name)
        indexed-attrs (find-indexed-attributes entity-name)]
    (with-open [conn (jdbc/get-connection datasource)]
      (jdbc/with-transaction [txn conn]
        (delete-indices! txn tabname indexed-attrs id)
        (delete-inst! txn tabname id)))
    id))

(defn- query-by-id-statement [conn query-sql ids]
  (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
    (.setObject pstmt ids)
    pstmt))

(defn query-by-id [datasource query-sql ids]
  (with-open [conn (jdbc/get-connection datasource)]
    (let [pstmt (query-by-id-statement conn query-sql ids)]
      (jdbc/execute-one! conn pstmt))))

(defn do-query [datasource query-sql query-params]
  (with-open [conn (jdbc/get-connection datasource)]
    (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
      (jdbcp/set-parameters pstmt query-params)
      (jdbc/execute-one! conn pstmt))))

(def compile-to-indexed-query (partial sql/compile-to-indexed-query
                                       table-for-entity
                                       index-table-name))
