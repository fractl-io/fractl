(ns fractl.store.alasql-internal
  (:require [clojure.string :as str]
            [cljsjs.alasql]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.store.sql :as sql]
            [fractl.store.util :as su]))

;; Store for databases.
(defonce sql-db (atom nil))

(def db-new (atom {}))

(declare execute-sql!)

(defn create-db [name]
  (js/alasql (str "CREATE DATABASE IF NOT EXISTS " name))
  (let [db (. js/alasql Database name)]
    (js/alasql (str "USE " name))
    db))

(defn create-entity-table-sql
  "Given a database-type, entity-table-name and identity-attribute name,
  return the DML statement to create that table."
  [tabname ident-attr]
  [(str su/create-table-prefix " " tabname " "
        (if ident-attr
          (str "(" (su/db-ident ident-attr) " UUID, ")
          "(")
        "instance_json JSON)")])

(defn create-index-table-sql
  "Given a database-type, entity-table-name and attribute-column name, return the
  DML statements for creating an index table and the index for its 'id' column."
  [entity-table-name colname coltype unique?]
  (let [index-tabname (su/index-table-name entity-table-name colname)]
    [(str su/create-table-prefix " " index-tabname " "
          ;; `id` is not a foreign key reference to the main table,
          ;; because insert is fully controlled by the fractl runtime and
          ;; we get an index for free.
          "(id UUID, "
          ;; Storage and search can be optimized by inferring a more appropriate
          ;; SQL type for `colname`, see the issue https://ventur8.atlassian.net/browse/V8DML-117.
          colname " " coltype
          (if unique? (str ",UNIQUE(" colname "))") ")"))]
    [(su/create-index-sql index-tabname colname unique?)]))

(defn create-identity-index-sql [entity-table-name colname]
  (let [sql (str su/create-unique-index-prefix
                 " " (su/index-name entity-table-name)
                 " ON " entity-table-name "(" colname ")")]
    [sql]))

(defn create-identity-index!
  "Create identity index for an entity-table-name. This implementation
  is slightly different than h2 part as here rather than use alasql
  internal DB connection we hardcode SQL to bypass alasql's limitations."
  [db entity-table-name ident-attr]
  (let [sqls (create-identity-index-sql entity-table-name (su/db-ident ident-attr))]
    (execute-sql! db sqls)
    entity-table-name))

(defn create-entity-table!
  "Create a table to store instances of an entity. As 'identity-attribute' is
  specified to be used as the primary-key in the table."
  [db tabname ident-attr]
  (let [sqls (create-entity-table-sql tabname ident-attr)]
    (execute-sql! db sqls)
    #_(doseq [sql sqls]
      (if-not (js/alasql (str sql))
        (u/throw-ex (str "Failed to create table for " tabname))))
    tabname))

(defn- create-index-table! [db entity-schema entity-table-name attrname idxattr]
  (let [[tabsqls idxsqls] (create-index-table-sql
                           entity-table-name attrname
                           (sql/sql-index-type (cn/attribute-type entity-schema idxattr))
                           (cn/unique-attribute? entity-schema idxattr))]
    (execute-sql! db tabsqls)
    (execute-sql! db idxsqls)))

(defn create-tables!
  "Create the main entity tables and lookup tables for the indexed attributes."
  [db entity-schema entity-table-name ident-attr indexed-attrs]
  (create-entity-table! db entity-table-name ident-attr)
  (when ident-attr
    (create-identity-index! db entity-table-name ident-attr))
  (let [cit (partial create-index-table! db entity-schema entity-table-name)]
    (doseq [idxattr indexed-attrs]
      (let [attrname (su/db-ident idxattr)]
        (cit attrname idxattr)))
    entity-table-name))

(defn create-db-schema!
  "Create a new schema (a logical grouping of tables), if it does not already exist."
  [db db-schema-name]
  (if (execute-sql! db [(su/create-schema-sql db-schema-name)])
    db-schema-name
    (u/throw-ex (str "Failed to create schema - " db-schema-name))))

(defn- drop-db-schema! [db db-schema-name]
  (if (execute-sql! db [(su/drop-schema-sql db-schema-name)])
    db-schema-name
    (u/throw-ex (str "Failed to drop schema - " db-schema-name))))

(defn create-schema
  "Create the schema, tables and indexes for the component."
  [datasource component-name]
  (let [scmname (su/db-schema-for-component component-name)]
    (create-db-schema! datasource scmname)
    (doseq [ename (cn/entity-names component-name)]
      (let [tabname (su/table-for-entity ename)
            schema (su/find-entity-schema ename)
            indexed-attrs (cn/indexed-attributes schema)]
        (create-tables! datasource schema tabname :Id indexed-attrs)))
    component-name))

(defn drop-schema
  "Remove the schema from the database, perform a non-cascading delete."
  [datasource component-name]
  (let [scmname (su/db-schema-for-component component-name)]
    (drop-db-schema! datasource scmname)
    component-name))

(defn upsert-index-statement [conn table-name _ id attrval]
  (let [sql (str "INSERT INTO " table-name " VALUES (?, ?)")
        params #js [#js [id attrval]]]
    [sql params]))

(defn upsert-inst-statement [conn table-name id obj]
  (let [sql (str "INSERT OR REPLACE INTO " table-name " VALUES(?, ?)") 
        params #js [#js [id obj]]]
    [sql params]))

(defn delete-index-statement [conn table-name _ id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")]
    [sql #js [#js [id]]]))

(defn delete-inst-statement [conn table-name id]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")]
    [sql #js [#js [id]]]))

(defn- query-by-id-statement [db query-sql id]
  (.exec db query-sql #js [#js [id]]))

(defn query-by-id [datasource entity-name query-sql ids]
  (let [[id-key json-key] (su/make-result-keys entity-name)]
    ((partial su/results-as-instances entity-name id-key json-key)
     (flatten (map #(let [pstmt (query-by-id-statement datasource query-sql %)]
                      pstmt
                      (set ids)))))))

(defn do-query [datasource query-sql query-params]
  (.exec datasource query-sql #js [#js [query-params]]))

(def compile-to-indexed-query (partial sql/compile-to-indexed-query
                                       su/table-for-entity
                                       su/index-table-name))

(defn transact! [db f]
  (f db))

(defn execute-sql! [db sqls]
  (doseq [sql sqls]
    (when-not (.exec db sql)
      (u/throw-ex (str "Failed to execute sql statement - " sql))))
  true)

(defn execute-stmt! [db stmt params]
  (.exec db stmt params))
