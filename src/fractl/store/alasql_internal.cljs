(ns fractl.store.alasql-internal
  (:require [clojure.string :as str]
            [cljsjs.alasql]
            [fractl.store.db-internal :as dbi]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.store.sql :as sql]
            [fractl.store.util :as su]))

;; Store for databases.
(defonce sql-db (atom nil))

(def db-new (atom {}))

(defn create-db [name]
  (js/alasql (str "CREATE DATABASE IF NOT EXISTS " name))
  (. js/alasql Database name))

(defn create-entity-table-sql
  "Given a database-type, entity-table-name and identity-attribute name,
  return the DML statement to create that table."
  [tabname ident-attr]
  (str dbi/create-table-prefix " " (second (str/split tabname #"\.")) " "
       (if ident-attr
         (str "(" (dbi/db-ident ident-attr) " UUID, ")
         "(")
       "instance_json JSON)"))

(defn create-index-table-sql
  "Given a database-type, entity-table-name and attribute-column name, return the
  DML statements for creating an index table and the index for its 'id' column."
  [entity-table-name colname coltype unique?]
  (let [index-tabname (dbi/index-table-name entity-table-name colname)]
    (str dbi/create-table-prefix " " index-tabname " "
            ;; `id` is not a foreign key reference to the main table,
            ;; because insert is fully controlled by the fractl runtime and
            ;; we get an index for free.
            "(id UUID, "
            ;; Storage and search can be optimized by inferring a more appropriate
            ;; SQL type for `colname`, see the issue https://ventur8.atlassian.net/browse/V8DML-117.
            colname " " coltype
            (if unique? (str ",UNIQUE(" colname "))") ")"))
    (dbi/create-index-sql index-tabname colname unique?)))

(defn create-identity-index-sql [entity-table-name colname]
  (str dbi/create-unique-index-prefix
       " " (dbi/index-name entity-table-name)
       " ON "
       (second (str/split entity-table-name #"\."))
       ;entity-table-name
       "(" colname ")"))

(defn create-identity-index! [entity-table-name ident-attr]
  "Create identity index for an entity-table-name. This implementation
  is slightly different than h2 part as here rather than use alasql
  internal DB connection we hardcode SQL to bypass alasql's limitations."
  (let [fname (first (str/split entity-table-name #"\."))
        sql (create-identity-index-sql entity-table-name (dbi/db-ident ident-attr))]
    (js/alasql (str "USE " fname))
    (js/alasql (str sql))
    entity-table-name))

(defn create-entity-table!
  "Create a table to store instances of an entity. As 'identity-attribute' is
  specified to be used as the primary-key in the table."
  [tabname ident-attr]
  (let [fname (first (str/split tabname #"\."))
        sql (create-entity-table-sql tabname ident-attr)
        db (js/alasql (str "USE " fname))]
    (if (js/alasql (str sql))
      tabname
      (u/throw-ex (str "Failed to create table for " tabname)))))

(defn- create-index-table! [db entity-schema entity-table-name attrname idxattr]
  (let [[tabsql idxsql] (create-index-table-sql
                          entity-table-name attrname
                          (sql/sql-index-type (cn/attribute-type entity-schema idxattr))
                          (cn/unique-attribute? entity-schema idxattr))]
    (when-not (and (.exec db tabsql)
                   (.exec db idxsql))
      (u/throw-ex (str "Failed to create lookup table for " [entity-table-name attrname])))))

(defn create-tables!
  "Create the main entity tables and lookup tables for the indexed attributes."
  [db entity-schema entity-table-name ident-attr indexed-attrs]
  (create-entity-table! entity-table-name ident-attr)
  (when ident-attr
    (create-identity-index! entity-table-name ident-attr))
  (let [cit (partial create-index-table! db entity-schema entity-table-name)]
    (doseq [idxattr indexed-attrs]
      (let [attrname (dbi/db-ident idxattr)]
        (cit attrname idxattr)))
    entity-table-name))

(defn create-db-schema!
  "Create a new schema (a logical grouping of tables), if it does not already exist."
  [db db-schema-name]
  (if (.exec db (dbi/create-schema-sql db-schema-name))
    db-schema-name
    (u/throw-ex (str "Failed to create schema - " db-schema-name))))

(defn- drop-db-schema! [db db-schema-name]
  (if (.exec db (dbi/drop-schema-sql db-schema-name))
    db-schema-name
    (u/throw-ex (str "Failed to drop schema - " db-schema-name))))

(defn create-schema
  "Create the schema, tables and indexes for the component."
  [datasource component-name]
  (let [scmname (dbi/db-schema-for-component component-name)]
    (create-db-schema! datasource scmname)
    (doseq [ename (cn/entity-names component-name)]
      (let [tabname (dbi/table-for-entity ename)
            schema (dbi/find-entity-schema ename)
            indexed-attrs (cn/indexed-attributes schema)]
        (create-tables! datasource schema tabname :Id indexed-attrs)))
    component-name))

(defn drop-schema
  "Remove the schema from the database, perform a non-cascading delete."
  [datasource component-name]
  (let [scmname (dbi/db-schema-for-component component-name)]
    (drop-db-schema! datasource scmname)
    component-name))

(defn upsert-index-statement [conn table-name _ id attrval]
  (let [sql (str "INSERT INTO " table-name " VALUES (?, ?)")]
    sql))

(defn- upsert-indices!
  "Insert or update new index entries relevant for an entity instance.
  The index values are available in the `attrs` parameter."
  [db entity-table-name indexed-attrs instance]
  (let [id (:Id instance)]
    (doseq [[attrname tabname] (dbi/index-table-names entity-table-name indexed-attrs)]
      (let [pstmt (upsert-index-statement tabname)]
        (.exec db pstmt #js [#js [id (attrname instance)]])))))

(defn upsert-inst-statement [conn table-name id obj]
  (.log js/console (str "upsert-inst-statement - table-name: " table-name))
  (let [sql (str "INSERT OR REPLACE INTO " table-name " VALUES(?, ?)") 
        params #js [#js [id obj]]]
    (.log js/console sql)
    (.log js/console params)
    [sql params]))

(defn transact! [db f]
  (f db))

(defn execute-sql! [db sql]
  (.exec db sql))

(defn execute-stmt! [db stmt params]
  (.exec db stmt params))

(defn- upsert-inst!
  "Insert or update an entity instance."
  [db table-name inst]
  (.log js/console "alasql_internal - upsert-inst! - table-name: " table-name)
  (let [attrs (cn/serializable-attributes inst)
        id (:Id attrs)
        obj (.stringify js/JSON (clj->js (dissoc attrs :Id)))
        [sql params] (upsert-inst-statement table-name id obj)]
    (execute-stmt! db sql params)))

(defn upsert-instance [datasource entity-name instance]
  (let [tabname (dbi/table-for-entity entity-name)
        entity-schema (dbi/find-entity-schema entity-name)
        indexed-attrs (cn/indexed-attributes entity-schema)]
    (upsert-inst! datasource tabname instance)
    (upsert-indices! datasource tabname indexed-attrs instance)
    instance))

(defn delete-index-statement [table-name]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")]
    sql))

(defn- delete-indices!
  "Delete index entries relevant for an entity instance."
  [db entity-table-name indexed-attrs id]
  (let [index-tabnames (dbi/index-table-names entity-table-name indexed-attrs)]
    (doseq [[attrname tabname] index-tabnames]
      (let [pstmt (delete-index-statement tabname)]
        (.exec db pstmt #js [#js [id]])))))

(defn delete-inst-statement [table-name]
  (let [sql (str "DELETE FROM " table-name " WHERE id = ?")]
    sql))

(defn- delete-inst!
  "Delete an entity instance."
  [db tabname id]
  (let [pstmt (delete-inst-statement tabname)]
    (.exec db pstmt #js [#js [id]])))

(defn delete-instance [datasource entity-name instance]
  (let [id (:Id instance)
        tabname (dbi/table-for-entity entity-name)
        entity-schema (dbi/find-entity-schema entity-name)
        indexed-attrs (cn/indexed-attributes entity-schema)]
    (delete-indices! datasource tabname indexed-attrs id)
    (delete-inst! datasource tabname id)
    id))

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
                                       dbi/table-for-entity
                                       dbi/index-table-name))
