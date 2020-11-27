(ns fractl.store.alasql-internal
  (:require [clojure.string :as str]
            ["alasql" :as alasql]
(ns fractl.store.sql-internal
  (:require ["alasql" :as alasql]
            [fractl.store.db-internal :as dbi]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.store.sql :as sql]
            [fractl.store.util :as su]))

;; Store for databases.
(defonce sql-db (atom nil))

(def db-new (atom {}))

(defn create-db [name]
  (alasql (str "CREATE DATABASE IF NOT EXISTS " name))
  (. alasql Database name))

(defn- create-entity-table-sql
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
  (println "HEREERERERE")
  (println entity-table-name)
  (println colname)
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
    (alasql (str "USE " fname))
    (alasql (str sql))
    (println sql)
    (println (alasql "SHOW DATABASES"))
    (println (alasql (str "SHOW TABLES FROM " fname)))
    entity-table-name
    #_(if (.exec other-db sql)
      entity-table-name
      (u/throw-ex (str "Failed to create index table for identity column - "
                       [entity-table-name ident-attr])))
    ))

(defn create-entity-table!
  "Create a table to store instances of an entity. As 'identity-attribute' is
  specified to be used as the primary-key in the table."
  [tabname ident-attr]
  (println "HERE:")
  (println tabname)
  (println ident-attr)
  (let [fname (first (str/split tabname #"\."))
        sql (create-entity-table-sql tabname ident-attr)
        db (. alasql Database fname)]
    (println "HERE is create-entity-table: " fname " and " sql)
    ;(.exec db sql)
    ;(println (.exec db "SHOW TABLES"))
    (if (.exec db sql)
      (do
        tabname
        (println "TABNAME IS: " tabname)
        (println (.exec db "SHOW TABLES")))
      (u/throw-ex (str "Failed to create table for " tabname)))))

(defn- create-index-table! [entity-schema entity-table-name attrname idxattr]
  (let [[tabsql idxsql] (create-index-table-sql
                          entity-table-name attrname
                          (sql/sql-index-type (cn/attribute-type entity-schema idxattr))
                          (cn/unique-attribute? entity-schema idxattr))
        ;db (. alasql Database)
        ]
    (println "HERE IS tabsql " tabsql)
    (println " HERE IS idxsql " idxsql)
    ;; Might be a problem on further tests... Need to investigate.
    (when-not (and (alasql tabsql)
                   (alasql idxsql))
      (u/throw-ex (str "Failed to create lookup table for " [entity-table-name attrname])))))

(defn create-tables!
  "Create the main entity tables and lookup tables for the indexed attributes."
  [db entity-schema entity-table-name ident-attr indexed-attrs]
  (println "PART OF CREATE TABLES!")
  (println entity-schema)
  (println entity-table-name)
  (println ident-attr)
  (println indexed-attrs)
  (create-entity-table! entity-table-name ident-attr)
  (when ident-attr
    (create-identity-index! entity-table-name ident-attr))
  #_(let [cit (partial create-index-table! entity-schema entity-table-name)]
    (println cit)
    (doseq [idxattr indexed-attrs]
      (let [attrname (dbi/db-ident idxattr)]
        (cit attrname idxattr)))
    entity-table-name)
  )

(defn create-db-schema!
  "Create a new schema (a logical grouping of tables), if it does not already exist."
  [db db-schema-name]
  ;(let [db (. alasql Database)])
  (if (.exec db (dbi/create-schema-sql db-schema-name))
    db-schema-name
    (u/throw-ex (str "Failed to create schema - " db-schema-name))))

(defn- drop-db-schema! [db db-schema-name]
  ;(let [db (. alasql Database)])
  (if (.exec db (dbi/drop-schema-sql db-schema-name))
    db-schema-name
    (u/throw-ex (str "Failed to drop schema - " db-schema-name))))

(defn create-schema
  "Create the schema, tables and indexes for the component."
  [datasource component-name]
  (.log js/console datasource)
  (let [scmname (dbi/db-schema-for-component component-name)]
    (create-db-schema! datasource scmname)
    (doseq [ename (cn/entity-names component-name)]
      (let [tabname (dbi/table-for-entity ename)
            schema (cn/entity-schema ename)
            indexed-attrs (dbi/find-indexed-attributes ename schema)]
        (println ename)
        (println schema)
        (create-tables! datasource schema tabname :Id indexed-attrs)))
    component-name))

(defn drop-schema
  "Remove the schema from the database, perform a non-cascading delete."
  [datasource component-name]
  (let [scmname (dbi/db-schema-for-component component-name)]
    (drop-db-schema! datasource scmname)
    component-name))
