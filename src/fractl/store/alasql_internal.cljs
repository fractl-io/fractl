(ns fractl.store.sql-internal
  (:require ["alasql" :as alasql]
            [fractl.store.db-internal :as dbi]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.store.sql :as sql]
            [clojure.string :as str]))

;; Store for databases.
(defonce sql-db (atom nil))

(def db-new (atom {}))

(defn- create-entity-table-sql
  "Given a database-type, entity-table-name and identity-attribute name,
  return the DML statement to create that table."
  [tabname ident-attr]
  (str dbi/create-table-prefix " " (second (str/split tabname #"\.")) " "
       (if ident-attr
         (str "(" (dbi/db-ident ident-attr) " UUID, ")
         "(")
       "instance_json CLOB)"))

(defn- create-index-table-sql
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
       " ON " (second (str/split entity-table-name #"\.")) "(" colname ")"))

(defn create-identity-index! [entity-table-name ident-attr]
  (let [fname (first (str/split entity-table-name #"\."))
        sql (create-identity-index-sql entity-table-name (dbi/db-ident ident-attr))
        db (. alasql Database fname)]
    ;(println (.exec db (str "SHOW TABLES from " fname)))
    ;(println fname)
    ;(println sql)
    (if (.exec db sql)
      entity-table-name
      (u/throw-ex (str "Failed to create index table for identity column - "
                       [entity-table-name ident-attr])))))

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
    (println "HERE'S JOHNNNY: " sql)
    (println "HERE's the DB name" fname)
    (if (.exec db (str sql))
      tabname
      (u/throw-ex (str "Failed to create table for " tabname)))))

(defn- create-index-table! [entity-schema entity-table-name attrname idxattr]
  (let [[tabsql idxsql] (create-index-table-sql
                          entity-table-name attrname
                          (sql/sql-index-type (cn/attribute-type entity-schema idxattr))
                          (cn/unique-attribute? entity-schema idxattr))
        ;db (. alasql Database)
        ]
    (when-not (and (alasql tabsql)
                   (alasql idxsql))
      (u/throw-ex (str "Failed to create lookup table for " [entity-table-name attrname])))))

(defn create-tables!
  "Create the main entity tables and lookup tables for the indexed attributes."
  [entity-schema entity-table-name ident-attr indexed-attrs]
  (println "PART OF CREATE TABLES!")
  (println entity-schema)
  (println entity-table-name)
  (println ident-attr)
  (println indexed-attrs)
  (create-entity-table! entity-table-name ident-attr)
  (println (create-entity-table! entity-table-name ident-attr))
  (when ident-attr
    (create-identity-index! entity-table-name ident-attr))
  (let [cit (partial create-index-table! entity-schema entity-table-name)]
    (println cit)
    (doseq [idxattr indexed-attrs]
      (let [attrname (dbi/db-ident idxattr)]
        (cit attrname idxattr)))
    entity-table-name))

(defn create-db-schema!
  "Create a new schema (a logical grouping of tables), if it does not already exist."
  [db-schema-name]
  ;(let [db (. alasql Database)])
  (if (alasql (dbi/create-schema-sql db-schema-name))
    db-schema-name
    (u/throw-ex (str "Failed to create schema - " db-schema-name))))

(defn- drop-db-schema! [db-schema-name]
  ;(let [db (. alasql Database)])
  (if (alasql (dbi/drop-schema-sql db-schema-name))
    db-schema-name
    (u/throw-ex (str "Failed to drop schema - " db-schema-name))))

(defn create-schema
  "Create the schema, tables and indexes for the component."
  [component-name]
  (let [scmname (dbi/db-schema-for-component component-name)
        ename (cn/entity-names component-name)]
    (create-db-schema! scmname)
    (doseq [vename (vec ename)]
      (let [tabname (dbi/table-for-entity vename)
            schema (cn/entity-schema vename)
            indexed-attrs (dbi/find-indexed-attributes vename schema)]
        (println vename)
        (println schema)
        (create-tables! schema tabname :Id indexed-attrs)))
    component-name))

(defn drop-schema
  "Remove the schema from the database, perform a non-cascading delete."
  [component-name]
  (let [scmname (dbi/db-schema-for-component component-name)]
    (drop-db-schema! scmname)
    component-name))