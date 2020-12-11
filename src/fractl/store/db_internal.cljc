(ns fractl.store.db-internal
  (:require [clojure.string :as string]
            [fractl.lang.internal :as li]
            [fractl.component :as cn]
            [fractl.util :as u]))

(defn db-ident [k]
  (if (keyword? k)
    (string/lower-case (name k))
    k))

(defn db-schema-for-component [component-name]
  (string/lower-case (string/replace (name component-name) #"\." "_")))

(defn table-for-entity
  ([entity-name db-schema-name]
   (let [[component-name r] (li/split-path entity-name)
         scmname (or db-schema-name (db-schema-for-component component-name))]
     (str scmname "." (db-ident r))))
  ([entity-name] (table-for-entity entity-name nil)))

(defn indexed-attributes [entity-schema]
  (set (remove #{:Id} (cn/indexed-attributes entity-schema))))

(defn index-table-name
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

(def create-table-prefix "CREATE TABLE IF NOT EXISTS")
(def create-index-prefix "CREATE INDEX IF NOT EXISTS")
(def create-unique-index-prefix "CREATE UNIQUE INDEX")

(defn create-index-sql
  "Given a table-name and an attribute-column-name, return the
  CREATE INDEX sql statement for that attribute."
  [table-name colname unique?]
  (str (if unique? create-unique-index-prefix create-index-prefix)
       " " (index-name table-name) " ON " table-name "(" colname ")"))

(defn create-schema-sql [schema-name]
  (str "CREATE SCHEMA IF NOT EXISTS " schema-name))

(defn drop-schema-sql [schema-name]
  (str "DROP SCHEMA IF EXISTS " schema-name))

(defn find-entity-schema [entity-name]
  (if-let [scm (cn/entity-schema entity-name)]
    scm
    (u/throw-ex (str "schema not found for entity - " entity-name))))
