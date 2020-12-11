(ns fractl.store.util
  (:require #?(:clj [cheshire.core :as json])
            [clojure.set :as s]
            [clojure.string :as str]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.util :as fu]))

(defn norm-sql-statement
  [sql]
  (clojure.string/lower-case sql))

(defn db-ident [k]
  (if (keyword? k)
    (str/lower-case (name k))
    k))

(defn db-schema-for-component [component-name]
  (str/lower-case (str/replace (name component-name) #"\." "_")))

(defn table-for-entity
  ([entity-name db-schema-name]
   (let [[component-name r] (li/split-path entity-name)
         scmname (or db-schema-name (db-schema-for-component component-name))]
     (str scmname "__" (db-ident r))))
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
  (str/replace (str tabname "_idx") #"\." "_"))

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
    (fu/throw-ex (str "schema not found for entity - " entity-name))))

(defn table-name->entity
  [tabname] 
   (let [tabnamestr (name tabname)
         [cnstr estr] (str/split tabnamestr #"__")]
     [(keyword cnstr) (keyword estr)]))

(defn- table-attr->entity-attr
  [table-attr]
  (let [[cne attr] (li/split-path table-attr)
        [cn e] (table-name->entity cne)]
    (keyword (str (name cn) "." (name e) "/" (name attr)))))

(defn normalize-connection-info [connection-info]
  (if-let [f (:decrypt-fn connection-info)]
    (let [pswd (f (:password connection-info))]
      (assoc connection-info :password pswd))
    connection-info))

(defn- normalize-result
  [result]
  (let [attrs (keys result)
        attrmap (apply assoc {} (interleave attrs (map table-attr->entity-attr attrs)))]
    (s/rename-keys result attrmap)))

(defn result-as-instance [entity-name id-key json-key result]
  (let [nresult (normalize-result result)
        id (id-key nresult)
        json-str #?(:clj  (String. (json-key nresult))
                    :cljs (str (json-key nresult)))
        parsed-obj (assoc #?(:clj (json/parse-string json-str true)
                             :cljs (cljs.reader/read-string json-str))
                          :Id (str id))]
    (cn/make-instance entity-name parsed-obj)))

(defn results-as-instances [entity-name id-key json-key results]
  (doall (map (partial result-as-instance entity-name id-key json-key) results)))

(defn make-result-keys [entity-name]
  (let [cn (name (first entity-name))
        e (name (second entity-name))]
    [(keyword (str cn "." e "/ID")) (keyword (str cn "." e "/INSTANCE_JSON"))]))

(defn clj->json
  [data]
  #?(:clj (json/generate-string data)
     :cljs (.stringify js/JSON (clj->js data))))
