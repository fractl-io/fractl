(ns fractl.store.util
  (:require #?(:clj [cheshire.core :as json])
            [clojure.set :as set]
            [clojure.string :as s]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.util :as u]))

(defn db-ident [k]
  (if (keyword? k)
    (name k)
    k))

(defn db-schema-for-component [component-name]
  (s/replace (name component-name) #"\." "_"))

(defn table-for-entity [entity-name]
  (let [[component-name r] (li/split-path entity-name)]
    (if (cn/entity-schema-predefined? entity-name)
      (db-ident r)
      (str (db-schema-for-component component-name) "__" (db-ident r)))))

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
  (s/replace (str tabname "_idx") #"\." "_"))

(defn index-table-names
  "Given an entity table-name and its indexed attributes, return a sequence of
  all index table names."
  [entity-table-name indexed-attrs]
  (let [tabnames (map #(index-table-name entity-table-name %) indexed-attrs)]
    (into {} (map vector indexed-attrs tabnames))))

(def create-table-prefix "CREATE TABLE IF NOT EXISTS")
#?(:clj 
   (def create-unique-index-prefix "CREATE UNIQUE INDEX IF NOT EXISTS")
   :cljs
   (def create-unique-index-prefix "CREATE UNIQUE INDEX"))
#?(:clj
   (def create-index-prefix "CREATE INDEX IF NOT EXISTS")
   :cljs
   (def create-index-prefix "CREATE INDEX"))

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

(defn table-name->entity
  [tabname] 
  (let [tabnamestr (name tabname)
        [cnstr estr] (s/split tabnamestr #"__")]
    [(keyword (s/replace cnstr #"_" ".")) (keyword estr)]))

(defn- table-attr->entity-attr
  [table-attr]
  (let [[cne attr] (li/split-path table-attr)
        [cn e] (table-name->entity cne)]
    (keyword (s/upper-case (str (name cn) "." (name e) "/" (name attr))))))

(defn normalize-connection-info [connection-info]
  (if-let [f (:decrypt-fn connection-info)]
    (let [pswd (f (:password connection-info))]
      (assoc connection-info :password pswd))
    connection-info))

(defn- normalize-result
  [result]
  (let [attrs (keys result)
        attrmap (apply assoc {} (interleave attrs (map table-attr->entity-attr attrs)))]
    (set/rename-keys result attrmap)))

(defn result-as-instance
  ([entity-name id-key json-key result]
   (if id-key
     (let [nresult #?(:clj (normalize-result result)
                      :cljs result)
           id (id-key nresult)
           json-str #?(:clj  (String. (json-key nresult))
                       :cljs (json-key nresult))
           parsed-obj (assoc #?(:clj (json/parse-string json-str true)
                                :cljs (js->clj (.parse js/JSON json-str) :keywordize-keys true))
                             :Id (str id))]
       (cn/make-instance entity-name parsed-obj))
     (let [attr-names (keys (cn/fetch-schema entity-name))]
       (loop [result-keys (keys result), obj {}]
         (if-let [rk (first result-keys)]
           (let [[_ f] (li/split-path rk)
                 aname (first
                        (filter
                         #(= (s/upper-case (name %)) (s/upper-case (name f)))
                         attr-names))]
             (if aname
               (recur (rest result-keys) (assoc obj aname (get result rk)))
               (u/throw-ex (str "cannot map " rk " to an attribute in " entity-name))))
           (cn/make-instance entity-name obj))))))
  ([entity-name result]
   (result-as-instance entity-name nil nil result)))

(defn make-result-keys [entity-name]
  #?(:clj
     (let [entity-name (li/split-path entity-name)
           cn (s/upper-case (name (first entity-name)))
           e (s/upper-case (name (second entity-name)))]
       [(keyword (str cn "." e "/ID"))
        (keyword (str cn "." e "/INSTANCE_JSON"))])
     :cljs
     [:Id :instance_json]))

(defn results-as-instances
  ([entity-name id-key json-key results]
   (mapv (partial result-as-instance entity-name id-key json-key) results))
  ([entity-name results]
   (let [[id-key json-key] (when-not (cn/relational-schema?)
                             (make-result-keys entity-name))]
     (results-as-instances entity-name id-key json-key results))))

(defn clj->json
  [data]
  #?(:clj (json/generate-string data)
     :cljs (.stringify js/JSON (clj->js data))))
