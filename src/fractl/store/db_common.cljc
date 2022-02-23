(ns fractl.store.db-common
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.walk :as w]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.store.util :as su]
            [fractl.store.sql :as sql]
            #?(:clj [fractl.store.jdbc-internal :as ji])
            #?(:clj [fractl.store.postgres-internal :as pi])
            #?(:clj [fractl.store.h2-internal :as h2i]
               :cljs [fractl.store.alasql-internal :as aqi])))

(def ^:private store-fns
  {:transact-fn! #?(:clj ji/transact-fn! :cljs aqi/execute-fn!)
   :execute-fn! #?(:clj ji/execute-fn! :cljs aqi/execute-fn!)
   :execute-sql! #?(:clj ji/execute-sql! :cljs aqi/execute-sql!)
   :execute-stmt! #?(:clj ji/execute-stmt! :cljs aqi/execute-stmt!)
   :upsert-inst-statement #?(:clj h2i/upsert-inst-statement :cljs aqi/upsert-inst-statement)
   :upsert-index-statement #?(:clj h2i/upsert-index-statement :cljs nil)
   :delete-by-id-statement #?(:clj ji/delete-by-id-statement :cljs aqi/delete-by-id-statement)
   :query-by-id-statement #?(:clj ji/query-by-id-statement :cljs aqi/query-by-id-statement)
   :do-query-statement #?(:clj ji/do-query-statement :cljs aqi/do-query-statement)
   :validate-ref-statement #?(:clj ji/validate-ref-statement :cljs aqi/validate-ref-statement)})

(def transact-fn! (:transact-fn! store-fns))
(def execute-fn! (:execute-fn! store-fns))
(def execute-sql! (:execute-sql! store-fns))
(def execute-stmt! (:execute-stmt! store-fns))
(def upsert-inst-statement (:upsert-inst-statement store-fns))
(def upsert-index-statement (:upsert-index-statement store-fns))
(def delete-by-id-statement (:delete-by-id-statement store-fns))
(def query-by-id-statement (:query-by-id-statement store-fns))
(def do-query-statement (:do-query-statement store-fns))
(def validate-ref-statement (:validate-ref-statement store-fns))

(def id-type (sql/attribute-to-sql-type :Kernel/UUID))

(defn- create-relational-table-sql [table-name entity-schema
                                    indexed-attributes unique-attributes]
  (concat
   [(str su/create-table-prefix " " table-name " ("
         (loop [attrs (sort (keys entity-schema)), cols ""]
           (if-let [a (first attrs)]
             (let [atype (cn/attribute-type entity-schema a)
                   sql-type (sql/attribute-to-sql-type atype)
                   uq (when (some #{a} unique-attributes) "NOT NULL UNIQUE")]
               (recur
                (rest attrs)
                (str cols (if (= :Id a)
                            (str "_Id " id-type " PRIMARY KEY")
                            (str "_" (name a) " " sql-type " " uq))
                     (when (seq (rest attrs))
                       ", "))))
             cols))
         ")")]
   (when (seq indexed-attributes)
     (mapv (fn [attr]
             (let [n (name attr)]
               (str "CREATE INDEX "
                    #?(:clj "IF NOT EXISTS "
                       :cljs "")
                    n "_Idx ON " table-name "(_" n ")")))
           indexed-attributes))))

(defn- create-relational-table [connection entity-schema table-name
                                indexed-attrs unique-attributes]
  (let [ss (create-relational-table-sql
            table-name entity-schema indexed-attrs
            unique-attributes)]
    (doseq [sql ss]
      (when-not (execute-sql! connection [sql])
        (u/throw-ex (str "Failed to execute SQL - " sql))))
    table-name))

(defn- create-db-schema!
  "Create a new schema (a logical grouping of tables), if it does not already exist."
  [connection db-schema-name]
  (if (execute-sql! connection [(su/create-schema-sql db-schema-name)])
    db-schema-name
    (u/throw-ex (str "Failed to create schema - " db-schema-name))))

(defn- drop-db-schema! [connection db-schema-name]
  (if (execute-sql! connection [(su/drop-schema-sql db-schema-name)])
    db-schema-name
    (u/throw-ex (str "Failed to drop schema - " db-schema-name))))

(defn create-schema
  "Create the schema, tables and indexes for the component."
  [datasource component-name]
  (let [scmname (su/db-schema-for-component component-name)]
    (execute-fn!
     datasource
     (fn [txn]
       (create-db-schema! txn scmname)
       (doseq [ename (cn/entity-names component-name)]
         (when-not (cn/entity-schema-predefined? ename)
           (let [tabname (su/table-for-entity ename)
                 schema (su/find-entity-schema ename)
                 indexed-attrs (cn/indexed-attributes schema)]
             (create-relational-table
              txn schema tabname
              indexed-attrs
              (cn/unique-attributes schema)))))))
    component-name))

(defn drop-schema
  "Remove the schema from the database, perform a non-cascading delete."
  [datasource component-name]
  (let [scmname (su/db-schema-for-component component-name)]
    (execute-fn! datasource
                 (fn [txn]
                 (drop-db-schema! txn scmname)))
    component-name))

(defn- validate-references! [conn inst ref-attrs]
  (doseq [[aname scmname] ref-attrs]
    (let [p (cn/find-ref-path scmname)
          component (:component p)
          entity-name (:record p)
          tabname (su/table-for-entity [component entity-name])
          rattr (first (:refs p))
          colname (name rattr)
          index-tabname (if (= rattr :Id) tabname (su/index-table-name tabname colname))
          [stmt params] (validate-ref-statement conn index-tabname colname (get inst aname))]
      (when-not (seq (execute-stmt! conn stmt params))
        (u/throw-ex (str "Reference not found - " aname ", " p))))))

(defn- remove-unique-attributes [indexed-attrs entity-schema]
  (if-let [uq-attrs (seq (cn/unique-attributes entity-schema))]
    (set/difference (set indexed-attrs) (set uq-attrs))
    indexed-attrs))

(defn upsert-relational-entity-instance [upsert-inst-statement datasource entity-name instance]
  (let [tabname (su/table-for-entity entity-name)
        inst (su/serialize-objects instance)]
    (execute-fn!
     datasource
     #(let [[pstmt params] (upsert-inst-statement % tabname nil [entity-name inst])]
        (execute-stmt! % pstmt params)))
    instance))

(defn upsert-instance
  ([upsert-inst-statement datasource entity-name instance]
   (upsert-relational-entity-instance
    upsert-inst-statement datasource entity-name instance))
  ([datasource entity-name instance]
   (upsert-relational-entity-instance
    upsert-inst-statement datasource entity-name instance)))

(defn- delete-inst!
  "Delete an entity instance."
  [conn tabname id delete-by-id-statement]
  (let [[pstmt params] (delete-by-id-statement conn tabname id)]
    (execute-stmt! conn pstmt params)))

(defn delete-by-id
  ([delete-by-id-statement datasource entity-name id]
   (let [tabname (su/table-for-entity entity-name)]
     (transact-fn!
      datasource
      (fn [txn]
        (delete-inst! txn tabname id delete-by-id-statement)))
     id))
  ([datasource entity-name id]
   (delete-by-id delete-by-id-statement datasource entity-name id)))

(defn compile-query [query-pattern]
  (sql/format-sql
   (su/table-for-entity (:from query-pattern))
   (if (> (count (keys query-pattern)) 2)
     (dissoc query-pattern :from)
     (let [where-clause (:where query-pattern)]
       (when (not= :* where-clause) where-clause)))))

(defn- raw-results [query-fns]
  (flatten (mapv u/apply0 query-fns)))

(defn- query-instances [entity-name query-fns]
   (let [results (raw-results query-fns)]
     (su/results-as-instances entity-name results)))

(defn query-by-id
  ([query-by-id-statement datasource entity-name query-sql ids]
   (execute-fn!
    datasource
    (fn [conn]
      (query-instances
       entity-name
       (mapv #(let [[pstmt params] (query-by-id-statement conn query-sql %)]
                (fn [] (execute-stmt! conn pstmt params)))
             (set ids))))))
  ([datasource entity-name query-sql ids]
   (query-by-id query-by-id-statement datasource entity-name query-sql ids)))

(defn do-query [datasource query-sql query-params]
  (execute-fn!
   datasource
   (fn [conn]
     (let [[pstmt params] (do-query-statement conn query-sql query-params)]
       (execute-stmt! conn pstmt params)))))

(defn- query-relational-entity-by-unique-keys [datasource entity-name unique-keys attribute-values]
  (let [sql (sql/compile-to-direct-query (su/table-for-entity entity-name) (mapv name unique-keys) :and)]
    (when-let [rows (seq (do-query datasource sql (mapv #(attribute-values %) unique-keys)))]
      (su/result-as-instance entity-name (first rows)))))

(defn query-by-unique-keys
  "Query the instance by a unique-key value."
  ([query-by-id-statement datasource entity-name unique-keys attribute-values]
   (query-relational-entity-by-unique-keys
    datasource entity-name unique-keys attribute-values))
  ([datasource entity-name unique-keys attribute-values]
   (query-by-unique-keys nil datasource entity-name unique-keys attribute-values)))

(defn query-all
  ([datasource entity-name rows-to-instances query-sql query-params]
   (execute-fn!
    datasource
    (fn [conn]
      (rows-to-instances
       entity-name
       (let [[pstmt params] (do-query-statement conn query-sql query-params)]
         [#(execute-stmt! conn pstmt params)])))))
  ([datasource entity-name query-sql]
   (query-all datasource entity-name query-instances query-sql nil)))

(defn- query-pk-columns [conn table-name sql]
  (let [pstmt (do-query-statement conn (s/replace sql #"\?" table-name))]
    (mapv :pg_attribute/attname (execute-stmt! conn pstmt nil))))

(defn- mark-pks [pks schema]
  (mapv
   #(let [colname (:columns/column_name %)]
      (if (some #{colname} pks)
        (assoc % :columns/pk true)
        %))
   schema))

(defn- normalize-table-schema [type-lookup cols]
  (apply
   merge
   (mapv
    (fn [c]
      (if-let [t (type-lookup (:columns/data_type c))]
        {(keyword (:columns/column_name c))
         (merge {:type t} (when (:columns/pk c) {:unique true :immutable true}))}
        (u/throw-ex (str "type not supported - " (:columns/data_type c)))))
    cols)))

(defn fetch-schema [datasource fetch-schema-sql
                    get-table-names fetch-columns-sql
                    fetch-pk-columns-sql type-lookup]
  (execute-fn!
   datasource
   (fn [conn]
     (let [[pstmt params] (do-query-statement conn fetch-schema-sql nil)
           tabnames (get-table-names
                     (raw-results
                      [#(execute-stmt! conn pstmt params)]))
           col-pstmt (do-query-statement conn fetch-columns-sql)]
       (mapv
        (fn [tn]
          (let [pks (query-pk-columns conn tn fetch-pk-columns-sql)
                r (raw-results
                   [#(execute-stmt!
                      conn col-pstmt [tn])])]
            {(keyword tn) (normalize-table-schema type-lookup (mark-pks pks r))}))
        tabnames)))))
