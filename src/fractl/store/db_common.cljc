(ns fractl.store.db-common
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.walk :as w]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.store.util :as su]
            [fractl.store.sql :as sql]
            [fractl.util.seq :as us]
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
   :delete-by-id-statement #?(:clj ji/delete-by-id-statement :cljs aqi/delete-by-id-statement)
   :delete-all-statement #?(:clj ji/delete-all-statement :cljs aqi/delete-all-statement)
   :query-by-id-statement #?(:clj ji/query-by-id-statement :cljs aqi/query-by-id-statement)
   :do-query-statement #?(:clj ji/do-query-statement :cljs aqi/do-query-statement)
   :validate-ref-statement #?(:clj ji/validate-ref-statement :cljs aqi/validate-ref-statement)})

(def transact-fn! (:transact-fn! store-fns))
(def execute-fn! (:execute-fn! store-fns))
(def execute-sql! (:execute-sql! store-fns))
(def execute-stmt! (:execute-stmt! store-fns))
(def upsert-inst-statement (:upsert-inst-statement store-fns))
(def delete-by-id-statement (:delete-by-id-statement store-fns))
(def delete-all-statement (:delete-all-statement store-fns))
(def query-by-id-statement (:query-by-id-statement store-fns))
(def do-query-statement (:do-query-statement store-fns))
(def validate-ref-statement (:validate-ref-statement store-fns))

(def id-type (sql/attribute-to-sql-type :Kernel/UUID))

(defn- create-relational-table-sql [table-name entity-schema
                                    indexed-attributes unique-attributes
                                    compound-unique-attributes]
  (concat
   [(str su/create-table-prefix " " table-name " ("
         (loop [attrs (sort (keys entity-schema)), cols ""]
           (if-let [a (first attrs)]
             (let [atype (cn/attribute-type entity-schema a)
                   sql-type (sql/attribute-to-sql-type atype)
                   is-ident (cn/attribute-is-identity? entity-schema a)
                   uq (if is-ident
                        "PRIMARY KEY"
                        (when (some #{a} unique-attributes)
                          "NOT NULL UNIQUE"))]
               (recur
                (rest attrs)
                (str cols (str "_" (name a) " " sql-type " " uq)
                     (when (seq (rest attrs))
                       ", "))))
             cols))
         (when (seq compound-unique-attributes)
           (str ", CONSTRAINT " (str table-name "_compound_uks")
                " UNIQUE "
                "(" (s/join ", " (mapv #(str "_" (name %)) compound-unique-attributes)) ")"))
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
                                indexed-attrs unique-attributes
                                compound-unique-attributes]
  (let [ss (create-relational-table-sql
            table-name entity-schema indexed-attrs
            unique-attributes compound-unique-attributes)]
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
           (let [tabname (su/entity-table-name ename)
                 schema (su/find-entity-schema ename)]
             (create-relational-table
              txn schema tabname
              (cn/indexed-attributes schema)
              (cn/unique-attributes schema)
              (cn/compound-unique-attributes ename)))))))
    component-name))

(defn drop-schema
  "Remove the schema from the database, perform a non-cascading delete."
  [datasource component-name]
  (let [scmname (su/db-schema-for-component component-name)]
    (execute-fn! datasource
                 (fn [txn]
                 (drop-db-schema! txn scmname)))
    component-name))

(defn- remove-unique-attributes [indexed-attrs entity-schema]
  (if-let [uq-attrs (seq (cn/unique-attributes entity-schema))]
    (set/difference (set indexed-attrs) (set uq-attrs))
    indexed-attrs))

(defn upsert-relational-entity-instance [upsert-inst-statement datasource entity-name instance]
  (let [tabname (su/entity-table-name entity-name)
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
  [conn tabname id-attr-name id delete-by-id-statement]
  (let [[pstmt params] (delete-by-id-statement conn tabname id-attr-name id)]
    (execute-stmt! conn pstmt params)))

(defn delete-by-id
  ([delete-by-id-statement datasource entity-name id-attr-name id]
   (let [tabname (su/entity-table-name entity-name)]
     (transact-fn!
      datasource
      (fn [txn]
        (delete-inst! txn tabname id-attr-name id delete-by-id-statement)))
     id))
  ([datasource entity-name id-attr-name id]
   (delete-by-id delete-by-id-statement datasource entity-name id-attr-name id)))

(defn delete-all [datasource entity-name]
  (let [tabname (su/entity-table-name entity-name)]
    (transact-fn!
     datasource
     (fn [txn]
       (let [pstmt (delete-all-statement txn tabname)]
         (execute-stmt! txn pstmt nil))))
    entity-name))

(defn- maybe-with-where-clause [q]
  (when q
    (let [sql (first q)]
      (concat
       [(if (s/index-of (s/lower-case sql) "where")
          sql
          (str sql " WHERE 1=1"))]
       (rest q)))))

(defn- merge-queries-with-in-clause [[compiled-queries attr-names]]
  (let [qp (maybe-with-where-clause (first compiled-queries))]
    (loop [cqs (rest compiled-queries)
           attrs (rest attr-names)
           sql (str (first qp) " AND " (su/attribute-column-name (first attr-names)) " IN (")
           params (rest qp)]
      (if-let [qp (maybe-with-where-clause (first cqs))]
        (let [a1 (first attrs)
              a2 (second attrs)]
          (recur (rest cqs) (rest attrs)
                 (str
                  sql (if a1
                        (s/replace (first qp) "*" (su/attribute-column-name a1))
                        (first qp))
                  (when a2
                    (str " AND " (su/attribute-column-name a2)
                         " IN (")))
                 (concat params (rest qp))))
        (vec
         (concat
          [(str sql (s/join (repeat (dec (count attr-names)) \))))]
          params))))))

(defn- merge-as-union-query [queries]
  (vec
   (flatten
    (reduce (fn [a b]
              [(if (seq (first a))
                 (str (first a) " UNION " (first b)) (first b))
               (concat (second a) (rest b))])
            ["" []] queries))))

(defn compile-query [query-pattern]
  (us/case-keys
   query-pattern
   :filter-in-sequence merge-queries-with-in-clause
   :union merge-as-union-query
   (fn [query-pattern]
     (sql/format-sql
      (su/entity-table-name (:from query-pattern))
      (if (> (count (keys query-pattern)) 2)
        (dissoc query-pattern :from)
        (let [where-clause (:where query-pattern)]
          (when (not= :* where-clause) where-clause)))))))

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
  (let [sql (sql/compile-to-direct-query (su/entity-table-name entity-name) (mapv name unique-keys) :and)]
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
