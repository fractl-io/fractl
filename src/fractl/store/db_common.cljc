(ns fractl.store.db-common
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.walk :as w]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.lang.raw :as raw]
            [fractl.store.util :as stu]
            [fractl.store.sql :as sql]
            [fractl.util.seq :as su]
            [fractl.util.logger :as log]
            #?(:clj [fractl.store.jdbc-internal :as ji])
            #?(:cljs [fractl.store.alasql-internal :as aqi])))

(def ^:private store-fns
  {:transact-fn! #?(:clj ji/transact-fn! :cljs aqi/execute-fn!)
   :execute-fn! #?(:clj ji/execute-fn! :cljs aqi/execute-fn!)
   :execute-sql! #?(:clj ji/execute-sql! :cljs aqi/execute-sql!)
   :execute-stmt! #?(:clj ji/execute-stmt! :cljs aqi/execute-stmt!)
   :create-inst-statement #?(:clj ji/create-inst-statement :cljs aqi/upsert-inst-statement)
   :update-inst-statement #?(:clj ji/update-inst-statement :cljs aqi/upsert-inst-statement)
   :purge-by-id-statement #?(:clj ji/purge-by-id-statement :cljs aqi/delete-by-id-statement)
   :delete-by-id-statement #?(:clj ji/delete-by-id-statement :cljs aqi/delete-by-id-statement)
   :delete-all-statement #?(:clj ji/delete-all-statement :cljs aqi/delete-all-statement)
   :delete-children-statement #?(:clj ji/delete-children-statement :cljs aqi/delete-children-statement)
   :query-by-id-statement #?(:clj ji/query-by-id-statement :cljs aqi/query-by-id-statement)
   :do-query-statement #?(:clj ji/do-query-statement :cljs aqi/do-query-statement)
   :validate-ref-statement #?(:clj ji/validate-ref-statement :cljs aqi/validate-ref-statement)})

(def transact-fn! (:transact-fn! store-fns))
(def execute-fn! (:execute-fn! store-fns))
(def execute-sql! (:execute-sql! store-fns))
(def execute-stmt! (:execute-stmt! store-fns))
(def create-inst-statement (:create-inst-statement store-fns))
(def update-inst-statement (:update-inst-statement store-fns))
(def purge-by-id-statement (:purge-by-id-statement store-fns))
(def delete-by-id-statement (:delete-by-id-statement store-fns))
(def delete-all-statement (:delete-all-statement store-fns))
(def delete-children-statement (:delete-children-statement store-fns))
(def query-by-id-statement (:query-by-id-statement store-fns))
(def do-query-statement (:do-query-statement store-fns))
(def validate-ref-statement (:validate-ref-statement store-fns))

(def id-type (sql/attribute-to-sql-type :Fractl.Kernel.Lang/UUID))

(defn- as-col-name [attr-name]
  (str "_" (name attr-name)))

(defn- append-fkeys [table-name [attr-name [refspec cascade-on-delete]]]
  (let [n (name attr-name)
        ename [(:component refspec) (:record refspec)]]
    (let [constraint-name (str "_" table-name "_" n "_fkey")]
      [(str "ALTER TABLE " table-name " DROP CONSTRAINT IF EXISTS " constraint-name)
       (str "ALTER TABLE " table-name " ADD CONSTRAINT " constraint-name
            " FOREIGN KEY(_" n ") "
            "REFERENCES " (stu/entity-table-name ename)
            "(_" (name (first (:refs refspec))) ")"
            (when cascade-on-delete
              " ON DELETE CASCADE"))])))

(defn- concat-sys-cols [s]
  (str s ", _" stu/deleted-flag-col " BOOLEAN DEFAULT false"))

(defn- uk [table-name col-name]
  (str table-name col-name "_uk"))

(defn- idx [table-name col-name]
  (str table-name col-name "_Idx"))

(defn- pk [table-name]
  (str table-name "_pk"))

(defn- concat-post-init-sql! [out-table-data sqls]
  (let [d (concat (:post-init-sqls @out-table-data) sqls)]
    (swap! out-table-data assoc :post-init-sqls d)))

(defn- create-relational-table-sql [table-name entity-schema
                                    indexed-attributes unique-attributes
                                    compound-unique-attributes out-table-data]
  (let [afk (partial append-fkeys table-name)
        post-init-sql! (partial concat-post-init-sql! out-table-data)
        compound-unique-attributes (if (keyword? compound-unique-attributes)
                                     [compound-unique-attributes]
                                     compound-unique-attributes)]
    (concat
     [(str stu/create-table-prefix " " table-name " ("
           (loop [attrs (sort (keys entity-schema)), col-types [], cols ""]
             (if-let [a (first attrs)]
               (let [atype (cn/attribute-type entity-schema a)
                     sql-type (sql/attribute-to-sql-type atype)
                     is-ident (cn/attribute-is-identity? entity-schema a)
                     is-uk (some #{a} unique-attributes)
                     attr-ref (cn/attribute-ref entity-schema a)
                     col-name (as-col-name a)
                     uq (if is-ident
                          (str "CONSTRAINT " (pk table-name) " PRIMARY KEY")
                          (when is-uk
                            (str "CONSTRAINT " (uk table-name col-name) " UNIQUE")))]
                 #?(:clj
                    (when attr-ref
                      (post-init-sql! (afk [a attr-ref]))))
                 (recur
                  (rest attrs)
                  (conj col-types [col-name sql-type (or is-ident is-uk)])
                  (str cols (str col-name " " sql-type " " uq)
                       (when (seq (rest attrs))
                         ", "))))
               (do (swap! out-table-data assoc :columns col-types)
                   (concat-sys-cols cols))))
           (when (seq compound-unique-attributes)
             (str ", CONSTRAINT " (str table-name "_compound_uks")
                  " UNIQUE "
                  "(" (s/join ", " (mapv as-col-name compound-unique-attributes)) ")"))
           ")")]
     (when (seq indexed-attributes)
       (mapv (fn [attr]
               (let [n (as-col-name attr)]
                 (str "CREATE INDEX "
                      #?(:clj "IF NOT EXISTS "
                         :cljs "")
                      (idx table-name n) " ON " table-name "(" n ")")))
             indexed-attributes)))))

(defn- create-relational-table [connection entity-schema table-name
                                indexed-attrs unique-attributes
                                compound-unique-attributes out-table-data]
  (let [ss (create-relational-table-sql
            table-name entity-schema indexed-attrs
            unique-attributes compound-unique-attributes out-table-data)]
    (doseq [sql ss]
      (when-not (execute-sql! connection [sql])
        (u/throw-ex (str "Failed to create table - " sql))))
    table-name))

(defn- create-db-schema!
  "Create a new schema (a logical grouping of tables), if it does not already exist."
  [connection db-schema-name]
  (if (execute-sql! connection [(stu/create-schema-sql db-schema-name)])
    db-schema-name
    (u/throw-ex (str "Failed to create schema - " db-schema-name))))

(defn- drop-db-schema! [connection db-schema-name]
  (if (execute-sql! connection [(stu/drop-schema-sql db-schema-name)])
    db-schema-name
    (u/throw-ex (str "Failed to drop schema - " db-schema-name))))

(defn- create-component-meta-table-sql [table-name]
  (str "CREATE TABLE IF NOT EXISTS " table-name " (KEY VARCHAR(100) PRIMARY KEY, VALUE VARCHAR(1052))"))

(defn- insert-entity-meta-sql [comp-meta-table entity-table meta-data]
  (str "INSERT INTO " comp-meta-table " VALUES ('" entity-table "', '" meta-data "')"
       " ON CONFLICT DO NOTHING"))

(defn- normalize-meta-result [r]
  (let [r (mapv (fn [[k v]]
                  [(second (li/split-path k)) v])
                r)]
    (into {} r)))

(defn load-component-meta
  ([datasource model-version component-name]
   (let [table-name (stu/component-meta-table-name component-name model-version)]
     (try
       (mapv normalize-meta-result (execute-sql! datasource [(str "SELECT * FROM " table-name)]))
       (catch Exception ex
         (log/error ex)))))
  ([datasource component-name]
   (load-component-meta datasource nil component-name)))

(defn create-schema
  "Create the schema, tables and indexes for the component."
  [datasource component-name]
  (let [scmname (stu/db-schema-for-component component-name)
        table-data (atom nil)
        component-meta-table (stu/component-meta-table-name component-name)]
    (execute-fn!
     datasource
     (fn [txn]
       (execute-sql! txn [(create-component-meta-table-sql component-meta-table)])
       (doseq [ename (cn/entity-names component-name false)]
         (when-not (cn/entity-schema-predefined? ename)
           (let [tabname (stu/entity-table-name ename)
                 schema (stu/find-entity-schema ename)]
             (create-relational-table
              txn schema tabname
              (cn/indexed-attributes schema)
              (cn/unique-attributes schema)
              (cn/compound-unique-attributes ename)
              table-data)
             (execute-sql!
              txn [(insert-entity-meta-sql
                    component-meta-table tabname
                    {:columns (:columns @table-data)})]))))
       (doseq [sql (:post-init-sqls @table-data)]
         (execute-sql! txn [sql]))
       component-name))))

(defn drop-schema
  "Remove the schema from the database, perform a non-cascading delete."
  [datasource component-name]
  (let [scmname (stu/db-schema-for-component component-name)]
    (execute-fn! datasource
                 (fn [txn]
                   (drop-db-schema! txn scmname)))
    component-name))

(defn drop-entity
  [datasource entity-name]
  (let [tabname (stu/entity-table-name entity-name)]
    (execute-fn! datasource
                 (fn [txn]
                   (let [sql (str "DROP TABLE IF EXISTS " tabname " CASCADE")]
                     (execute-sql! txn [sql]))))))

(defn- remove-unique-attributes [indexed-attrs entity-schema]
  (if-let [uq-attrs (seq (cn/unique-attributes entity-schema))]
    (set/difference (set indexed-attrs) (set uq-attrs))
    indexed-attrs))

(defn- upsert-relational-entity-instance [upsert-inst-statement create-mode
                                          datasource entity-name instance]
  (let [tabname (stu/entity-table-name entity-name)
        inst (stu/serialize-objects instance)]
    (execute-fn!
     datasource
     #(do (when create-mode
            (let [id-attr-name (cn/identity-attribute-name entity-name)
                  id-val (id-attr-name instance)
                  [pstmt params] (purge-by-id-statement % tabname id-attr-name id-val)]
              (execute-stmt! % pstmt params)))
          (let [[pstmt params] (upsert-inst-statement % tabname nil [entity-name inst])]
            (execute-stmt! % pstmt params))))
    instance))

(defn upsert-instance [upsert-inst-statement create-mode datasource entity-name instance]
  (upsert-relational-entity-instance
   upsert-inst-statement create-mode datasource entity-name instance))

(def create-instance (partial upsert-instance create-inst-statement true))
(def update-instance (partial upsert-instance update-inst-statement false))

(defn- delete-inst!
  "Delete an entity instance."
  [conn tabname id-attr-name id delete-by-id-statement]
  (let [[pstmt params] (delete-by-id-statement conn tabname id-attr-name id)]
    (execute-stmt! conn pstmt params)))

(defn delete-by-id
  ([delete-by-id-statement datasource entity-name id-attr-name id]
   (let [tabname (stu/entity-table-name entity-name)]
     (execute-fn!
      datasource
      (fn [conn]
        (delete-inst! conn tabname id-attr-name id delete-by-id-statement)))
     id))
  ([datasource entity-name id-attr-name id]
   (delete-by-id delete-by-id-statement datasource entity-name id-attr-name id)))

(defn delete-all [datasource entity-name purge]
  (let [tabname (stu/entity-table-name entity-name)]
    (execute-fn!
     datasource
     (fn [conn]
       (let [pstmt (delete-all-statement conn tabname purge)]
         (execute-stmt! conn pstmt nil))))
    entity-name))

(defn delete-children [datasource entity-name path]
  (let [tabname (stu/entity-table-name entity-name)]
    (execute-fn!
     datasource
     (fn [conn]
       (let [pstmt (delete-children-statement conn tabname path)]
         (execute-stmt! conn pstmt nil))))
    entity-name))

(defn compile-query [query-pattern]
  (sql/format-sql
   (stu/entity-table-name (:from query-pattern))
   (if (> (count (keys query-pattern)) 2)
     (dissoc query-pattern :from)
     (let [where-clause (:where query-pattern)]
       (when (not= :* where-clause) where-clause)))))

(defn- raw-results [query-fns]
  (flatten (mapv u/apply0 query-fns)))

(defn- query-instances [entity-name query-fns]
  (let [results (raw-results query-fns)]
    (stu/results-as-instances entity-name results)))

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
  (let [sql (sql/compile-to-direct-query (stu/entity-table-name entity-name) (mapv name unique-keys) :and)]
    (when-let [rows (seq (do-query datasource sql (mapv #(attribute-values %) unique-keys)))]
      (stu/result-as-instance entity-name (first rows)))))

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
