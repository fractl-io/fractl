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

(defn- create-relational-table-sql [table-name entity-schema
                                    indexed-attributes unique-attributes
                                    compound-unique-attributes post-init-sqls]
  (let [afk (partial append-fkeys table-name)]
    (concat
     [(str stu/create-table-prefix " " table-name " ("
           (loop [attrs (sort (keys entity-schema)), cols ""]
             (if-let [a (first attrs)]
               (let [atype (cn/attribute-type entity-schema a)
                     sql-type (sql/attribute-to-sql-type atype)
                     is-ident (cn/attribute-is-identity? entity-schema a)
                     attr-ref (cn/attribute-ref entity-schema a)
                     uq (if is-ident
                          (str "CONSTRAINT " (pk table-name) " PRIMARY KEY")
                          (when (some #{a} unique-attributes)
                            (str "CONSTRAINT " (uk table-name (as-col-name a)) " UNIQUE")))]
                 #?(:clj
                    (when attr-ref
                      (swap! post-init-sqls concat (afk [a attr-ref]))))
                 (recur
                  (rest attrs)
                  (str cols (str (as-col-name a) " " sql-type " " uq)
                       (when (seq (rest attrs))
                         ", "))))
               (concat-sys-cols cols)))
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
                                compound-unique-attributes post-init-sqls]
  (let [ss (create-relational-table-sql
            table-name entity-schema indexed-attrs
            unique-attributes compound-unique-attributes post-init-sqls)]
    (doseq [sql ss]
      (when-not (execute-sql! connection [sql])
        (u/throw-ex (str "Failed to execute SQL - " sql))))
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

(defn create-schema
  "Create the schema, tables and indexes for the component."
  [datasource component-name]
  (let [scmname (stu/db-schema-for-component component-name)
        post-init-sqls (atom [])]
    (execute-fn!
     datasource
     (fn [txn]
       (doseq [ename (cn/entity-names component-name false)]
         (when-not (cn/entity-schema-predefined? ename)
           (let [tabname (stu/entity-table-name ename)
                 schema (stu/find-entity-schema ename)]
             (create-relational-table
              txn schema tabname
              (cn/indexed-attributes schema)
              (cn/unique-attributes schema)
              (cn/compound-unique-attributes ename)
              post-init-sqls))))
       (doseq [sql @post-init-sqls]
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

(defn- remove-unique-attributes [indexed-attrs entity-schema]
  (if-let [uq-attrs (seq (cn/unique-attributes entity-schema))]
    (set/difference (set indexed-attrs) (set uq-attrs))
    indexed-attrs))

(defn upsert-relational-entity-instance [upsert-inst-statement datasource entity-name instance]
  (let [tabname (stu/entity-table-name entity-name)
        inst (stu/serialize-objects instance)]
    (execute-fn!
     datasource
     #(let [[pstmt params] (upsert-inst-statement % tabname nil [entity-name inst])]
        (execute-stmt! % pstmt params)))
    instance))

(defn upsert-instance [upsert-inst-statement datasource entity-name instance]
  (upsert-relational-entity-instance
   upsert-inst-statement datasource entity-name instance))

(def create-instance (partial upsert-instance create-inst-statement))
(def update-instance (partial upsert-instance update-inst-statement))

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

(defn- table-drop [table-name]
  (str "DROP TABLE " table-name))

(defn- table-rename [old-name new-name]
  (str "ALTER TABLE " old-name " RENAME TO " new-name))

(defn- drop-column [table-name attr-name]
  (str "ALTER TABLE " table-name " DROP COLUMN " (as-col-name attr-name)))

(defn- rename-column [table-name spec]
  (str "ALTER TABLE " table-name " RENAME COLUMN " (as-col-name (:from spec))
       " TO " (as-col-name (:to spec))))

(defn- alter-column [table-name [attr-name attr-type]]
  (if-let [sql-type (sql/as-sql-type (u/string-as-keyword attr-type))]
    (str "ALTER TABLE " table-name " ALTER COLUMN " (as-col-name attr-name)
         " " sql-type)
    (u/throw-ex (str "failed to find sql-type for " [attr-name attr-type]))))

(defn- add-column [table-name [attr-name attr-type]]
  (if-let [sql-type (sql/as-sql-type (u/string-as-keyword attr-type))]
    (str "ALTER TABLE " table-name " ADD COLUMN " (as-col-name attr-name)
         " " sql-type)
    (u/throw-ex (str "failed to find sql-type for " [attr-name attr-type]))))

(defn- update-columns [table-name attrs-spec]
  (concat
   (mapv (partial drop-column table-name) (:drop attrs-spec))
   (mapv (partial rename-column table-name) (:rename attrs-spec))
   (mapv (partial alter-column table-name) (:alter attrs-spec))
   (mapv (partial add-column table-name) (:add attrs-spec))))

(defn- add-unique [table-name col-name]
  (str "ALTER TABLE " table-name " ADD CONSTRAINT " (uk table-name col-name) " UNIQUE(" col-name ")"))

(defn- drop-unique [table-name col-name]
  (str "ALTER TABLE " table-name " DROP CONSTRAINT " (uk table-name col-name) " UNIQUE(" col-name ")"))

(defn- create-index [table-name col-name]
  (str "CREATE INDEX " (idx table-name col-name) " ON " table-name "(" col-name ")"))

(defn- drop-index [table-name col-name]
  (str "DROP INDEX " (idx table-name col-name)))

(defn- add-identity [table-name col-name]
  (str "ALTER TABLE " table-name " ADD CONSTRAINT " (pk table-name) " PRIMARY KEY(" col-name ")"))

(defn- drop-identity [table-name]
  (str "ALTER TABLE " table-name " DROP CONSTRAINT " (pk table-name)))

(defn- update-for-contains [table-name tag]
  (let [col-name (as-col-name li/path-attr)]
    (case tag
      :add [(str "ALTER TABLE " table-name " ADD COLUMN " col-name " " (sql/as-sql-type :String))
            (add-unique table-name col-name)
            (create-index table-name col-name)]
      :drop [(str "ALTER TABLE " table-name " DROP COLUMN " col-name)
             (drop-index table-name col-name)]
      nil)))

(defn- update-constraints [table-name spec]
  (concat
   (when-let [ident (:identity spec)]
     [(drop-identity table-name)
      (add-unique table-name (as-col-name ident))])
   (flatten
    (mapv #(create-index table-name (as-col-name %)) (:index spec))
    (mapv #(add-unique table-name (as-col-name %)) (:unique spec))
    (mapv #(drop-unique table-name (as-col-name %)) (:drop-unique spec))
    (mapv #(drop-index table-name (as-col-name %)) (:drop-index spec)))))

(defn plan-changeset [changeset-inst]
  (let [ename (u/string-as-keyword (:Entity changeset-inst))
        table-name (stu/entity-table-name ename)
        opr (u/string-as-keyword (:Operation changeset-inst))]
    (if (= opr :drop)
      [(table-drop table-name)]
      (su/nonils
       `[~(when (= opr :rename)
            (table-rename table-name (stu/entity-table-name
                                      (u/string-as-keyword
                                       (:NewName changeset-inst)))))
         ~@(when-let [attrs (:Attributes changeset-inst)]
             (update-columns table-name attrs))
         ~@(let [conts (:Contains changeset-inst)]
             (when (not= conts :none)
               (update-for-contains table-name conts)))
         ~@(when-let [consts (:Constraints changeset-inst)]
             (update-constraints table-name consts))]))))
