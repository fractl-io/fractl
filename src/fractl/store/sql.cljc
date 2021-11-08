(ns fractl.store.sql
  "Support for translating dataflow-query patterns to generic SQL."
  (:require [clojure.string :as str]
            [honeysql.core :as hsql]
            [fractl.util :as u]
            [fractl.lang.kernel :as k]))

(defn format-sql [table-name where-clause]
  (if (= :Id (keyword (second where-clause)))
    {:result (nth where-clause 2)}
    {:query
     (hsql/format
      (let [p {:select [:*]
               :from [(keyword table-name)]}]
        (if where-clause
          (assoc p :where
                 (let [f (first where-clause)]
                   (cond
                     (string? f)
                     [(keyword f) (keyword (second where-clause)) (nth where-clause 2)]
                     (seqable? f) f
                     :else where-clause)))
          p)))}))

(defn- concat-where-clauses [clauses]
  (if (> (count clauses) 1)
    (reduce conj [:and] clauses)
    (first clauses)))

(defn compile-to-indexed-query [table-name-fn index-table-name-fn query-pattern]
  (let [table (table-name-fn (:from query-pattern))
        where-clause (:where query-pattern)]
    (if (= :* where-clause)
      {:query (str "SELECT * FROM " table)}
      (let [fc (first where-clause)
            logical-clause (and (keyword? fc)
                                (or (= fc :and) (= fc :or)))
            norm-where-clause (if logical-clause
                                (rest where-clause)
                                (if (vector? (first where-clause))
                                  where-clause
                                  [where-clause]))
            index-tables (distinct (mapv #(index-table-name-fn table (second %)) norm-where-clause))]
        ;; (when (and logical-clause (> (count index-tables) 1))
        ;;   (u/throw-ex (str "cannot merge multiple indices under an `and` clause - " index-tables)))
        (let [qs (cond
                   (= (count index-tables) 1)
                   [(format-sql
                     (first index-tables)
                     (concat-where-clauses norm-where-clause))]
                   
                   (not= (count index-tables) (count norm-where-clause))
                   (u/throw-ex (str "cannot match where clause to index tables - " where-clause))

                   :else
                   (mapv #(format-sql %1 %2) index-tables norm-where-clause))]
          {:id-queries qs
           :merge-opr
           (if logical-clause
             fc
             :or)
           :query
           (str "SELECT * FROM " table " WHERE Id = ?")})))))

(defn compile-to-direct-query [table-name col-names]
  (let [sql (str "SELECT * FROM " table-name)]
    (if (= :* col-names)
      sql
      (str sql " WHERE "
           (loop [cs col-names, s ""]
             (if-let [c (first cs)]
               (recur (rest cs)
                      (str s c " = ? "
                           (when (seq (rest cs))
                             "AND ")))
               s))))))

(defn attribute-to-sql-type
  ([max-varchar-length bool-type date-time-type attribute-type]
   (if-let [root-type (k/find-root-attribute-type attribute-type)]
     (case root-type
       (:Kernel/String
        :Kernel/Keyword :Kernel/Email
        :Kernel/DateTime :Kernel/Date :Kernel/Time
        :Kernel/List)
       (str "VARCHAR(" max-varchar-length ")")

       :Kernel/UUID "UUID"
       :Kernel/Int "INT"
       (:Kernel/Int64 :Kernel/Integer) "BIGINT"
       :Kernel/Float "REAL"
       :Kernel/Double "DOUBLE"
       :Kernel/Decimal "DECIMAL"
       :Kernel/Boolean bool-type
       (u/throw-ex (str "SQL type mapping failed for " attribute-type
                        ", root type is " root-type)))
     (str "VARCHAR(" max-varchar-length ")")))
  ([attribute-type]
   #?(:clj
      ;; For postgres
      (attribute-to-sql-type "10485760" "BOOLEAN" "DATE" attribute-type)
      ;(attribute-to-sql-type Integer/MAX_VALUE "BOOLEAN" "DATE" attribute-type)
      :cljs (attribute-to-sql-type (.-MAX_SAFE_INTEGER js/Number) "BOOLEAN" "DATE" attribute-type))
   ))
