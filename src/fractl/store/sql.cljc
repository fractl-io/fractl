(ns fractl.store.sql
  "Support for translating dataflow-query patterns to generic SQL."
  (:require [clojure.string :as str]
            [honeysql.core :as hsql]
            [fractl.util :as u]))

(defn- select-from-index-table [index-table-name where-clause]
  (if (= :Id (keyword (second where-clause)))
    {:result (nth where-clause 2)}
    {:query
     (hsql/format {:select [:*]
                   :from [(keyword index-table-name)]
                   :where (if (seqable? (first where-clause))
                            (first where-clause)
                            where-clause)})}))

(defn- concat-where-clauses [clauses]
  (reduce conj [:and] clauses))

(defn compile-to-indexed-query [table-name-fn index-table-name-fn query-pattern]
  (let [table (table-name-fn (:from query-pattern))
        where-clause (:where query-pattern)]
    (if (= :* where-clause)
      {:query (str "SELECT * FROM " table)}
      (let [and-clause (= :and (first where-clause))
            norm-where-clause (if and-clause
                                (rest where-clause)
                                [where-clause])
            index-tables (set (map #(index-table-name-fn table (second %)) norm-where-clause))]
        (when (and and-clause (> (count index-tables) 1))
          (u/throw-ex (str "cannot merge multiple indices under an `and` clause - " index-tables)))
        {:id-queries
         (if and-clause
           [(select-from-index-table
             (first index-tables)
             (concat-where-clauses norm-where-clause))]
           (vec (map #(select-from-index-table %1 %2)
                     index-tables norm-where-clause)))
         :query
         (str "SELECT * FROM " table " WHERE Id = ?")}))))

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

(defn sql-index-type
  ([max-varchar-length bool-type date-time-type attribute-type]
   (case attribute-type
     (:Kernel/String
      :Kernel/Keyword :Kernel/Email
      :Kernel/DateTime :Kernel/Date :Kernel/Time)
     (str "VARCHAR(" max-varchar-length ")")
     :Kernel/UUID "UUID"
     :Kernel/Int "INT"
     (:Kernel/Int64 :Kernel/Integer) "BIGINT"
     :Kernel/Float "REAL"
     :Kernel/Double "DOUBLE"
     :Kernel/Decimal "DECIMAL"
     :Kernel/Boolean bool-type
     (u/throw-ex (str "type cannot be indexed - " attribute-type))))
  ([attribute-type]
   #?(:clj
      ;; For postgres
      (sql-index-type "10485760" "BOOLEAN" "DATE" attribute-type)
      ;(sql-index-type Integer/MAX_VALUE "BOOLEAN" "DATE" attribute-type)
      :cljs (sql-index-type (.-MAX_SAFE_INTEGER js/Number) "BOOLEAN" "DATE" attribute-type))
   ))
