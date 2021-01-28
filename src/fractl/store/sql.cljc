(ns fractl.store.sql
  "Support for translating dataflow-query patterns to generic SQL."
  (:require [honeysql.core :as hsql]
            [fractl.util :as u]))

(defn- select-from-index-table [index-table-name where-clause]
  (if (= :Id (second where-clause))
    {:result (nth where-clause 2)}
    {:query
     (hsql/format {:select [:*]
                   :from [(keyword index-table-name)]
                   :where where-clause})}))

(defn compile-to-indexed-query [table-name-fn index-table-name-fn query-pattern]
  (let [table (table-name-fn (:from query-pattern))
        where-clause (:where query-pattern)]
    (if (= :* where-clause)
      {:query (str "SELECT * FROM " table)}
      (let [norm-where-clause (if (= :and (first where-clause))
                                (rest where-clause)
                                [where-clause])
            index-tables (map #(index-table-name-fn table (second %)) norm-where-clause)]
        {:id-queries
         (vec (map #(select-from-index-table %1 %2)
                   index-tables norm-where-clause))
         :query
         (str "SELECT * FROM " table " WHERE Id = ?")}))))

(defn sql-index-type
  ([max-varchar-length bool-type date-time-type attribute-type]
   (case attribute-type
     (:Kernel/String :Kernel/Keyword :Kernel/Email)
     (str "VARCHAR(" max-varchar-length ")")
     :Kernel/DateTime "DATE"
     :Kernel/UUID "UUID"
     :Kernel/Int "INT"
     (:Kernel/Int64 :Kernel/Integer) "BIGINT"
     :Kernel/Float "REAL"
     :Kernel/Double "DOUBLE"
     :Kernel/Decimal "DECIMAL"
     :Kernel/Boolean bool-type
     (u/throw-ex (str "type cannot be indexed - " attribute-type))))
  ([attribute-type]
   #?(:clj (sql-index-type Integer/MAX_VALUE "BOOLEAN" "DATE" attribute-type)
      :cljs (sql-index-type (.-MAX_SAFE_INTEGER js/Number) "BOOLEAN" "DATE" attribute-type))
   ))
