(ns fractl.store.sql
  "Support for translating dataflow-query patterns to generic SQL."
  (:require [honeysql.core :as hsql]))

(defn- select-from-index-table [index-table-name where-clause]
  (if (= :Id (second where-clause))
    {:result (nth where-clause 2)}
    {:query
     (hsql/format {:select [:id]
                   :from [(keyword index-table-name)]
                   :where where-clause})}))

(defn compile-to-indexed-query [table-name-fn index-table-name-fn query-pattern]
  (let [table (table-name-fn (:from query-pattern))
        where-clause (:where query-pattern)
        norm-where-clause (if (= :and (first where-clause))
                            (rest where-clause)
                            [where-clause])
        index-tables (map #(index-table-name-fn table (second %)) norm-where-clause)]
    {:id-queries
     (vec (map #(select-from-index-table %1 %2)
               index-tables norm-where-clause))
     :query
     (str "SELECT * FROM " table " WHERE id = ?")}))
