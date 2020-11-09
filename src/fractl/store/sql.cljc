(ns fractl.store.sql
  "Support for translating dataflow-query patterns to generic SQL.")

(defn- parameterized-where-clause [where-clause]
  (let [col (second where-clause)]
    [(first where-clause) col (keyword (str "?" (name col)))]))

(defn- select-from-index-table [index-table-name where-clause]
  {:query
   {:select [:id]
    :from index-table-name
    :where (parameterized-where-clause where-clause)}
   :param (nth where-clause 2)})

(defn compile-to-indexed-query [table-name-fn index-table-name-fn query-pattern]
  (let [from (table-name-fn (:from query-pattern))
        where-clause (:where query-pattern)
        norm-where-clause (if (= :and (first where-clause))
                            (rest where-clause)
                            [where-clause])
        index-tables (map #(index-table-name-fn from (second %)) norm-where-clause)]
     {:index-queries
      (vec (map #(select-from-index-table %1 %2)
                index-tables norm-where-clause))
      :query
      {:select [:*]
       :from from
       :where [:id :in :?id]}}))
