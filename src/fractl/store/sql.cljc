(ns fractl.store.sql
  "Support for translating dataflow-query patterns to generic SQL."
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [honeysql.core :as hsql]
            [fractl.util :as u]
            [fractl.store.util :as su]
            [fractl.lang.internal :as li]
            [fractl.lang.kernel :as k]))

(defn- attach-column-name-prefixes [where-clause]
  (w/prewalk
   #(if (and (keyword? %) (not (li/operator? %)))
      (keyword (su/attribute-column-name %))
      %)
   where-clause))

(defn- make-wildcard [query]
  (if (su/aggregate-query? query)
    (mapv
     #(let [c (get query %)]
        (keyword (str "%" (name %) "." (when (not= c :*) "_") (name (get query %)))))
     (keys (dissoc query :where)))
    [:*]))

(defn format-sql [table-name query]
  (let [wildcard (make-wildcard query)
        final-pattern
        (if (map? query)
          (merge
           {:select wildcard :from [(keyword table-name)]}
           (attach-column-name-prefixes query))
          (let [where-clause (attach-column-name-prefixes query)
                p {:select wildcard
                   :from [(keyword table-name)]}]
            (if where-clause
              (assoc p :where
                     (let [f (first where-clause)]
                       (cond
                         (string? f)
                         [(keyword f) (keyword (second where-clause)) (nth where-clause 2)]
                         (seqable? f) f
                         :else where-clause)))
              p)))]
    (hsql/format final-pattern)))

(defn- concat-where-clauses [clauses]
  (if (> (count clauses) 1)
    (reduce conj [:and] clauses)
    (first clauses)))

(defn compile-to-direct-query
  ([table-name col-names log-opr-tag]
   (let [sql (str "SELECT * FROM " table-name)
         logopr (if (= log-opr-tag :and) "AND " "OR ")]
     (if (= :* col-names)
       sql
       (str sql " WHERE "
            (loop [cs col-names, s ""]
              (if-let [c (first cs)]
                (recur (rest cs)
                       (str s "_" c " = ? "
                            (when (seq (rest cs))
                              logopr)))
                s))))))
  ([table-name col-names]
   (compile-to-direct-query table-name col-names :and)))

(defn attribute-to-sql-type
  ([max-varchar-length bool-type date-time-type attribute-type]
   (if-let [root-type (k/find-root-attribute-type attribute-type)]
     (case root-type
       (:Kernel.Lang/String
        :Kernel.Lang/Keyword :Kernel.Lang/Email :Kernel.Lang/Password
        :Kernel.Lang/DateTime :Kernel.Lang/Date :Kernel.Lang/Time
        :Kernel.Lang/List :Kernel.Lang/Edn :Kernel.Lang/Any :Kernel.Lang/Map
        :Kernel.Lang/Path)
       (str "VARCHAR(" max-varchar-length ")")

       :Kernel.Lang/UUID "UUID"
       :Kernel.Lang/Int "INT"
       (:Kernel.Lang/Int64 :Kernel.Lang/Integer) "BIGINT"
       :Kernel.Lang/Float "REAL"
       :Kernel.Lang/Double "DOUBLE"
       :Kernel.Lang/Decimal "DECIMAL"
       :Kernel.Lang/Boolean bool-type
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
