(ns fractl.store.sql
  "Support for translating dataflow-query patterns to generic SQL."
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [honeysql.core :as hsql]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.lang.kernel :as k]))

(defn- attach-column-name-prefixes [where-clause]
  (w/prewalk
   #(if (and (keyword? %) (not (li/operator? %)))
      (keyword (str "_" (name %)))
      %)
   where-clause))

(defn format-sql [table-name where-clause]
  (hsql/format
   (let [where-clause (attach-column-name-prefixes where-clause)
         p {:select [:*]
            :from [(keyword table-name)]}]
     (if where-clause
       (assoc p :where
              (let [f (first where-clause)]
                (cond
                  (string? f)
                  [(keyword f) (keyword (second where-clause)) (nth where-clause 2)]
                  (seqable? f) f
                  :else where-clause)))
       p))))

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
       (:Kernel/String
        :Kernel/Keyword :Kernel/Email :Kernel/Password
        :Kernel/DateTime :Kernel/Date :Kernel/Time
        :Kernel/List :Kernel/Edn)
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
