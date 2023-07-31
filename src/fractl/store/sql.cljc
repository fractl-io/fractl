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

(defn- maybe-remove-where [qpat]
  (if (:where qpat) qpat (dissoc qpat :where)))

(defn- with-not-deleted-clause [where]
  [:and [:= su/deleted-flag-col-kw false] where])

(defn format-sql [table-name query]
  (let [group-by (:group-by query)
        query (if group-by (dissoc query :group-by) query)
        wildcard (make-wildcard query)
        interim-pattern
        (maybe-remove-where
         (if (map? query)
           (merge
            {:select wildcard :from [(keyword table-name)]}
            (attach-column-name-prefixes query))
           (let [where-clause (attach-column-name-prefixes query)
                 p {:select wildcard
                    :from [(keyword table-name)]}]
             (if where-clause
               (assoc p :where
                      (with-not-deleted-clause
                       (let [f (first where-clause)]
                         (cond
                           (string? f)
                           [(keyword f) (keyword (second where-clause)) (nth where-clause 2)]
                           (seqable? f) f
                           :else where-clause))))
               p))))
        final-pattern (if group-by
                        (assoc interim-pattern :group-by (mapv #(keyword (str "_" (name %))) group-by))
                        interim-pattern)]
    (println (hsql/format final-pattern))
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
       (str sql " WHERE _" su/deleted-flag-col " = FALSE")
       (str sql " WHERE _" su/deleted-flag-col " = FALSE AND "
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
       (:Fractl.Kernel.Lang/String
        :Fractl.Kernel.Lang/Keyword :Fractl.Kernel.Lang/Email :Fractl.Kernel.Lang/Password
        :Fractl.Kernel.Lang/DateTime :Fractl.Kernel.Lang/Date :Fractl.Kernel.Lang/Time
        :Fractl.Kernel.Lang/List :Fractl.Kernel.Lang/Edn :Fractl.Kernel.Lang/Any :Fractl.Kernel.Lang/Map
        :Fractl.Kernel.Lang/Path)
       (str "VARCHAR(" max-varchar-length ")")

       (:Fractl.Kernel.Lang/UUID :Fractl.Kernel.Lang/Identity) "UUID"
       :Fractl.Kernel.Lang/Int "INT"
       (:Fractl.Kernel.Lang/Int64 :Fractl.Kernel.Lang/Integer) "BIGINT"
       :Fractl.Kernel.Lang/Float "REAL"
       :Fractl.Kernel.Lang/Double "DOUBLE"
       :Fractl.Kernel.Lang/Decimal "DECIMAL"
       :Fractl.Kernel.Lang/Boolean bool-type
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
