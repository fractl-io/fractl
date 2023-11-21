(ns fractl.store.migration
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.store :as store]
            [fractl.store.util :as stu]
            [fractl.store.db-common :as dbc]))

(defn- load-component-meta [datasource vers component-name]
  (when-let [meta (dbc/load-component-meta datasource vers component-name)]
    (into {} (mapv (fn [r] [(:KEY r) (u/parse-string (:VALUE r))]) meta))))

(defn- raise-uk-change-error [table-name col-name]
  (u/throw-ex (str "Migration cannot automatically handle unique-key conversion for " table-name "." col-name)))

(defn- raise-type-change-error [table-name col-name]
  (u/throw-ex (str "Migration cannot automatically handle data-type conversion for " table-name "." col-name)))

(defn- raise-uk-number-error [table-name col-name]
  (u/throw-ex (str "Migration cannot automatically handle addition of unique-numeric column " table-name "." col-name)))

(defn- cols-spec-to-multiple-inserts [from-table to-table cols-spec]
  [(str "SELECT * FROM " from-table)
   (str "INSERT INTO " to-table " (" (s/join ", " (mapv first cols-spec)) ") VALUES "
        "(" (s/join "," (repeat (count cols-spec) \?)) ")")
   (mapv (fn [[_ c]]
           (if (and (string? c) (s/starts-with? c "_"))
             (keyword (subs c 1))
             c))
         cols-spec)])

(defn- cols-spec-to-sql [from-table to-table cols-spec]
  (if (some #(fn? (second %)) cols-spec)
    (cols-spec-to-multiple-inserts from-table to-table cols-spec)
    (str "INSERT INTO " to-table " (" (s/join "," (mapv first cols-spec)) ") "
         "SELECT " (s/join "," (mapv second cols-spec)) " FROM " from-table)))

(defn- generate-inserts [[[from-table from-cols] [to-table to-cols]]]
  (cols-spec-to-sql
   from-table to-table
   (mapv
    (fn [[tc tt tu]]
      (if-let [[c t u] (first (filter #(= tc (first %)) from-cols))]
        (cond
          (and (not u) tu) (raise-uk-change-error to-table tc)
          (= tt t) [tc c]
          :else (raise-type-change-error to-table tc))
        (if tu
          (case tt
            :s [tc u/uuid-string]
            :n (raise-uk-number-error to-table tc))
          (case tt
            :s [tc "NULL"]
            :n [tc 0]))))
    to-cols)))

(defn- preproc-cols [{cols :columns}]
  (mapv (fn [[c t u]]
          [c (if (or (s/starts-with? t "VARCHAR")
                     (= t "UUID"))
               :s
               :n)
           u])
        cols))

(defn- compute-diff [from-tables from-meta to-tables to-meta]
  (mapv
   generate-inserts
   (mapv (fn [f t] [[f (preproc-cols (get from-meta f))]
                    [t (preproc-cols (get to-meta t))]])
         from-tables to-tables)))

(defn migrate [store model-name config]
  (let [model-spec (cn/fetch-model model-name)]
    (when-not model-spec
      (u/throw-ex (str "model " model-name " not loaded")))
    (let [from-vers (:from config)
          to-vers (:version model-spec)
          components (:components model-spec)
          ds (store/connection-info store)
          load-from (partial load-component-meta ds from-vers)
          load-to (partial load-component-meta ds to-vers)]
      (doseq [cn components]
        (let [from-meta (load-from cn), to-meta (load-to cn),
              from-tables (keys from-meta)
              fvs (stu/escape-graphic-chars from-vers)
              from-base (set (map #(subs % 0 (s/index-of % fvs)) from-tables))
              to-tables (keys to-meta)
              tvs (stu/escape-graphic-chars to-vers)
              to-base (set (map #(subs % 0 (s/index-of % tvs)) to-tables))
              final-tables (filter
                            (fn [[f t]]
                              (when-let [fmeta (get from-meta f)]
                                (not= fmeta (get to-meta t))))
                            (mapv (fn [n] [(str n fvs) (str n tvs)]) (set/union to-base from-base)))
              final-from-tables (mapv first final-tables)
              final-to-tables (mapv second final-tables)]
          ;; TODO: apply the diffs by executing the generated SQL.
          (compute-diff final-from-tables from-meta
                        final-to-tables to-meta))))))
