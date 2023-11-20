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
              final-to-table (mapv second final-tables)]
          ;; TODO: compute diff and generate migration sql.
          final-to-table)))))
