(ns fractl.resolver.meta
  "Dynamic model definition"
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.store :as store]
            [fractl.store.util :as su]
            [fractl.resolver.core :as r]))

(defn- query-meta [meta-type parent]
  (let [store (store/get-default-store)
        q (store/compile-query
           store
           {:from :Kernel/Meta
            :where [:and
                    [:= :Type meta-type]
                    [:= :Parent parent]]})]
    (su/results-as-instances
     :Kernel/Meta
     (store/do-query store (first q) (rest q)))))

(defn- load-model-from-meta [model-name]
  )

(defn- meta-eval [fractl-api event-instance]
  (let [[c n] (cn/instance-name event-instance)]
    (when (= c :Kernel)
      (case n
        :QueryMeta (query-meta (:Type event-instance) (:Parent event-instance))
        :LoadModelFromMeta (load-model-from-meta (:Model event-instance))
        nil))))

(def ^:private resolver-fns
  {:eval {:handler meta-eval}})

(defn make
  "Create and return a policy resolver"
  [resolver-name config]
  (r/make-resolver
   resolver-name
   {:eval {:handler (partial meta-eval (:fractl-api config))}}))
