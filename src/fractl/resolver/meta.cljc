(ns fractl.resolver.meta
  "Dynamic model definition"
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]))

(defn- load-model-from-meta [model-name]
  ;; TODO: implement model-loading from store
  ;; See https://github.com/fractl-io/fractl/issues/471
  )

(defn- meta-eval [fractl-api event-instance]
  (let [[c n] (cn/instance-type event-instance)]
    (when (= c :Kernel)
      (case n
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
