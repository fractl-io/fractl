(ns agentlang.resolver.meta
  "Dynamic model definition"
  (:require [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.resolver.registry
             #?(:clj :refer :cljs :refer-macros)
             [defmake]]
            [agentlang.resolver.core :as r]))

(defn- load-model-from-meta [model-name]
  ;; TODO: implement model-loading from store
  ;; See https://github.com/fractl-io/fractl/issues/471
  )

(defn- meta-eval [agentlang-api event-instance]
  (let [[c n] (cn/instance-type event-instance)]
    (when (= c :Agentlang.Kernel)
      (case n
        :LoadModelFromMeta (load-model-from-meta (:Model event-instance))
        nil))))

(def ^:private resolver-fns
  {:eval {:handler meta-eval}})

(defmake :meta
  (fn [resolver-name config]
    (r/make-resolver
     resolver-name
     {:eval {:handler (partial meta-eval (:agentlang-api config))}})))
