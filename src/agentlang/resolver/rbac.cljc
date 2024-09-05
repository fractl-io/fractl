(ns agentlang.resolver.rbac
  (:require [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.store :as store]
            [agentlang.global-state :as gs]
            [agentlang.resolver.core :as r]
            [agentlang.resolver.registry
             #?(:clj :refer :cljs :refer-macros)
             [defmake]]))

(def ^:private resolver-fns
  {:create {:handler identity}})

(defmake :rbac
  (fn [resolver-name _]
    (r/make-resolver resolver-name resolver-fns)))
