(ns fractl.resolver.rbac
  (:require [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.store :as store]
            [fractl.global-state :as gs]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry
             #?(:clj :refer :cljs :refer-macros)
             [defmake]]))

(def ^:private resolver-fns
  {:create {:handler identity}})

(defmake :rbac
  (fn [resolver-name _]
    (r/make-resolver resolver-name resolver-fns)))
