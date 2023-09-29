(ns fractl.resolver.storage
  (:require [fractl.util :as u]
            [fractl.util.http :as uh]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.storage.core :as core]))

(def ^:private resolver-fns
  {:create {:handler core/create-storage}
   :delete {:handler core/delete-storage}})

(defmake :storage
  (fn [_ _]
    (r/make-resolver :storage resolver-fns)))
