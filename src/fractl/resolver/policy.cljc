(ns fractl.resolver.policy
  (:require [fractl.resolver.core :as r]))

(defn- policy-upsert [inst]
  ;; TODO: implement upsert
  inst)

(defn- policy-delete [inst]
  ;; TODO: implement delete
  (:Id inst))

(defn- policy-query [query]
  ;; TODO: implement query
  nil)

(def ^:private resolver-fns
  {:upsert {:handler policy-upsert}
   :delete {:handler policy-delete}
   :query {:handler policy-query}})

(defn make [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
