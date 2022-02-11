(ns fractl.resolver.meta
  "Dynamic model definition"
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]))

(defn meta-upsert [meta-inst]
  ;; TODO: parse the :Spec and define a
  ;; new fractl type.
  meta-inst)

(defn- meta-delete [meta-inst]
  ;; TODO: implement delete
  (:Id meta-inst))

(defn- meta-query [query]
  ;; TODO: implement fractl type lookup
  nil)

(def ^:private resolver-fns
  {:upsert {:handler meta-upsert}
   :delete {:handler meta-delete}
   :query {:handler meta-query}})

(defn make
  "Create and return a policy resolver"
  [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
