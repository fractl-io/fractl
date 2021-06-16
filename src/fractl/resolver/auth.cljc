(ns fractl.resolver.auth
  "Authentication management"
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.lang.rule :as rl]
            [fractl.lang.internal :as li]
            [fractl.policy.logging-util :as lu]
            [fractl.resolver.core :as r]))

(def ^:private db (u/make-cell {}))

(defn auth-upsert [inst]
  )

(defn- auth-delete [inst]
  ;; TODO: implement delete
  (:Id inst))

(defn- auth-query [query]
  ;; TODO: implement delete
  nil)

(def ^:private resolver-fns
  {:upsert {:handler auth-upsert}
   :delete {:handler auth-delete}
   :query {:handler auth-query}})

(defn make
  "Create and return a policy resolver"
  [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
