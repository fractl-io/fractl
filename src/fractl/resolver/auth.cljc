(ns fractl.resolver.auth
  "Authentication management"
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.lang.datetime :as dt]))

(def ^:private db (u/make-cell {}))

(defn auth-upsert [inst]
  (let [inst-with-issued
        (assoc inst :Issued (dt/now))]
    (u/safe-set
     db
     (assoc
      @db (:Id inst)
      inst-with-issued))
    inst-with-issued))

(defn- auth-delete [inst]
  (let [id (:Id inst)]
    (u/safe-set
     db
     (dissoc @db id))
    id))

(defn auth-query [id]
  (get @db id))

(def ^:private resolver-fns
  {:upsert {:handler auth-upsert}
   :delete {:handler auth-delete}
   :query {:handler auth-query}})

(defn make
  "Create and return a policy resolver"
  [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
