(ns fractl.resolver.auth
  "Authentication management"
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.lang.datetime :as dt]))

(def ^:private db (u/make-cell {}))

(defn auth-upsert [inst]
  (let [now (dt/now-raw)
        inst-with-issued
        (assoc inst :Issued now)]
    (u/safe-set
     db
     (assoc
      @db (:Id inst)
      inst-with-issued))
    (assoc inst :Issued (dt/as-string now))))

(defn- auth-delete [inst]
  (let [id (:Id inst)]
    (u/safe-set
     db
     (dissoc @db id))
    id))

(defn auth-query [id]
  (when-let [inst (get @db id)]
    (if (> (:ExpirySeconds inst)
           (dt/difference-in-seconds
            (:Issued inst) (dt/now-raw)))
      inst
      (do (auth-delete {:Id id})
          nil))))

(def ^:private resolver-fns
  {:upsert {:handler auth-upsert}
   :delete {:handler auth-delete}
   :query {:handler auth-query}})

(defn make
  "Create and return a policy resolver"
  [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
