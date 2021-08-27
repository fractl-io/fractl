(ns fractl.resolver.timer
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.lang.datetime :as dt]))

(def ^:private db (u/make-cell {}))

(defn timer-upsert [inst]
  (let [created (dt/now-raw)]
    (u/call-and-set
     db
     #(assoc
       @db (:Id inst)
       [created inst]))
    inst))

(defn- timer-delete [inst]
  (let [id (:Id inst)]
    (u/call-and-set
     db
     #(dissoc @db id))
    id))

(def ^:private resolver-fns
  {:upsert {:handler timer-upsert}
   :delete {:handler timer-delete}})

(defn make
  "Create and return a policy resolver"
  [_ _]
  (r/make-resolver :timer resolver-fns))
