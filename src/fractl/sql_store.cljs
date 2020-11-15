(ns fractl.sql-store
  (:require [fractl.store.sqljs-protocol :as p]
            [fractl.store.sqljs :as sq]
            [fractl.util :as u]))

(defn get-default-store []
  (u/safe-set-once
   default-store
   #(let [store (sq/make)]
      store)))

(def create-schema p/create-schema)
(def drop-schema p/drop-schema)