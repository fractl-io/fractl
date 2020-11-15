(ns fractl.sql-store
  (:require [fractl.store.protocol :as p]
            [fractl.store.sqljs :as sq]
            [fractl.util :as u]))

(def ^:private default-store (u/make-cell))

(defn get-default-store []
  (u/safe-set-once
   default-store
   #(let [store (sq/make)]
      store)))

(def open-connection p/open-connection)
(def close-connection p/close-connection)
(def create-schema p/create-schema)
(def drop-schema p/drop-schema)
(def upsert-instance p/upsert-instance)
(def delete-instance p/delete-instance)
(def find-by-id p/find-by-id)
(def find-by-query p/find-by-query)
