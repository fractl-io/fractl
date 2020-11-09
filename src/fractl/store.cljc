(ns fractl.store
  (:require [fractl.store.protocol :as p]
            [fractl.store.h2 :as h2]
            [fractl.util :as u]))

(def ^:private default-store (u/make-cell))

(defn get-default-store []
  (u/safe-set-once
   default-store
   #(let [store (h2/make)]
      ;; NOTE: The default db connection will last the lifetime of the app.
      (p/open-connection store {:dbname "./test.db"})
      store)))

(def open-connection p/open-connection)
(def close-connection p/close-connection)
(def create-schema p/create-schema)
(def drop-schema p/drop-schema)
(def upsert-instance p/upsert-instance)
(def delete-instance p/delete-instance)
(def query-by-id p/query-by-id)
(def do-query p/do-query)
(def compile-query p/compile-query)
