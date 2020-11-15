(ns fractl.store
  (:require [fractl.store.protocol :as p]
            [fractl.store.h2 :as h2]
            [fractl.util :as u]))

(def ^:private default-store (u/make-cell))

(defn- make-default-store-config []
  #?(:clj {:dbname (str "./fractl.db." (System/currentTimeMillis))}))

(defn open-default-store [store-config]
  #?(:clj
     (u/safe-set-once
      default-store
      #(let [store (h2/make)]
         (p/open-connection store (or store-config
                                      (make-default-store-config)))
         ;; NOTE: The default db connection, if opened,
         ;; will last the lifetime of the app.
         store))))

(def open-connection p/open-connection)
(def close-connection p/close-connection)
(def create-schema p/create-schema)
(def drop-schema p/drop-schema)
(def upsert-instance p/upsert-instance)
(def delete-instance p/delete-instance)
(def query-by-id p/query-by-id)
(def do-query p/do-query)
(def compile-query p/compile-query)

(defn upsert-instances [store record-name insts]
  (doseq [inst insts]
    (p/upsert-instance store record-name inst))
  insts)
