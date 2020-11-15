(ns fractl.store.sqljs
  "The storage layer implementation for SQL.js database."
  (:require [fractl.store.protocol :as p]
            [fractl.store.sqljs-internal :as i]))

(defn make []
  (reify p/sql-store
    (create-schema [model-name]
      (i/create-schema model-name))
    (drop-schema [model-name]
      (i/drop-schema model-name))))

(defn make []
  (reify p/Store
    (open-connection [store connection-info])
    (close-connection [_])
    (create-schema [_ model-name]
      (i/create-schema model-name))
    (drop-schema [_ model-name]
      (i/drop-schema model-name))
    (upsert-instance [_ entity-name instance]
      (i/upsert-instance entity-name instance))
    (delete-instance [_ entity-name instance]
      (i/delete-instance entity-name instance))
    (find-by-id [_ entity-name id]
      (i/find-by-id entity-name id))
    (find-by-query [_ query]
      )))
