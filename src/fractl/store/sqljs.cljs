(ns fractl.store.sqljs
  "The storage layer implementation for SQL.js database."
  (:require [fractl.store.protocol :as p]
            [fractl.store.sqljs-internal :as i]))

(defn make []
  (reify p/Store
    (open-connection [store connection-info])
    (close-connection [_])
    (create-schema [_ component-name]
      (i/create-schema component-name))
    (drop-schema [_ component-name]
      (i/drop-schema component-name))
    (upsert-instance [_ entity-name instance]
      (i/upsert-instance entity-name instance))
    (delete-instance [_ entity-name instance]
      (i/delete-instance entity-name instance))
    (query-by-id [_ entity-name query ids]
      (i/query-by-id entity-name query ids))
    (do-query [_ query params]
      (i/do-query query params))
    (compile-query [_ query-pattern]
      (i/compile-to-indexed-query query-pattern))))
