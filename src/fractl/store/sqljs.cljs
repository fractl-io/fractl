(ns fractl.store.sqljs
  "The storage layer implementation for SQL.js database."
  (:require [fractl.util :as u]
            [fractl.store.protocol :as p]
            [fractl.store.sqljs-internal :as i]))

(defn make []
  (let [datasource (atom {})]
    (reify p/Store
      (open-connection [this connection-info]
        true)
      (close-connection [this]
        true)
      (create-schema [this component-name]
        (i/create-schema component-name))
      (drop-schema [this component-name]
        (i/drop-schema component-name))
      (upsert-instance [this entity-name instance]
        (i/upsert-instance entity-name instance))
      (delete-instance [this entity-name instance]
        (i/delete-instance entity-name instance))
      (query-by-id [this entity-name query ids]
        (i/query-by-id entity-name query ids))
      (do-query [this query params]
        (i/do-query query params))
      (compile-query [this query-pattern]
        (i/compile-to-indexed-query query-pattern)))))
