(ns fractl.store.sqljs
  "The storage layer implementation for SQL.js database."
  (:require [fractl.util :as u]
            [fractl.store.util :as su]
            ;[fractl.store.protocol :as p]
            [fractl.store.proj :as p]
            ;[fractl.store.sqljs-internal :as i]
            [fractl.store.alasql-internal :as i]))

(defn make []
  (let [datasource (u/make-cell)]
    (reify p/Store
      (open-connection [store connection-info]
        (let [connection-info (su/normalize-connection-info connection-info)]
          (u/safe-set-once
            datasource
            (. alasql Database (:dbname connection-info)))
          true))
      (close-connection [_]
        (try
          (do (u/safe-set-result
                datasource
                (when @datasource
                  (cp/close-pooled-datasource @datasource)
                  nil))
              true)
          (catch Exception _ false)))
      (create-schema [_ component-name]
        (i/create-schema @datasource component-name))
      (drop-schema [_ component-name]
        (i/drop-schema @datasource component-name))
      (upsert-instance [_ entity-name instance]
        (i/upsert-instance @datasource entity-name instance))
      (delete-instance [_ entity-name instance]
        (i/delete-instance @datasource entity-name instance))
      (query-by-id [_ entity-name query ids]
        (i/query-by-id @datasource entity-name query ids))
      (do-query [_ query params]
        (i/do-query @datasource query params))
      (compile-query [_ query-pattern]
        (i/compile-to-indexed-query query-pattern)))))

#_(defn make []
  (let [datasource (u/make-cell)]
    (reify p/Store
      (open-connection [_ connection-info]
        (let [connection-info (su/normalize-connection-info connection-info)]
          (i/open-connection connection-info set-f)
          true))
      (close-connection [_]
        (try
          (do (u/safe-set-result
               datasource
               (when @datasource
                 (i/close-connection @datasource)
                 nil))
              true)
          (catch js/Error _ false)))
      (create-schema [_ component-name]
        (i/create-schema @datasource component-name))
      (drop-schema [_ component-name]
        (i/drop-schema @datasource component-name))
      (upsert-instance [_ entity-name instance]
        (i/upsert-instance @datasource entity-name instance))
      (delete-instance [_ entity-name instance]
        (i/delete-instance @datasource entity-name instance))
      (query-by-id [_ entity-name query ids]
        (i/query-by-id @datasource entity-name query ids))
      (do-query [_ query params]
        (i/do-query @datasource query params))
      (compile-query [_ query-pattern]
        (i/compile-to-indexed-query query-pattern)))))



(defn make []
  (reify
    p/Store
    (create-schema [this component-name]
      (i/create-schema component-name))))