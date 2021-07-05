(ns fractl.store.postgres
  "The storage layer implementation for PostgresSQL."
  (:require [fractl.util :as u]
            [fractl.store.protocol :as p]
            [fractl.store.util :as su]
            [fractl.store.jdbc-cp :as cp]
            [fractl.store.db-common :as db]
            [fractl.store.postgres-internal :as pi]))

(def ^:private driver-class "org.postgresql.Driver")
(def ^:private jdbc-url-prefix "jdbc:postgresql://")

(defn- maybe-uuid-from-str [x]
  (if (string? x)
    (u/uuid-from-string x)
    x))

(defn make []
  (let [datasource (u/make-cell)]
    (reify p/Store
      (open-connection [store connection-info]
        (let [connection-info (su/normalize-connection-info connection-info)
              jdbc-url (str jdbc-url-prefix
                            (or (:host connection-info) "localhost")
                            "/"
                            (or (:dbname connection-info) "v8"))
              username (or (:username connection-info) "postgres")
              password (or (:password connection-info) "posterV8")]
          (u/safe-set-once
           datasource
           #(let [dbspec {:driver-class driver-class
                          :jdbc-url jdbc-url
                          :username username
                          :password password}]
              (cp/open-pooled-datasource dbspec)))
          true))
      (close-connection [_]
        (try
          (do (u/call-and-set
               datasource
               #(when @datasource
                  (cp/close-pooled-datasource @datasource)
                  nil))
              true)
          (catch Exception _ false)))
      (connection-info [_]
        (or @datasource {}))
      (create-schema [_ component-name]
        (db/create-schema @datasource component-name))
      (drop-schema [_ component-name]
        (db/drop-schema @datasource component-name))
      (upsert-instance [_ entity-name instance]
        (db/upsert-instance
         pi/upsert-inst-statement pi/upsert-index-statement
         @datasource entity-name instance))
      (delete-by-id [_ entity-name id]
        (db/delete-by-id @datasource entity-name id))
      (query-by-id [_ entity-name query ids]
        (db/query-by-id
         pi/query-by-id-statement
         @datasource entity-name query (map maybe-uuid-from-str ids)))
      (query-all [_ entity-name query]
        (db/query-all @datasource entity-name query))
      (do-query [_ query params]
        (db/do-query @datasource query params))
      (compile-query [_ query-pattern]
        (db/compile-to-indexed-query query-pattern))
      (get-reference [_ path refs]))))
