(ns fractl.store.h2
  "The storage layer implementation for H2 database.
   Ideal for testing the storage protocol integration with
   the runtime resolver, demos etc."
  (:require [fractl.util :as u]
            [fractl.store.protocol :as p]
            [fractl.store.util :as su]
            [fractl.store.jdbc-cp :as cp]
            [fractl.store.db-common :as db]))

(def ^:private driver-class "org.h2.Driver")
(def ^:private url-prefix "jdbc:h2:")

(defn make []
  (let [datasource (u/make-cell)]
    (reify p/Store
      (open-connection [store connection-info]
        (let [connection-info (su/normalize-connection-info connection-info)
              jdbc-url (str url-prefix (:dbname connection-info))
              username (or (:username connection-info) "sa")
              password (or (:password connection-info) "")]
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
          (do (u/safe-set
               datasource
               (when @datasource
                 (cp/close-pooled-datasource @datasource)
                 nil))
              true)
          (catch Exception _ false)))
      (connection-info [_]
        (if @datasource
          @datasource
          {}))
      (create-schema [_ component-name]
        (db/create-schema @datasource component-name))
      (drop-schema [_ component-name]
        (db/drop-schema @datasource component-name))
      (upsert-instance [_ entity-name instance]
        (db/upsert-instance @datasource entity-name instance))
      (delete-by-id [_ entity-name id]
        (db/delete-by-id @datasource entity-name id))
      (query-by-id [_ entity-name query ids]
        (db/query-by-id @datasource entity-name query ids))
      (query-all [_ entity-name query]
        (db/query-all @datasource entity-name query))
      (do-query [_ query params]
        (db/do-query @datasource query params))
      (compile-query [_ query-pattern]
        (db/compile-to-indexed-query query-pattern))
      (get-reference [_ path refs]))))
