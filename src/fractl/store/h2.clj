(ns fractl.store.h2
  "The storage layer implementation for H2 database.
   Ideal for testing the storage protocol integration with
   the runtime resolver, demos etc."
  (:require [fractl.util :as u]
            [fractl.store.protocol :as p]
            [fractl.store.util :as su]
            [fractl.store.jdbc-cp :as cp]
            [fractl.store.h2-internal :as i]))

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
          (do (u/safe-set-result
               datasource
               (when @datasource
                 (cp/close-pooled-datasource @datasource)
                 nil))
              true)
          (catch Exception _ false)))
      (create-schema [_ model-name]
        (i/create-schema @datasource model-name))
      (drop-schema [_ model-name]
        (i/drop-schema @datasource model-name))
      (upsert-instance [_ entity-name instance]
        (i/upsert-instance @datasource entity-name instance))
      (delete-instance [_ entity-name instance]
        (i/delete-instance @datasource entity-name instance))
      (query-by-id [_ query ids]
        (i/query-by-id @datasource query ids))
      (do-query [_ query params]
        (i/do-query @datasource query params))
      (compile-query [_ query-pattern]
        (i/compile-to-indexed-query query-pattern)))))
