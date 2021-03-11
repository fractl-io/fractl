(ns fractl.store.sfdc-metadata
  "Define a storage layer on top of SFDC Metadata API."
  (:require [clojure.string :as s]
            [fractl.util :as u]
            [fractl.store.util :as su]
            [fractl.store.protocol :as p])
  (:import [fractl.store.sfdc MetadataLoginUtil]))

(def ^:private login-url "https://login.salesforce.com/services/Soap/c/51.0")

(defn- service-endpoint [conn]
  (-> (.getConfig conn)
      .getServiceEndpoint))

(defn- session-id [conn]
  (-> (.getConfig conn)
      .getSessionId))

(defn- sfdc-login [username password]
  (let [conn (MetadataLoginUtil/login username password login-url)]
    conn))

(defn make []
  (let [datasource (u/make-cell)]
    (reify p/Store
      (open-connection [store connection-info]
        (let [connection-info (su/normalize-connection-info connection-info)
              username (or (:username connection-info) (System/getenv "SFDC_USERNAME"))
              password (or (:password connection-info) (System/getenv "SFDC_PASSWORD"))]
          (u/safe-set-once
           datasource
           #(sfdc-login username password))
          true))
      (close-connection [_]
        (do (u/safe-set datasource nil)
            true))
      (connection-info [_]
        (or @datasource {}))
      (create-schema [_ component-name]
        component-name)
      (drop-schema [_ component-name]
        component-name)
      (upsert-instance [_ entity-name instances]
        ;; TODO: instance should ideally be a composite of multiple SFDC objects.
        ;; Call the bulk upsert API
        )
      (delete-by-id [_ entity-name id]
        ;; TODO: call the delete bulk/SOAP API
        )
      (query-by-id [_ entity-name query ids]
        ;; TODO: call the bulk/SOAP fetch API
        )
      (query-all [_ entity-name query]
        ;; TODO: call the bulk API query, return result
        )
      (do-query [_ query params]
        ;; TODO: call the bulk API query, return result
        )
      (compile-query [_ query-pattern]
        ;; TODO: compile query to a bulk API query
        )
      (get-reference [_ path refs]
        nil)
      (pull [store options]
        ))))
