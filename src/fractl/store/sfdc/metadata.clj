(ns fractl.store.sfdc.metadata
  "Define a storage layer on top of SFDC Metadata API."
  (:require [clojure.string :as s]
            [clojure.walk :as w]
            [clojure.java.io :as io]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.store.util :as su]
            [fractl.store.protocol :as p]
            [fractl.store.db-common :as db]
            [fractl.store.sfdc.metadata-types :as mt]
            [fractl.store.sfdc.persistence :as prs])
  (:import [fractl.store.sfdc MetadataLoginUtil MetadataPushPull]))

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

(def ^:private zip-file-name "components.zip")

(defn- normalize-where-clause [where-clause lookup-fn-params]
  (w/prewalk
   #(if (fn? %)
      (apply % lookup-fn-params)
      %)
   where-clause))

(defn metadata-root []
  (or (u/getenv "SFDC_METADATA_ROOT")
      "."))

(defn make []
  (let [datasource (u/make-cell)
        pull-options (u/make-cell)
        rest-config (u/make-cell)]
    (reify p/Store
      (open-connection [store connection-info]
        (let [connection-info (su/normalize-connection-info connection-info)
              username (or (:username connection-info) (u/getenv "SFDC_USERNAME"))
              password (or (:password connection-info) (u/getenv "SFDC_PASSWORD"))
              auth-token (or (:auth-token connection-info) (u/getenv "SFDC_AUTH_TOKEN"))
              instance-name (or (:instance-name connection-info) (u/getenv "SFDC_INSTANCE_NAME"))
              sfdc-namespace (or (:namespace connection-info) (u/getenv "SFDC_NAMESPACE"))]
          (u/safe-set rest-config {:auth-token auth-token
                                   :instance-name instance-name
                                   :namespace sfdc-namespace})
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
      (create-table [_ entity-name]
        (prs/create-custom-type
         @datasource (:namespace @rest-config)
         entity-name (su/find-entity-schema entity-name)))
      (upsert-instance [_ entity-name instances]
        (let [instances (if (map? instances)
                          [instances]
                          instances)]
          (if (mt/built-in-type (second entity-name))
            (prs/write-object entity-name instances
                              (metadata-root))
            (let [cfg @rest-config]
              (prs/create-record
               (:instance-name cfg)
               (:auth-token cfg)
               (:namespace cfg)
               entity-name instances)))))
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
        (let [mroot (metadata-root)
              conditions (when-let [wc (:where query)]
                           (normalize-where-clause
                            wc (:lookup-fn-params params)))]
          (prs/filter-records (:from query) conditions mroot)))
      (compile-query [_ query-pattern]
        {:query-direct true})
      (get-reference [_ path refs]
        nil)
      (pull [store options]
        (let [mpp (MetadataPushPull. @datasource)]
          (prs/write-manifest! options)
          (.retrieveZip mpp zip-file-name prs/manifest-file-name)
          (u/safe-set pull-options options)
          (prs/init-local-store zip-file-name
                                (metadata-root))))
      (push [store options]
        (let [mroot (metadata-root)
              pkg (prs/prepare-deploy-package mroot)
              mpp (MetadataPushPull. @datasource)]
          (.deployZip mpp pkg)
          (prs/finalize-deploy pkg mroot))))))
