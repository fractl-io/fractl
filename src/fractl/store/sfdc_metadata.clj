(ns fractl.store.sfdc-metadata
  "Define a storage layer on top of SFDC Metadata API."
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.xml :as xml]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.store.util :as su]
            [fractl.store.protocol :as p])
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

(defn- members-tag [n]
  {:tag :members :content [(str n)]})

(def ^:private all-members (members-tag "*"))

(defn- types-tag [opt]
  (let [has-query (seqable? opt)
        [_ m] (li/split-path (if has-query (first opt) opt))
        n (name m)]
    {:tag :types :content
     (concat
      (if has-query
        (concat [{:tag :name :content [n]}]
                (if (seqable? (second opt))
                  (map members-tag (second opt))
                  [(members-tag (second opt))]))
        [{:tag :name :content [n]} all-members])
      [{:tag :version :content [(u/getenv "SFDC_METADATA_API_VERSION" "51.0")]}])}))

(defn- manifest-from-options [options]
  (let [content (map types-tag options)
        xml {:tag :Package :attrs
             {:xmlns (u/getenv
                      "SFDC_METADATA_SCHEMA_URL"
                      "http://soap.sforce.com/2006/04/metadata")}
             :content content}]
    (with-out-str (xml/emit xml))))

(def ^:private zip-file-name "components.zip")
(def ^:private manifest-file-name "package.xml")

(defn- write-manifest! [xml]
  (with-open [w (io/writer manifest-file-name)]
    (.write w xml)))

(defn make []
  (let [datasource (u/make-cell)]
    (reify p/Store
      (open-connection [store connection-info]
        (let [connection-info (su/normalize-connection-info connection-info)
              username (or (:username connection-info) (u/getenv "SFDC_USERNAME"))
              password (or (:password connection-info) (u/getenv "SFDC_PASSWORD"))]
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
        (let [manifest-xml (manifest-from-options options)
              mpp (MetadataPushPull. @datasource)]
          (write-manifest! manifest-xml)
          (.retrieveZip mpp zip-file-name manifest-file-name)
          true)))))
