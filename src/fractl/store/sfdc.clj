(ns fractl.store.sfdc
  "Define a storage layer on top of SFDC Bulk API."
  (:require [clojure.string :as s]
            [clojure.xml :as xml]
            [org.httpkit.client :as http]
            [fractl.util :as u]
            [fractl.store.util :as su]
            [fractl.store.protocol :as p]))

(def ^:private login-request
  "<?xml version=\"1.0\" encoding=\"utf-8\" ?>
    <env:Envelope xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\"
                  xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"
                  xmlns:env=\"http://schemas.xmlsoap.org/soap/envelope/\">
      <env:Body>
       <n1:login xmlns:n1=\"urn:partner.soap.sforce.com\">
          <n1:username>$USERNAME</n1:username>
          <n1:password>$PASSWORD</n1:password>
       </n1:login>
      </env:Body>
    </env:Envelope>")

(def ^:private username-pattern #"\$USERNAME")
(def ^:private password-pattern #"\$PASSWORD")

(def ^:private sfdc-login-url
  "https://login.salesforce.com/services/Soap/u/51.0")

(def ^:private sfdc-login-options
  {:headers
   {"Content-Type" "text/xml"
    "SOAPAction" "login"}})

(defn- filter-tag [tag xs]
  (first (filter #(= tag (:tag %)) xs)))

(defn- normalize-data [xs]
  (if (= 1 (count xs))
    (first xs)
    xs))

(defn- normalize-content [xs]
  (if (map? (first xs))
    (let [m (map #(when-let [tag (:tag %)]
                    [tag (normalize-content (:content %))])
                 xs)]
      (into {} (filter identity m)))
    (normalize-data xs)))

(defn- fetch-login-response [response]
  (let [dom (xml/parse
             (java.io.ByteArrayInputStream.
              (.getBytes response)))
        content (:content (first (:content dom)))
        result (:content
                (filter-tag
                 :result
                 (:content (filter-tag :loginResponse content))))]
    (normalize-content result)))

(defn- sfdc-login [username password]
  (let [req (-> (s/replace login-request username-pattern username)
                (s/replace password-pattern password))
        {:keys [status error body]} @(http/post
                                 sfdc-login-url
                                 (assoc sfdc-login-options :body req))]
    (if (= status 200)
      (fetch-login-response body)
      (u/throw-ex (str "SFDC login failed - " {:status status :error error})))))

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
      (upsert-instance [_ entity-name instance]
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
        nil))))
