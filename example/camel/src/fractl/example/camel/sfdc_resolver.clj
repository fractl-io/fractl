(ns fractl.example.camel.sfdc-resolver
  (:require [clojure.string :as s]
            [selmer.parser :as st]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.datafmt.json :as json]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [fractl.evaluator.camel :as camel])
  (:import [org.apache.camel Component]
           [org.apache.camel.component.salesforce SalesforceComponent AuthenticationType]))

(defn- ^Component make-component [config]
  (let [^SalesforceComponent sfc (SalesforceComponent.)
        config (or (:salesforce config) config)
        inst-url (or (:instance-url config)
                     (u/getenv "SFDC_INSTANCE_URL"))]
    (.setClientId sfc (or (:client-id config)
                          (u/getenv "SFDC_CLIENT_ID")))
    (.setClientSecret sfc (or (:client-secret config)
                              (u/getenv "SFDC_CLIENT_SECRET")))
    (.setAuthenticationType sfc (AuthenticationType/valueOf "CLIENT_CREDENTIALS"))
    (.setInstanceUrl sfc inst-url)
    (.setLoginUrl sfc inst-url)
    sfc))

(def ^:private create-endpoint-template
  "salesforce:createSObject?apiVersion=59.0&rawPayload=true&format=JSON&sObjectName={{sObjectName}}")

(defn- sf-create [camel-component instance]
  (let [[_ n] (li/split-path (cn/instance-type instance))
        ep (st/render create-endpoint-template {:sObjectName (name n)})
        result (camel/exec-route {:endpoint ep
                                  :user-arg (json/encode
                                             (dissoc
                                              (cn/instance-attributes instance)
                                              li/id-attr))
                                  :camel-component camel-component})
        r (when result (json/decode result))]
    (when (:success r) (assoc instance :Id (:id r)))))

(defn- sf-query [config [entity-name {clause :where} :as param]]
  [])

(rg/register-resolver-type
 :camel-salesforce
 (fn [_ _]
   (let [c (make-component nil)]
     (r/make-resolver
      :camel-salesforce
      {:create (partial sf-create c)
       :query (partial sf-query c)}))))
