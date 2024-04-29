(ns fractl.example.camel
  (:require [fractl.core :as fractl]
            [fractl.resolver.camel :as camel])
  (:import [org.apache.camel Component]
           [org.apache.camel.component.salesforce SalesforceComponent AuthenticationType]))

(defn- ^Component make-component [config endpoint]
  (let [^SalesforceComponent sfc (SalesforceComponent.)
        config (or (:salesforce config) config)
        inst-url (or (:instance-url config)
                     (u/getenv "SFDC_INSTANCE_URL"))
        is-service (or (s/index-of endpoint "subscribe")
                       (s/index-of endpoint "pubSubSubscribe"))]
    (.setClientId sfc (or (:client-id config)
                          (u/getenv "SFDC_CLIENT_ID")))
    (.setClientSecret sfc (or (:client-secret config)
                              (u/getenv "SFDC_CLIENT_SECRET")))
    (.setAuthenticationType sfc (AuthenticationType/valueOf "CLIENT_CREDENTIALS"))
    (.setInstanceUrl sfc inst-url)
    (.setLoginUrl sfc inst-url)
    {:component sfc :service is-service}))

(defn -main [& args]
  (camel/register-component :salesforce make-component)
  (fractl/run-script ["sfdc.fractl"]))
