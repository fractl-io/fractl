(ns fractl.example.camel.core
  (:require [fractl.core :as fractl]
            [fractl.resolver.camel :as cr]
            [fractl.example.camel.sfdc-resolver :as sfr]))

(defn -main [& args]
  (cr/register-component :salesforce sfr/make-component)
  (fractl/run-script args))
