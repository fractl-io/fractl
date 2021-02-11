(ns fractl.client.core
  (:require [fractl.client.form :as form]
            [fractl.client.counter :as counter]))

(js/console.log "Write your awesome fractl app here!")
(println "Welcome to Fractl!")
(enable-console-print!)

(def resolver-config
  [{:name :remote-platform
    :type :remote
    :compose? true
    :paths [:Library.Catalog/Book :Library.Identity/User]
    :config {:host "http://localhost:8000"}}])

;(rr/register-resolvers resolver-config)
(form/run-test)
