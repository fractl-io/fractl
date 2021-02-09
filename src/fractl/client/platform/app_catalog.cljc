(ns fractl.client.platform.app-catalog
  "Manage a list of deployable applications"
  (:require [fractl.client.platform.identity]
            [fractl.lang.string :as str]
            [fractl.lang.datetime :as dt]
            [fractl.lang :refer [event entity dataflow component]]))

(component :Platform.AppCatalog)

(def valid-name? (partial str/string-in-range? 3 120))
(def app-name? valid-name?)
(def app-artifact? (partial str/string-in-range? 1 10485760)) ; max size - 10MB

(entity {:Application
         {:Name {:check app-name?}
          :Publisher {:ref :Platform.Identity/User.Id
                      :indexed true}
          :PublishDate {:type :Kernel/DateTime
                        :immutable true
                        :default dt/now}
          :LastUpdated {:type :Kernel/DateTime
                        :default dt/now}
          :Artifact {:check app-artifact?}}})

(event {:ListApplications
        {:Publisher :Kernel/UUID}})

(dataflow :ListApplications
          {:Application {:Publisher? :Platform.AppCatalog/ListApplications.Publisher}})
