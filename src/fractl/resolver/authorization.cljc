(ns fractl.resolver.authorization
  (:require [fractl.resolver.core :as r]
            [fractl.auth.core :as auth]))

(defn make [resolver-name config]
  (let [client (auth/make-client config)]
    (r/make-resolver
     resolver-name
     {:upsert {:handler (partial auth/call-upsert-user client config)}
      :delete {:handler (partial auth/call-delete-user client config)}})))
