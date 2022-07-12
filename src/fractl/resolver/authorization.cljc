(ns fractl.resolver.authorization
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.auth.internal :as i]))

(defn make [resolver-name config]
  (if-let [client (i/make-client config)]
    (r/make-resolver
     resolver-name
     {:upsert {:handler (partial i/call-upsert-user client config)}
      :delete {:handler (partial i/call-delete-user client config)}})
    (u/throw-ex (str "failed to create auth-client for " resolver-name))))
