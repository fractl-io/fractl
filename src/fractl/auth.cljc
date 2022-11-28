(ns fractl.auth
  (:require [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.auth.keycloak]
            [fractl.auth.cognito]
            [fractl.auth.df]
            [fractl.auth.model :as am]
            [fractl.resolver.registry :as rr]
            [fractl.resolver.authentication :as authn]))

(defn setup-resolver [config]
  (let [resolver (authn/make :authentication config)]
    (rr/compose-resolver
     ;; give :auth-user-type as :Fractl.Meta.Identity/User in config.edn
     (vec (concat [:Kernel.Identity/User] [(:auth-user-type config)]))
     resolver)))
