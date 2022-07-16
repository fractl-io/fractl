(ns fractl.auth
  (:require [fractl.auth.keycloak]
            [fractl.resolver.registry :as rr]
            [fractl.resolver.authentication :as authn]))

(defn setup-resolver [config]
  (let [resolver (authn/make :authentication config)]
    (rr/compose-resolver [:Kernel.Identity/User] resolver)))
