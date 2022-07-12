(ns fractl.auth
  (:require [fractl.auth.keycloak]
            [fractl.resolver.registry :as rr]
            [fractl.resolver.authorization :as authr]))

(defn setup-resolver [config]
  (let [resolver (authr/make :authorization config)]
    (rr/compose-resolver [:Kernel.RBAC/User] resolver)))
