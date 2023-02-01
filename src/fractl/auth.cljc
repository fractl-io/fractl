(ns fractl.auth
  (:require [fractl.util :as u]
            [fractl.evaluator :as ev]
            [fractl.auth.keycloak]
            [fractl.auth.cognito]
            [fractl.auth.df]
            [fractl.auth.model :as am]
            [fractl.resolver.registry :as rr]
            [fractl.resolver.authentication :as authn]))

(defn setup-resolver [config evaluator]
  (let [resolver (authn/make :authentication config)
        admin-email (:superuser-email config)]
    (when-not admin-email
      (u/throw-ex (str "superuser email not set in auth-config")))
    (if (ev/safe-ok-result
         (evaluator {:Kernel.Identity/SignUp
                     {:User
                      {:Kernel.Identity/User
                       {:Email admin-email}}}}))
      (rr/compose-resolver
       [:Kernel.Identity/User]
       resolver)
      (u/throw-ex (str "failed to create local user for " admin-email)))))
