(ns fractl.auth
  (:require [fractl.util :as u]
            [fractl.evaluator :as ev]
            [fractl.auth.keycloak]
            [fractl.auth.cognito]
            [fractl.auth.df]
            [fractl.resolver.registry :as rr]
            [fractl.resolver.authentication :as authn]))

(defn setup-resolver [config evaluator]
  (let [resolver (authn/make :authentication config)
        admin-email (:superuser-email config)]
    (when-not admin-email
      (u/throw-ex (str "superuser email not set in auth-config")))
    (if (ev/safe-ok-result
         (evaluator {:Fractl.Kernel.Identity/SignUp
                     {:User
                      {:Fractl.Kernel.Identity/User
                       {:Email admin-email}}}}))
      (rr/compose-resolver
       [:Fractl.Kernel.Identity/User]
       resolver)
      (u/throw-ex (str "failed to create local user for " admin-email)))))
