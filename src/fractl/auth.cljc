(ns fractl.auth
  (:require [fractl.util :as u]
            [fractl.auth.cognito]
            [fractl.auth.df]
            [fractl.resolver.registry :as rr]
            [fractl.resolver.authentication :as authn]))

(defn- maybe-signup-user [evaluator email]
  (if-let [user (first
                 (u/safe-ok-result
                  (evaluator
                   {:Fractl.Kernel.Identity/FindUser
                    {:Email email}})))]
    user
    (u/safe-ok-result
     (evaluator {:Fractl.Kernel.Identity/SignUp
                 {:User
                  {:Fractl.Kernel.Identity/User
                   {:Email email}}}}))))

(defn setup-resolver [config evaluator]
  (let [resolver (authn/make :authentication config)
        admin-email (:superuser-email config)]
    (when-not admin-email
      (u/throw-ex (str "superuser email not set in auth-config")))
    (if (maybe-signup-user evaluator admin-email)
      (rr/compose-resolver
       [:Fractl.Kernel.Identity/User
        :Fractl.Kernel.Rbac/Role
        :Fractl.Kernel.Rbac/RoleAssignment]
       resolver)
      (u/throw-ex (str "failed to create local user for " admin-email)))))
