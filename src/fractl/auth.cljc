(ns fractl.auth
  (:require [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.auth.cognito]
            [fractl.auth.df]
            [fractl.resolver.registry :as rr]
            [fractl.resolver.authentication :as authn]))

(defn- maybe-signup-user [evaluator email password]
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
                   {:Name "Superuser"
                    :FirstName "Superuser"
                    :LastName "Superuser"
                    :Email email
                    :Password password}}}}))))

(defn setup-resolver [config evaluator]
  (let [resolver (authn/make :authentication config)
        admin-email (:superuser-email config)
        admin-password (:superuser-password config)]
    (when-not admin-email
      (u/throw-ex (str "superuser email not set in auth-config")))
    (let [resolver-ret (rr/compose-resolver
                        [:Fractl.Kernel.Identity/User
                         :Fractl.Kernel.Rbac/Role
                         :Fractl.Kernel.Rbac/RoleAssignment]
                        resolver)]
      (when-not (maybe-signup-user evaluator admin-email admin-password)
        (u/throw-ex (str "failed to create local user for " admin-email)))
      resolver-ret)))
