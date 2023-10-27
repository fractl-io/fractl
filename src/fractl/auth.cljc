(ns fractl.auth
  (:require [clojure.string :as s]
            [fractl.util :as u]
            #?(:clj [fractl.auth.cognito])
            #?(:clj [fractl.auth.df])
            [fractl.resolver.registry :as rr]
            [fractl.resolver.authentication :as authn]))

(defn- maybe-signup-user [evaluator names email password]
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
                   (merge
                    names
                    {:Password password
                     :Email email})}}}))))

(defn- email-to-names [email default-last-name]
  (let [[n _] (s/split email #"@")
        names (s/split n #"\.")
        first-name (first names)
        last-name (or (second names) default-last-name)]
    {:Name email
     :FirstName first-name
     :LastName last-name}))

(defn setup-resolver [config evaluator]
  (let [resolver (authn/make :authentication config)
        admin-email (:superuser-email config)
        admin-password (u/getenv "FRACTL_SUPERUSER_PASSWORD")]
    (when-not admin-email
      (u/throw-ex (str "superuser email not set in auth-config")))
    (when-not admin-password
      (u/throw-ex (str "FRACTL_SUPERUSER_PASSWORD not set")))
    (rr/compose-resolver
     [:Fractl.Kernel.Identity/User
      :Fractl.Kernel.Rbac/Role
      :Fractl.Kernel.Rbac/RoleAssignment]
     resolver)
    (when-not (maybe-signup-user
               evaluator (email-to-names admin-email "superuser")
               admin-email admin-password)
      (u/throw-ex (str "failed to create local user for " admin-email)))
    resolver))
