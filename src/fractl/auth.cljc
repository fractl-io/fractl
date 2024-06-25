(ns fractl.auth
  (:require [clojure.string :as s]
            [fractl.util :as u]
            #?(:clj [fractl.util.logger :as log]
               :cljs [fractl.util.jslogger :as log])
            [fractl.auth.cognito]
            [fractl.auth.okta]
            [fractl.auth.keycloak]
            [fractl.auth.df]
            [fractl.resolver.registry :as rr]
            [fractl.resolver.authentication :as authn]
            #?(:clj [fractl.resolver.redis :as cache])))

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

(defn- setup-cache-resolver [config]
  #?(:clj
     (when config
       (try
         (let [resolver (cache/make :auth-cache config)]
           (rr/override-resolver [:Fractl.Kernel.Identity/SessionCookie] resolver))
         (catch Exception ex
           (log/error ex))))))

(defn setup-resolver [config evaluator]
  (let [r-ident (authn/make :auth-identity config)
        r-roles (authn/make :auth-roles config)
        admin-email (:superuser-email config)
        admin-password (u/getenv "FRACTL_SUPERUSER_PASSWORD" "admin")]
    (when-not admin-email
      (u/throw-ex (str "superuser email not set in auth-config")))
    (when-not admin-password
      (u/throw-ex (str "FRACTL_SUPERUSER_PASSWORD not set")))
    ((if (:is-identity-store config) rr/override-resolver rr/compose-resolver)
     [:Fractl.Kernel.Identity/User]
     r-ident)
    (rr/compose-resolver
     [:Fractl.Kernel.Rbac/Role
      :Fractl.Kernel.Rbac/RoleAssignment]
     r-roles)
    (when-not (maybe-signup-user
               evaluator (email-to-names admin-email "superuser")
               admin-email admin-password)
      (log/error (str "failed to create local user for " admin-email)))
    (when-not (setup-cache-resolver (:cache config))
      (log/warn "failed to setup cache for authentication"))
    true))
