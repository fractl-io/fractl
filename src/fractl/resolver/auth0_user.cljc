(ns fractl.resolver.auth0-user
  "User signup/profile mangement with auth0"
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.component :as cn]
            [fractl.store :as s]
            [fractl.lang.datetime :as dt])
  #?(:clj (:import [fractl.auth.auth0 Auth0AuthUtil])))

(defn auth0-user-upsert [inst]
  #?(:clj
     (let [username (:UserName inst)
           passwd (:Password inst)
           email (:Email inst)
           fields (:Fields inst)
           request (:RequestObject inst)
           auth-api (Auth0AuthUtil/createAuthAPI (:ClientID request)
                                                 (:ClientSecret request)
                                                 (:AuthDomain request)
                                                 true)
           created-user (Auth0AuthUtil/signupUser auth-api username email passwd fields)]
       (if created-user
         (let [user-id (.getUserId created-user)
               user-email (.getEmail created-user)
               user-name (.getUsername created-user)
               inst-with-attrs (assoc inst :UserId user-id :UserName user-name :UserEmail user-email)]
            inst-with-attrs)))))

(defn- delete-auth0-user
  "Delete an auth0 user using the management API"
  [user]
  (let [request (:RequestObject user)
        userId (:UserId user)
        mgmt-api (Auth0AuthUtil/createMgmtAPI (:AuthDomain request)
                                              (:ApiToken request)
                                              true)]
    (Auth0AuthUtil/deleteUser mgmt-api userId)))

(defn- get-user-info
  "Return the UserInfo for the given auth0-user"
  [user]
  ;; login and get token
  (let [username (:UserName user)
        passwd (:Password user)
        request (:RequestObject user)
        scope "openid profile email"
        auth-api (Auth0AuthUtil/createAuthAPI (:ClientID request)
                                              (:ClientSecret request)
                                              (:AuthDomain request)
                                              true)
        token-holder (Auth0AuthUtil/passwordLogin auth-api username passwd scope)]
    (if token-holder
      (Auth0AuthUtil/getUserInfo auth-api (.getAccessToken token-holder)))))
            
(defn auth0-user-delete [inst]
  #?(:clj
     (case (cn/instance-name inst)
       [:Kernel :Auth0User] (delete-auth0-user inst))))

(defn auth0-user-query [inst]
  #?(:clj
     (case (cn/instance-name inst)
       [:Kernel :Auth0User] (get-user-info inst))))
     
(def ^:private resolver-fns
  {:upsert {:handler auth0-user-upsert}
   :delete {:handler auth0-user-delete}
   :query {:handler auth0-user-query}})

(defn make
  "Create and return an auth0-user resolver"
  [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
