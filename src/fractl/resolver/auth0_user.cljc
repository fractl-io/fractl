(ns fractl.resolver.auth0-user
  "User signup/profile mangement with auth0"
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.component :as cn]
            [fractl.env :as env]
            [fractl.store :as s]
            [fractl.lang.datetime :as dt])
  #?(:clj (:import [fractl.auth.auth0 Auth0AuthUtil])))

(def ^:private fractl-auth0-domain "fractl.us.auth0.com")

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
               inst-with-attrs (assoc inst :UserId (str "auth0|" user-id)
                                      :UserName user-name
                                      :UserEmail user-email
                                      :UserInfo (assoc nil "user-id" user-id))]
            inst-with-attrs)))))

(defn- delete-auth0-user
  "Delete an auth0 user using the management API"
  [user]
  #?(:clj
     (let [request (:RequestObject user)
           userId (:UserId user)
           mgmt-api (Auth0AuthUtil/createMgmtAPI
                     (:AuthDomain request)
                     (:ApiToken request)
                     true)]
       (Auth0AuthUtil/deleteUser mgmt-api userId)
       true)))

(defn- get-user-info
  "Return the UserInfo for the given auth0-user"
  [user]
  #?(:clj
     ;; login and get token
     (let [username (:UserName user)
           passwd (:Password user)
           request (:RequestObject user)
           scope "openid profile email"
           auth-api (Auth0AuthUtil/createAuthAPI
                     (:ClientID request)
                     (:ClientSecret request)
                     (:AuthDomain request)
                     true)
           token-holder (Auth0AuthUtil/passwordLogin auth-api username passwd scope)]
       (if token-holder
         (Auth0AuthUtil/getUserInfo auth-api (.getAccessToken token-holder))))))

(defn auth0-user-delete [env arg]
  #?(:clj
     (let [store (env/get-store env)
           [entity-name entity-id] (first arg)
           inst (s/lookup-by-id store entity-name entity-id)]
     (case (cn/instance-name inst)
       [:Kernel :Auth0User]
       (if (delete-auth0-user inst)
         (s/delete-by-id store entity-name entity-id) nil)))))

(defn auth0-user-query [env arg]
  #?(:clj
     (let [store (env/get-store env)
           entity-name (first arg)
           entity-id (nth (:where (second arg)) 2)
           inst (s/lookup-by-id store entity-name entity-id)]
       
       (case (cn/instance-name inst)
         [:Kernel :Auth0User]
         (let [user-map (get-user-info inst)
               user-hash (into {} user-map)
               updated-inst (assoc inst :UserInfo user-hash)]
           (s/upsert-instance store entity-name updated-inst)
           nil)))))
               

(defn auth0-user-invoke [method env arg]
  #?(:clj
     (case method
       :query (auth0-user-query env arg)
       :delete (auth0-user-delete env arg))))
     
(def ^:private resolver-fns
  {:upsert {:handler auth0-user-upsert}
   :invoke {:handler auth0-user-invoke}})

(defn make
  "Create and return an auth0-user resolver"
  [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
