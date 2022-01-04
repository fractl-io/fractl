(ns fractl.resolver.auth0-user
  "User signup/profile mangement with auth0"
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.component :as cn]
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
         
(defn auth0-user-delete [inst]
  #?(:clj (u/throw-ex (str "No user delete API defined on auth0"))))

(defn auth0-user-query [inst]
  #?(:clj (u/throw-ex (str "Auth0 user query API via ID lookup not defined"))))

(def ^:private resolver-fns
  {:upsert {:handler auth0-user-upsert}
   :delete {:handler auth0-user-delete}
   :query {:handler auth0-user-query}})

(defn make
  "Create and return an auth0-user resolver"
  [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
