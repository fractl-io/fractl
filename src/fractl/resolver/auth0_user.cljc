(ns fractl.resolver.auth0-user
  "User signup/profile mangement with auth0"
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.component :as cn]
            [fractl.lang.datetime :as dt])
  #?(:clj (:import [fractl.auth.auth0 Auth0AuthUtil])))

(def ^:private db (u/make-cell {}))

(defn auth0-user-upsert [inst]
  #?(:clj
;;     (println "auth0-user-upsert")
     (let [username (:UserName inst)
           passwd (:Password inst)
           email (:Email inst)
           fields (:Fields inst)
           request (:RequestObject inst)
           authApi (Auth0AuthUtil/createAuthAPI (:ClientID request)
                                                (:ClientSecret request)
                                                (:AuthDomain request)
                                                true)
           createdUser (Auth0AuthUtil/signupUser authApi username email passwd fields)]
       (if createdUser
         (let [userId (.getUserId createdUser)
               inst-with-attrs (assoc inst :UserId userId)]
           (u/call-and-set
            db
            #(assoc
              @db (:Id inst)
              inst-with-attrs)))))))
         
(defn auth0-user-delete [inst]
  #?(:clj (println "auth0-user-delete")))

(defn auth0-user-query [inst]
  #?(:clj (println "auth0-user-query")))

(def ^:private resolver-fns
  {:upsert {:handler auth0-user-upsert}
   :delete {:handler auth0-user-delete}
   :query {:handler auth0-user-query}})

(defn make
  "Create and return an auth0-user resolver"
  [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
