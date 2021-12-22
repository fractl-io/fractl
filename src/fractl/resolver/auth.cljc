(ns fractl.resolver.auth
  "Authentication management"
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.component :as cn]
            [fractl.lang.datetime :as dt])
  #?(:clj (:import [fractl.auth.auth0 Auth0AuthUtil])))

(def ^:private db (u/make-cell {}))

(defn- auth-kernel-auth-upsert [inst]
  #?(:clj
     (let [now (dt/now-raw)
           inst-with-issued
           (assoc inst :Issued now)]
       (u/call-and-set
        db
        #(assoc
          @db (:Id inst)
          inst-with-issued))
       (assoc inst :Issued (dt/as-string now)))))

(defn- auth-kernel-auth0-auth-upsert [inst]
  #?(:clj
     (let [now (dt/now-raw)
           request (:RequestObject inst)
           username (:UserName request)
           passwd (:Password request)
           scope (:AuthScope request)
           authApi (Auth0AuthUtil/createAuthAPI (:ClientID request)
                                                (:ClientSecret request)
                                                (:AuthDomain request)
                                                true)
           
           tokenHolder (Auth0AuthUtil/passwordLogin authApi username passwd scope)]
       (if tokenHolder
         (let [accessToken (.getAccessToken tokenHolder)
               idToken (.getIdToken tokenHolder)
               expiresIn (.getExpiresIn tokenHolder)
               tokenType (.getTokenType tokenHolder)
               inst-with-attrs (assoc inst
                                      :Issued (dt/as-string now)
                                      :ExpirySeconds expiresIn
                                      :AccessToken accessToken)               
               auth-response {:Kernel/AuthResponse
                              {:AccessToken accessToken
                               :IdToken idToken
                               :TokenType tokenType
                               :Owner username
                               :Issued (dt/as-string now)
                               :ExpirySeconds expiresIn}}]
                                      
               (u/call-and-set
                db
                #(assoc
                  @db (:Id inst)
                  inst-with-attrs))
               auth-response)))))
           
(defn- auth-kernel-oauth2-upsert [inst]
  #?(:clj
     (let [now (dt/now-raw)
           request (:RequestObject inst)
           authApi (Auth0AuthUtil/createAuthAPI (:ClientID request)
                                                (:ClientSecret request)
                                                (:AuthDomain request)
                                                true)
           authorizeUrl (Auth0AuthUtil/authorizeUrl authApi
                                                    (:CallbackURL request)
                                                    (:AuthScope request))        
           inst-with-generated
           (assoc inst :Generated now :AuthorizeURL authorizeUrl)]
       (u/call-and-set
        db
        #(assoc
          @db (:Id inst)
          inst-with-generated))
       (assoc inst :Generated (dt/as-string now) :AuthorizeURL authorizeUrl))))

(defn auth-upsert [inst]
  (cond
    (= (:AuthType inst) "Database") (auth-kernel-auth-upsert inst)
    (= (:AuthType inst) "OAuth2Request") (auth-kernel-oauth2-upsert inst)
    (= (:AuthType inst) "Auth0Database") (auth-kernel-auth0-auth-upsert inst)))

(defn- auth-delete [inst]
  (let [id (:Id inst)]
    (u/call-and-set
     db
     #(dissoc @db id))
    id))

(defn auth-query [id]
  (when-let [inst (get @db id)]
    (if (> (:ExpirySeconds inst)
           (dt/difference-in-seconds
            (:Issued inst) (dt/now-raw)))
      inst
      (do (auth-delete {:Id id})
          nil))))

(def ^:private resolver-fns
  {:upsert {:handler auth-upsert}
   :delete {:handler auth-delete}
   :query {:handler auth-query}})

(defn make
  "Create and return an auth resolver"
  [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
