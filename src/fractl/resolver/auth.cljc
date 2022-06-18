(ns fractl.resolver.auth
  "Authentication management"
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.store :as s]
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
          @db (cn/id-attr inst)
          inst-with-issued))
       (assoc inst :Issued (dt/as-string now)))))

(defn- auth-kernel-auth0-auth-upsert [inst]
  #?(:clj
     (let [now (dt/now-raw)
           request (:RequestObject inst)
           username (:UserName request)
           passwd (:Password request)
           scope (:AuthScope request)
           auth-api (Auth0AuthUtil/createAuthAPI (:ClientID request)
                                                 (:ClientSecret request)
                                                 (:AuthDomain request)
                                                 true)
           
           token-holder (Auth0AuthUtil/passwordLogin auth-api username passwd scope)]
       (if token-holder
         (let [access-token (.getAccessToken token-holder)
               id-token (.getIdToken token-holder)
               expires-in (.getExpiresIn token-holder)
               token-type (.getTokenType token-holder)
               inst-with-attrs (assoc inst
                                      :Issued (dt/as-string now)
                                      :ExpirySeconds expires-in
                                      :AccessToken access-token)               
               auth-response {:Kernel/AuthResponse
                              {:AccessToken access-token
                               :IdToken id-token
                               :TokenType token-type
                               :Owner username
                               :Issued (dt/as-string now)
                               :ExpirySeconds expires-in}}]
                                      
               (u/call-and-set
                db
                #(assoc
                  @db (cn/id-attr inst)
                  inst-with-attrs))
               (s/upsert-instance
                (s/get-default-store)
                 ":Kernel/AuthResponse"
                 (cn/make-instance auth-response))
               auth-response)))))
           
(defn- auth-kernel-oauth2-upsert [inst]
  #?(:clj
     (let [now (dt/now-raw)
           request (:RequestObject inst)
           auth-api (Auth0AuthUtil/createAuthAPI (:ClientID request)
                                                 (:ClientSecret request)
                                                 (:AuthDomain request)
                                                 true)
           authorize-url (Auth0AuthUtil/authorizeUrl auth-api
                                                     (:CallbackURL request)
                                                     (:AuthScope request))        
           inst-with-generated
           (assoc inst :Generated now :AuthorizeURL authorize-url)]
       (u/call-and-set
        db
        #(assoc
          @db (cn/id-attr inst)
          inst-with-generated))
       (assoc inst :Generated (dt/as-string now) :AuthorizeURL authorize-url))))

(defn auth-upsert [inst]
  (case (keyword (:AuthType inst))
    :Database (auth-kernel-auth-upsert inst)
    :OAuth2Request (auth-kernel-oauth2-upsert inst)
    :Auth0Database (auth-kernel-auth0-auth-upsert inst)
    (u/throw-ex (str "invalid AuthType - " (:AuthType inst)))))

(defn- auth-delete [inst]
  (let [id (cn/id-attr inst)]
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
      (do (auth-delete {cn/id-attr id})
          nil))))

(def ^:private resolver-fns
  {:upsert {:handler auth-upsert}
   :delete {:handler auth-delete}
   :query {:handler auth-query}})

(defn make
  "Create and return an auth resolver"
  [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
