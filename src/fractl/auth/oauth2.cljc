(ns fractl.auth.oauth2
  (:require [fractl.util :as u])
  #?(:clj
     (:import [java.util HashMap]
              [com.github.scribejava.core.builder ServiceBuilder]
              [com.github.scribejava.core.oauth OAuth20Service]
              [com.github.scribejava.apis
               BoxApi20 DiscordApi DropboxApi
               EtsyApi FacebookApi Foursquare2Api GitHubApi
               GoogleApi20 InstagramApi LinkedInApi20
               MicrosoftAzureActiveDirectory20Api
               SalesforceApi SlackApi TwitterApi]
              [com.github.scribejava.core.model
               OAuth2AccessToken OAuthRequest Response Verb])))

(def generic (constantly nil)); TODO: implement bare-bones oauth2 in portable clj.

(def box #?(:clj #(BoxApi20/instance) :cljs generic))
(def discord #?(:clj #(DiscordApi/instance) :cljs generic))
(def dropbox #?(:clj #(DropboxApi/instance) :cljs generic))
(def etsy #?(:clj #(EtsyApi/instance) :cljs generic))
(def facebook #?(:clj #(FacebookApi/instance) :cljs generic))
(def foursquare #?(:clj #(Foursquare2Api/instance) :cljs generic))
(def git-hub #?(:clj #(GitHubApi/instance) :cljs generic))
(def google #?(:clj #(GoogleApi20/instance) :cljs generic))
(def microsoft-azure-active-directory #?(:clj #(MicrosoftAzureActiveDirectory20Api/instance) :cljs generic))
(def salesforce #?(:clj #(SalesforceApi/instance) :cljs generic))
(def slack #?(:clj #(SlackApi/instance) :cljs generic))
(def twitter #?(:clj #(TwitterApi/instance) :cljs generic))

;; Step 1: Initialize an oauth2 client and get the authorization URL.
(defn initialize
  ([api {client-id :client-id client-secret :client-secret
         scope :scope callback :callback} additional-params]
   #?(:clj
      (let [state (str "secret" (rand-int 999999))
            ^ServiceBuilder builder (ServiceBuilder. client-id)]
        (when scope (.defaultScope builder scope))
        (let [^OAuth20Service service
              (-> builder
                  (.apiSecret client-secret)
                  (.callback callback)
                  (.build (api)))
              authorization-url
              (if additional-params
                (let [^HashMap hm (HashMap.)]
                  (doseq [[^String k ^String v] additional-params]
                    (.put hm k v))
                  (-> service
                      (.createAuthorizationUrlBuilder)
                      (.state state)
                      (.additionalParams hm)
                      (.build)))
                (.getAuthorizationUrl service state))]
          {:auth-url authorization-url
           :state state
           :service service}))))   
  ([api options] (initialize api options nil)))

(defn oauth2? [obj]
  (and (map? obj)
       (and (= (set (keys obj))
               #{:auth-url :state :service}))))

(def authorization-url :auth-url)

#?(:clj
   (defn- as-token-map [^OAuth2AccessToken access-token]
     {:token (.getRawResponse access-token)
      :handle access-token}))

;; Step 2: Use the code from the server to get an access token.
(defn get-access-token [oauth2 code secret]
  (when-not (= secret (:state oauth2))
    (u/throw-ex (str "State mismatch, expected: " (:state oauth2), " got: " secret)))
  #?(:clj
     (let [^OAuth20Service service (:service oauth2)
           ^OAuth2AccessToken access-token (.getAccessToken service code)]
       (as-token-map access-token))))

(defn token? [obj]
  (and (map? obj)
       (= (set (keys obj)) #{:token :handle})
       (seq (:token obj))))

(defn refresh-token [oauth2 token]
  #?(:clj
     (let [^OAuth2AccessToken access-token (:handle token)
           ^OAuth20Service service (:service oauth2)
           ^OAuth2AccessToken new-access-token 
           (.refreshAccessToken service (.getRefreshToken access-token))]
       (as-token-map new-access-token))))

(defn release [oauth2]
  #?(:clj
     (let [^OAuth20Service service (:service oauth2)]
       (.close service)
       (dissoc oauth2 :service)))
  true)

(defn- http-request [verb oauth2 token url headers body]
  (let [^OAuth20Service service (:service oauth2)
        ^OAuth2AccessToken access-token (:handle token)
        ^OAuthRequest request (OAuthRequest. verb url)]
    (when headers
      (doseq [[^String k ^String v] headers]
        (.addHeader request k v)))
    (when body (.setBody request body))
    (.signRequest service access-token request)
    (let [^Response response (.execute service request)]
      {:status (.getCode response)
       :body (.getBody response)})))

(defn http-post
  ([oauth2 token url headers body]
   (http-request Verb/POST oauth2 token url headers body))
  ([oauth2 token url body] (http-post oauth2 token url nil body)))

(defn http-get
  ([oauth2 token url headers]
   (http-request Verb/GET oauth2 token url headers nil))
  ([oauth2 token url] (http-get oauth2 token url nil)))

(defn http-delete
  ([oauth2 token url headers]
   (http-request Verb/DELETE oauth2 token url headers nil))
  ([oauth2 token url] (http-delete oauth2 token url nil)))
