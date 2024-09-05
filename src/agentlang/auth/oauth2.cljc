(ns agentlang.auth.oauth2
  (:require [agentlang.util :as u])
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

(def ^:private type-tag :-*-tag-*-)

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
          {type-tag :oauth2
           :auth-url authorization-url
           :state state
           :service service}))))   
  ([api options] (initialize api options nil)))

(defn oauth2? [obj]
  (and (map? obj)
       (= :oauth2 (type-tag obj))))

(def authorization-url :auth-url)

#?(:clj
   (defn- as-token-map [^OAuth2AccessToken access-token]
     {:raw-token (.getRawResponse access-token)
      :token access-token}))

;; Step 2: Use the code from the server to get an access token.
(defn enable-access [auth-obj code secret]
  (when-not (= secret (:state auth-obj))
    (u/throw-ex (str "State mismatch, expected: " (:state auth-obj), " got: " secret)))
  #?(:clj
     (let [^OAuth20Service service (:service auth-obj)
           ^OAuth2AccessToken access-token (.getAccessToken service code)]
       (merge auth-obj (as-token-map access-token)))))

(defn access-enabled? [obj]
  (and (oauth2? obj)
       (:token obj)))

(defn refresh-token [api-obj]
  #?(:clj
     (let [^OAuth2AccessToken access-token (:token api-obj)
           ^OAuth20Service service (:service api-obj)
           ^OAuth2AccessToken new-access-token 
           (.refreshAccessToken service (.getRefreshToken access-token))]
       (merge api-obj (as-token-map new-access-token)))))

(defn release [api-obj]
  (when (oauth2? api-obj)
    #?(:clj
       (let [^OAuth20Service service (:service api-obj)]
         (.close service)
         (dissoc api-obj :service type-tag)))
    true))

(defn- http-request [verb api-obj url headers body]
  (let [^OAuth20Service service (:service api-obj)
        ^OAuth2AccessToken access-token (:token api-obj)
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
  ([api-obj url headers body]
   (http-request Verb/POST api-obj url headers body))
  ([api-obj url body] (http-post api-obj url nil body)))

(defn http-get
  ([api-obj url headers]
   (http-request Verb/GET api-obj url headers nil))
  ([api-obj url] (http-get api-obj url nil)))

(defn http-delete
  ([api-obj url headers]
   (http-request Verb/DELETE api-obj url headers nil))
  ([api-obj url] (http-delete api-obj url nil)))
