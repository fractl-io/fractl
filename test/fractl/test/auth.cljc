(ns fractl.test.auth
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.evaluator :as e]
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.lang.datetime :as dt]
            [fractl.util.hash :as h]
            [clojure.string :as cs]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest password-auth
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :PasswordAuth
     (entity {:PasswordAuth/User
              {:UserName {:type :Kernel/String
                          :indexed true}
               :Password :Kernel/Password}})

     (event :PasswordAuth/Login {:UserName :Kernel/String
                                 :Password :Kernel/Password})     
     (dataflow
      :PasswordAuth/Login
      {:PasswordAuth/User
       {:UserName? :PasswordAuth/Login.UserName}}
      [:match :PasswordAuth/User.Password
       :PasswordAuth/Login.Password {:Kernel/Authentication
                                     {:Owner :PasswordAuth/User
                                      :ExpirySeconds 5000}}])
     (let [uname "manju"
           pswd "uber123"
           user (tu/first-result
                 (cn/make-instance
                  {:PasswordAuth/Upsert_User
                   {:Instance
                    {:PasswordAuth/User
                     {:UserName uname
                      :Password pswd}}}}))
           auth (tu/first-result
                 (cn/make-instance
                  {:PasswordAuth/Login
                   {:UserName uname
                    :Password pswd}}))
           ]
       (is (cn/instance-of? :Kernel/Authentication auth))
       (is (= (cn/id-attr user) (cn/id-attr (:Owner auth))))
       (is (dt/parse-date-time (:Issued auth)))
       (is (= 5000 (:ExpirySeconds auth)))
       (let [user2 (tu/first-result
                    (cn/make-instance
                     {:PasswordAuth/Upsert_User
                      {:Instance
                       {:PasswordAuth/User
                        {:UserName "hhh"
                         :Password "34sss"}}
                       :EventContext {:Auth (cn/id-attr auth)}}}))]
         (is (cn/instance-of? :PasswordAuth/User user2)))
       (is (false? (tu/fresult
                    (e/eval-all-dataflows
                     (cn/make-instance
                      {:PasswordAuth/Login
                       {:UserName uname
                        :Password "aaaa"}})))))))))

(deftest auth0-oauth2
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :Auth0TestAuth

     (event
      :Auth0TestAuth/Login
      {:ClientID :Kernel/String
       :ClientSecret :Kernel/String
       :AuthDomain :Kernel/String
       :AuthScope :Kernel/String
       :CallbackURL :Kernel/String})

     (dataflow
      :Auth0TestAuth/LoginRequest
      {:Kernel/OAuthAnyRequest
       {:ClientID? :Auth0TestAuth/LoginRequest.ClientID}}
      [:match :Kernel/OAuthAnyRequest.ClientSecret
       :Auth0TestAuth/LoginRequest.ClientSecret
       {:Kernel/Authentication
        {:AuthType "OAuth2Request"
         :RequestObject :Kernel/OAuthAnyRequest
         :ExpirySeconds 86400}}])
     (let [client-id "Zpd3u7saV3Y7tebdzJ1Vo0eFALWyxMnR"
           client-secret "DSiQSiVT7Sd0RJwxdQ4gCfjLUA495PjlVNKhkgB6yFgpH2rgt9kpRbxJLPOcAaXH"
           auth-domain "fractl.us.auth0.com"
           auth-scope "openid profile email"
           callback-url "http://localhost:8080/_callback/?tag=auth_id"
           auth-req (tu/first-result
                     (cn/make-instance
                      {:Kernel/Upsert_OAuthAnyRequest
                       {:Instance
                        {:Kernel/OAuthAnyRequest
                         {:ClientID client-id
                          :ClientSecret client-secret
                          :AuthDomain auth-domain
                          :AuthScope auth-scope
                          :CallbackURL callback-url}}}}))
           auth-login (tu/first-result
                       (cn/make-instance
                        {:Auth0TestAuth/LoginRequest
                         {:ClientID client-id
                          :ClientSecret client-secret}}))
           auth-req-id (cn/id-attr auth-req)
           ;; this is the authorize-url we should present to the user
           ;; when we build the UI part of the auth flow.
           authorize-url-mod (cs/replace  (:AuthorizeURL auth-login) #"auth_id" auth-req-id)]
       
       (is (cn/instance-of? :Kernel/Authentication auth-login))
       (is (dt/parse-date-time (:Generated auth-login)))
       (is (not-empty authorize-url-mod))))))


(deftest auth0-db-auth
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :Auth0TestDbAuth
     (event :Auth0TestDbAuth/Login {:ClientID :Kernel/String
                                    :ClientSecret :Kernel/String
                                    :AuthDomain :Kernel/String
                                    :AuthScope :Kernel/String
                                    :UserName :Kernel/String
                                    :Password :Kernel/String})

     (dataflow
      :Auth0TestDbAuth/LoginRequest
      {:Kernel/OAuthAnyRequest
       {:ClientID? :Auth0TestDbAuth/LoginRequest.ClientID}}
      [:match :Kernel/OAuthAnyRequest.ClientSecret
       :Auth0TestDbAuth/LoginRequest.ClientSecret {:Kernel/Authentication
                                                   {:AuthType "Auth0Database"
                                                    :RequestObject :Kernel/OAuthAnyRequest}}])

     ;; this is a test that actually logs in a test user via the
     ;; auth0 database authentication API - there is no way to "mock" this
     (let [client-id "Zpd3u7saV3Y7tebdzJ1Vo0eFALWyxMnR"
           client-secret "DSiQSiVT7Sd0RJwxdQ4gCfjLUA495PjlVNKhkgB6yFgpH2rgt9kpRbxJLPOcAaXH"
           auth-domain "fractl.us.auth0.com"
           auth-scope "openid profile email"
           username "test.user@ventur8.io"
           passwd "P@s$w0rd123"
           auth-req (tu/first-result
                     (cn/make-instance
                      {:Kernel/Upsert_OAuthAnyRequest
                       {:Instance
                        {:Kernel/OAuthAnyRequest
                         {:ClientID client-id
                          :ClientSecret client-secret
                          :AuthDomain auth-domain
                          :AuthScope auth-scope
                          :UserName username
                          :Password passwd}}}}))
           auth-login (tu/first-result
                       (cn/make-instance
                        {:Auth0TestDbAuth/LoginRequest
                         {:ClientID client-id
                          :ClientSecret client-secret}}))
           auth-response (:Kernel/AuthResponse auth-login)
           ]
       (is (cn/instance-of? :Kernel/Authentication auth-login))
       (is (dt/parse-date-time (:Issued auth-response)))       
       (is (> (:ExpirySeconds auth-response) 0))
       (is (= (:TokenType auth-response) "Bearer"))
       (is (= (:Owner auth-response) username))
       (is (not-empty (:AccessToken auth-response)))
       (is (not-empty (:IdToken auth-response)))))))
