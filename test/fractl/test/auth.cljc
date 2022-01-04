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
       (is (= (:Id user) (:Id (:Owner auth))))
       (is (dt/parse-date-time (:Issued auth)))
       (is (= 5000 (:ExpirySeconds auth)))
       (let [user2 (tu/first-result
                    (cn/make-instance
                     {:PasswordAuth/Upsert_User
                      {:Instance
                       {:PasswordAuth/User
                        {:UserName "hhh"
                         :Password "34sss"}}
                       :EventContext {:Auth (:Id auth)}}}))]
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
     (entity
      {:Auth0TestAuth/AuthRequest
       {:ClientID :Kernel/String
        :ClientSecret :Kernel/String
        :AuthDomain :Kernel/String
        :AuthScope :Kernel/String
        :CallbackURL :Kernel/String}})

     (event
      :Auth0TestAuth/Login
      {:ClientID :Kernel/String
       :ClientSecret :Kernel/String
       :AuthDomain :Kernel/String
       :AuthScope :Kernel/String
       :CallbackURL :Kernel/String})

     (dataflow
      :Auth0TestAuth/LoginRequest
      {:Auth0TestAuth/AuthRequest
       {:ClientID? :Auth0TestAuth/LoginRequest.ClientID}}
      [:match :Auth0TestAuth/AuthRequest.ClientSecret
       :Auth0TestAuth/LoginRequest.ClientSecret
       {:Kernel/Authentication
        {:AuthType "OAuth2Request"
         :RequestObject :Auth0TestAuth/AuthRequest
         :ExpirySeconds 86400}}])
     (let [client-id "xyz123"
           client-secret "xyzsecretsauce"
           auth-domain "client.us.auth0.com"
           auth-scope "openid profile email"
           callback-url "http://localhost"
           auth-req (tu/first-result
                     (cn/make-instance
                      {:Auth0TestAuth/Upsert_AuthRequest
                       {:Instance
                        {:Auth0TestAuth/AuthRequest
                         {:ClientID client-id
                          :ClientSecret client-secret
                          :AuthDomain auth-domain
                          :AuthScope auth-scope
                          :CallbackURL callback-url}}}}))
           auth-login (tu/first-result
                       (cn/make-instance
                        {:Auth0TestAuth/LoginRequest
                         {:ClientID client-id
                          :ClientSecret client-secret}}))]
       (is (cn/instance-of? :Kernel/Authentication auth-login))
       (is (dt/parse-date-time (:Generated auth-login)))
       (is (not-empty (:AuthorizeURL auth-login)))))))


(deftest auth0-db-auth
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :Auth0TestDbAuth
     (entity {:Auth0TestDbAuth/AuthRequest
              {:ClientID :Kernel/String
               :ClientSecret :Kernel/String
               :AuthDomain :Kernel/String
               :AuthScope :Kernel/String
               :UserName :Kernel/String
               :Password :Kernel/String}})

     (event :Auth0TestDbAuth/Login {:ClientID :Kernel/String
                                    :ClientSecret :Kernel/String
                                    :AuthDomain :Kernel/String
                                    :AuthScope :Kernel/String
                                    :UserName :Kernel/String
                                    :Password :Kernel/String})

     (dataflow
      :Auth0TestDbAuth/LoginRequest
      {:Auth0TestDbAuth/AuthRequest
       {:ClientID? :Auth0TestDbAuth/LoginRequest.ClientID}}
      [:match :Auth0TestDbAuth/AuthRequest.ClientSecret
       :Auth0TestDbAuth/LoginRequest.ClientSecret {:Kernel/Authentication
                                                   {:AuthType "Auth0Database"
                                                    :RequestObject :Auth0TestDbAuth/AuthRequest}}])

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
                      {:Auth0TestDbAuth/Upsert_AuthRequest
                       {:Instance
                        {:Auth0TestDbAuth/AuthRequest
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
