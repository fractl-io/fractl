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

(deftest auth0-auth
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :Auth0TestAuth
     (entity {:Auth0TestAuth/AuthRequest
              {:ClientID :Kernel/String
               :ClientSecret :Kernel/String
               :AuthDomain :Kernel/String
               :AuthScope {:listof :Kernel/String}
               :CallbackURL :Kernel/String}})

     (event :Auth0TestAuth/Login {:ClientID :Kernel/String
                                  :ClientSecret :Kernel/String
                                  :AuthDomain :Kernel/String
                                  :AuthScope {:listof :Kernel/String}
                                  :CallbackURL :Kernel/String})
     (dataflow
      :Auth0TestAuth/LoginRequest
      {:Auth0TestAuth/AuthRequest
       {:ClientID? :Auth0TestAuth/LoginRequest.ClientID}}
      [:match :Auth0TestAuth/AuthRequest.ClientSecret
       :Auth0TestAuth/LoginRequest.ClientSecret {:Kernel/OAuth2Request
                                                 {:ClientID :Auth0TestAuth/AuthRequest.ClientID
                                                  :ClientSecret  :Auth0TestAuth/AuthRequest.ClientSecret
                                                  :AuthDomain :Auth0TestAuth/AuthRequest.AuthDomain
                                                  :AuthScope :Auth0TestAuth/AuthRequest.AuthScope
                                                  :CallbackURL :Auth0TestAuth/AuthRequest.CallbackURL}}])

     (let [clientId "xyz123"
           clientSecret "xyzsecretsauce"
           authDomain "client.us.auth0.com"
           authScope ["openid" "profile" "email"]
           callbackURL "http://localhost"
           authReq (tu/first-result
                    (cn/make-instance
                     {:Auth0TestAuth/Upsert_AuthRequest
                      {:Instance
                       {:Auth0TestAuth/AuthRequest
                        {:ClientID clientId
                         :ClientSecret clientSecret
                         :AuthDomain authDomain
                         :AuthScope authScope
                         :CallbackURL callbackURL}}}}))
           authLogin (tu/first-result
                      (cn/make-instance
                       {:Auth0TestAuth/LoginRequest
                        {:ClientID clientId
                         :ClientSecret clientSecret}}))
           ]
       (is (cn/instance-of? :Kernel/OAuth2Request authLogin))))))
