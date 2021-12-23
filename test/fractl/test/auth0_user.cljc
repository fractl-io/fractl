(ns fractl.test.auth0-user
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.evaluator :as e]
            [fractl.component :as cn]
            [cheshire.core :as json]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.lang.datetime :as dt]
            [fractl.util.hash :as h]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest auth0-db-create-user
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :Auth0TestDbSignupUser
     (entity
      {:Auth0TestDbSignupUser/SignupRequest
       {:ClientID :Kernel/String
        :ClientSecret :Kernel/String
        :AuthDomain :Kernel/String
        :Email :Kernel/String
        :UserName :Kernel/String
        :Password :Kernel/String}})

     (event
      :Auth0TestDbSignupUser/SignUp
      {:ClientID :Kernel/String
       :ClientSecret :Kernel/String
       :AuthDomain :Kernel/String
       :Email :Kernel/String
       :UserName :Kernel/String
       :Password :Kernel/String})

     (dataflow
      :Auth0TestDbSignupUser/SignupUserRequest
      {:Auth0TestDbSignupUser/SignupRequest
       {:ClientID? :Auth0TestDbSignupUser/SignupUserRequest.ClientID}}
      [:match :Auth0TestDbSignupUser/SignupRequest.ClientSecret
       :Auth0TestDbSignupUser/SignupUserRequest.ClientSecret
       {:Kernel/Auth0User
        {:RequestObject :Auth0TestDbSignupUser/SignupRequest
         :UserName :Auth0TestDbSignupUser/SignupRequest.UserName
         :Password :Auth0TestDbSignupUser/SignupRequest.Password
         :Email :Auth0TestDbSignupUser/SignupRequest.Email}}])

     (let [client-id "Zpd3u7saV3Y7tebdzJ1Vo0eFALWyxMnR"
           client-secret "DSiQSiVT7Sd0RJwxdQ4gCfjLUA495PjlVNKhkgB6yFgpH2rgt9kpRbxJLPOcAaXH"
           auth-domain "fractl.us.auth0.com"
           email "random@ventur8.io"
           username "fractl-test-user"
           passwd "P@s$w0rd123"
           signup-req (tu/first-result
                       (cn/make-instance
                        {:Auth0TestDbSignupUser/Upsert_SignupRequest
                         {:Instance
                          {:Auth0TestDbSignupUser/SignupRequest
                           {:ClientID client-id
                            :ClientSecret client-secret
                            :AuthDomain auth-domain
                            :Email email
                            :UserName username
                            :Password passwd}}}}))

           signup-user-resp (tu/first-result
                             (cn/make-instance
                              {:Auth0TestDbSignupUser/SignupUserRequest
                               {:ClientID client-id
                                :ClientSecret client-secret}}))]
       ;;       (println "signup response" signup-user-resp)
       ;;       (is (cn/instance-of? :Kernel/Auth0User signup-user-resp))))))
       ))))

