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
