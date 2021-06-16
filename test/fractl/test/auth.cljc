(ns fractl.test.auth
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.evaluator :as e]
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.lang.datetime :as dt]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest simple-auth
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :SimpleAuth
     (entity {:SimpleAuth/User
              {:UserName {:type :Kernel/String
                          :indexed true}
               :Password :Kernel/Password}})
     (dataflow
      :SimpleAuth/Login
      {:SimpleAuth/User
       {:UserName? :SimpleAuth/Login.UserName}}
      [:match :SimpleAuth/User.Password
       :SimpleAuth/Login.Password {:Kernel/Authentication
                                   {:Owner :SimpleAuth/User.Id
                                    :OwnerType "SimpleAuth/User"
                                    :ExpirySeconds 5000}}])
     (let [uname "abc"
           pswd "gg677"
           user (tu/first-result
                 (cn/make-instance
                  {:SimpleAuth/Upsert_User
                   {:Instance
                    {:SimpleAuth/User
                     {:UserName uname
                      :Password pswd}}}}))
           auth (tu/first-result
                 (cn/make-instance
                  {:SimpleAuth/Login
                   {:UserName uname
                    :Password pswd}}))]
       (is (cn/instance-of? :Kernel/Authentication auth))
       (is (= (:Id user) (:Owner auth)))
       (is (= :SimpleAuth/User (keyword (:OwnerType auth))))
       (is (dt/parse-date-time (:Issued auth)))
       (is (= 5000 (:ExpirySeconds auth)))
       (is (false? (tu/fresult
                    (e/eval-all-dataflows
                     (cn/make-instance
                      {:SimpleAuth/Login
                       {:UserName uname
                        :Password "aaaa"}})))))))))
