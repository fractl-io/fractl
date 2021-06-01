(ns fractl.test.policy
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.lang.datetime :as dt]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest basic-policies
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :BPI
     (entity {:BPI/User
              {:UserName :Kernel/String
               :Password :Kernel/Password
               :Group {:oneof ["admin" "customer" "sales"]}}})
     (let [admin (cn/make-instance
                  {:BPI/User
                   {:UserName "admin"
                    :Password "kj6671"
                    :Group "admin"}})
           r1 (tu/first-result
               (cn/make-instance
                {:BPI/Upsert_User
                 {:Instance admin}}))
           policy (cn/make-instance
                   {:Kernel/Policy
                    {:Intercept :RBAC
                     :Resource [:BPI/Upsert_User]
                     :Rule [:allow
                            [:when
                             [:= "admin" :_Context.Auth.Owner.Group]]]}})
           r2 (tu/first-result
               (cn/make-instance
                {:Kernel/Upsert_Policy
                 {:Instance policy}}))]
       (is (cn/instance-of? :BPI/User r1))
       (is (cn/instance-of? :Kernel/Policy r2))
       (is (:Id r2))
       (is (= :Default (keyword (:InterceptStage r2))))))))
