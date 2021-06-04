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

(deftest event-policies
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :EVP
     (entity {:EVP/User
              {:UserName :Kernel/String
               :Password :Kernel/Password
               :Group {:oneof ["admin" "customer" "sales"]}}})
     (let [admin (cn/make-instance
                  {:EVP/User
                   {:UserName "admin"
                    :Password "kj6671"
                    :Group "admin"}})
           r1 (tu/first-result
               (cn/make-instance
                {:EVP/Upsert_User
                 {:Instance admin}}))
           policy (cn/make-instance
                   {:Kernel/Policy
                    {:Intercept :RBAC
                     :Resource [:EVP/Upsert_User]
                     :Rule [:when
                            [:= "admin" :EventContext.Auth.Owner.Group]]}})
           r2 (tu/first-result
               (cn/make-instance
                {:Kernel/Upsert_Policy
                 {:Instance policy}}))]
       (is (cn/instance-of? :EVP/User r1))
       (is (cn/instance-of? :Kernel/Policy r2))
       (is (:Id r2))
       (is (= :Default (keyword (:InterceptStage r2))))
       (let [user (cn/make-instance
                   {:EVP/User
                    {:UserName "akc"
                     :Password "998112kl"
                     :Group "customer"}})
             r2 (tu/first-result
                 (cn/make-instance
                  {:EVP/Upsert_User
                   {:Instance user
                    :EventContext {:Auth {:Owner {:Group "admin"}}}}}))]
         (is (cn/instance-of? :EVP/User r2))
         (tu/is-error
          #(tu/first-result
            (cn/make-instance
             {:EVP/Upsert_User
              {:Instance user
               :EventContext {:Auth {:Owner {:Group "sales"}}}}}))))))))

(deftest entity-policies
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :ENP
     (entity {:ENP/User
              {:UserName :Kernel/String
               :Password :Kernel/Password
               :Group {:oneof ["admin" "customer" "sales"]}}})
     (let [admin (cn/make-instance
                  {:ENP/User
                   {:UserName "admin"
                    :Password "kj6671"
                    :Group "admin"}})
           r1 (tu/first-result
               (cn/make-instance
                {:ENP/Upsert_User
                 {:Instance admin}}))
           policy (cn/make-instance
                   {:Kernel/Policy
                    {:Intercept :RBAC
                     :Resource [:ENP/User]
                     :Rule [[:Upsert]
                            [:when
                             [:= "admin" :EventContext.Auth.Owner.Group]]]}})
           r2 (tu/first-result
               (cn/make-instance
                {:Kernel/Upsert_Policy
                 {:Instance policy}}))]
       (is (cn/instance-of? :ENP/User r1))
       (is (cn/instance-of? :Kernel/Policy r2))
       (is (:Id r2))
       (is (= :Default (keyword (:InterceptStage r2))))
       (let [user (cn/make-instance
                   {:ENP/User
                    {:UserName "akc"
                     :Password "998112kl"
                     :Group "customer"}})
             r2 (tu/first-result
                 (cn/make-instance
                  {:ENP/Upsert_User
                   {:Instance user
                    :EventContext {:Auth {:Owner {:Group "admin"}}}}}))]
         (is (cn/instance-of? :ENP/User r2))
         (tu/is-error
          #(tu/first-result
            (cn/make-instance
             {:ENP/Upsert_User
              {:Instance user
               :EventContext {:Auth {:Owner {:Group "sales"}}}}}))))))))
