(ns fractl.test.policy
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.resolver.policy :as rp]
            [fractl.lang.datetime :as dt]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(defn- mkauth [s]
  {:Auth {:Owner {:Group s}}})

(deftest event-rbac-policies
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
                    {:Intercept "RBAC"
                     :Resource ["EVP/Upsert_User"]
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
                    :EventContext (mkauth "admin")}}))]
         (is (cn/instance-of? :EVP/User r2))
         (tu/is-error
          #(tu/first-result
            (cn/make-instance
             {:EVP/Upsert_User
              {:Instance user
               :EventContext (mkauth "sales")}}))))))))

(deftest entity-rbac-policies
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :ENP
     (entity {:ENP/User
              {:UserName :Kernel/String
               :Password :Kernel/Password
               :Group {:oneof ["admin" "customer" "sales"]}}})
     (event {:ENP/ChangeGroup {:UserId :Kernel/UUID :Group :Kernel/String}})
     (dataflow :ENP/ChangeGroup
               {:ENP/User
                {:Id? :ENP/ChangeGroup.UserId
                 :Group :ENP/ChangeGroup.Group}})
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
                    {:Intercept "RBAC"
                     :Resource ["ENP/User"]
                     :Rule [[:Upsert]
                            [:when
                             [:= "admin"
                              :EventContext.Auth.Owner.Group]]]}})
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
                    :EventContext (mkauth "admin")}}))]
         (is (cn/instance-of? :ENP/User r2))
         (tu/is-error
          #(tu/first-result
            (cn/make-instance
             {:ENP/Upsert_User
              {:Instance user
               :EventContext (mkauth "sales")}})))
         (let [attrs {:UserId (:Id r2)
                      :Group "sales"
                      :EventContext (mkauth "admin")}
               evt (cn/make-instance
                    {:ENP/ChangeGroup attrs})
               r3 (tu/first-result evt)]
           (is (cn/instance-of? :ENP/User r3))
           (is (= "sales" (:Group r3)))
           (tu/is-error
            #(tu/first-result
              (cn/make-instance
               {:ENP/ChangeGroup
                (assoc attrs :EventContext (mkauth "sales"))})))))))))

(deftest logging-policies
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :LP
     (entity {:LP/User {:UserName :Kernel/String
                        :Password :Kernel/Password
                        :DOB :Kernel/DateTime}})
     (let [p1 (tu/first-result
               (cn/make-instance
                {:Kernel/Upsert_Policy
                 {:Instance
                  (cn/make-instance
                   {:Kernel/Policy
                    {:Intercept "Logging"
                     :Resource ["LP/Upsert_User" "LP/Lookup_User"]
                     :Rule {:Disable :INFO
                            :PagerThreshold {:WARN {:count 5
                                                    :duration-minutes 10}
                                             :ERROR {:count 3
                                                     :duration-minutes 5}}}}})}}))
           p2 (tu/first-result
               (cn/make-instance
                {:Kernel/Upsert_Policy
                 {:Instance
                  (cn/make-instance
                   {:Kernel/Policy
                    {:Intercept "Logging"
                     :Resource ["LP/User"]
                     :Rule [[:Upsert :Lookup] {:ExcludeAttributes [:LP/User.DOB]}]}})}}))]
       (is (cn/instance-of? :Kernel/Policy p1))
       (is (cn/instance-of? :Kernel/Policy p2))
       (is (= [{:ExcludeAttributes [:LP/User.DOB]}]
              (rp/logging-eval-rules [:LP :Upsert_User])))
       (tu/is-error
        #(tu/first-result
          (cn/make-instance
           {:Kernel/Upsert_Policy
            {:Instance
             (cn/make-instance
              {:Kernel/Policy
               {:Intercept "Logging"
                :Resource ["LP/User"]
                :Rule [[:Upsert :Lookup] {:InvalidPolicyKey 123}]}})}})))))))
