(ns fractl.test.policy
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.resolver.policy :as rp]
            [fractl.resolver.auth :as auth]
            [fractl.policy.logging :as pl]
            [fractl.lang.datetime :as dt]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(defn- mkauth [group]
  (auth/auth-upsert
   (cn/make-instance
    {:Kernel/Authentication
     {:Owner {:Group group}}})))

(defn- ctx [auth]
  {:Auth (:Id auth)})

(deftest event-rbac-policies
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :EVP
     (entity {:EVP/User
              {:UserName :Kernel/String
               :Password :Kernel/Password
               :Group {:oneof ["admin" "customer" "sales"]}}})
     (let [admin-auth (mkauth "admin")
           sales-auth (mkauth "sales")
           admin (cn/make-instance
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
                     :Rule [:q#
                            [:when
                             [:= "admin" :EventContext.Auth.Owner.Group]]]}})
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
                    :EventContext (ctx admin-auth)}}))]
         (is (cn/instance-of? :EVP/User r2))
         (tu/is-error
          #(tu/first-result
            (cn/make-instance
             {:EVP/Upsert_User
              {:Instance user
               :EventContext (ctx sales-auth)}}))))))))

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
                     :Rule [:q#
                            [[:Upsert]
                             [:when
                              [:= "admin"
                               :EventContext.Auth.Owner.Group]]]]}})
           r2 (tu/first-result
               (cn/make-instance
                {:Kernel/Upsert_Policy
                 {:Instance policy}}))]
       (is (cn/instance-of? :ENP/User r1))
       (is (cn/instance-of? :Kernel/Policy r2))
       (is (:Id r2))
       (is (= :Default (keyword (:InterceptStage r2))))
       (let [admin-auth (mkauth "admin")
             sales-auth (mkauth "sales")
             user (cn/make-instance
                   {:ENP/User
                    {:UserName "akc"
                     :Password "998112kl"
                     :Group "customer"}})
             r2 (tu/first-result
                 (cn/make-instance
                  {:ENP/Upsert_User
                   {:Instance user
                    :EventContext (ctx admin-auth)}}))]
         (is (cn/instance-of? :ENP/User r2))
         (tu/is-error
          #(tu/first-result
            (cn/make-instance
             {:ENP/Upsert_User
              {:Instance user
               :EventContext (ctx sales-auth)}})))
         (let [attrs {:UserId (:Id r2)
                      :Group "sales"
                      :EventContext (ctx admin-auth)}
               evt (cn/make-instance
                    {:ENP/ChangeGroup attrs})
               r3 (tu/first-result evt)]
           (is (cn/instance-of? :ENP/User r3))
           (is (= "sales" (:Group r3)))
           (tu/is-error
            #(tu/first-result
              (cn/make-instance
               {:ENP/ChangeGroup
                (assoc attrs :EventContext (ctx sales-auth))})))))))))

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
                     :Resource [:LP/Upsert_User :LP/Lookup_User]
                     :Rule [:q#
                            {:Disable :INFO
                             :PagerThreshold
                             {:WARN
                              {:count 5
                               :duration-minutes 10}
                              :ERROR
                              {:count 3
                               :duration-minutes 5}}}]}})}}))
           p2 (tu/first-result
               (cn/make-instance
                {:Kernel/Upsert_Policy
                 {:Instance
                  (cn/make-instance
                   {:Kernel/Policy
                    {:Intercept "Logging"
                     :Resource ["LP/User"]
                     :Rule [:q#
                            [[:Upsert :Lookup]
                             {:HideAttributes
                              [:LP/User.Password
                               :LP/Upsert_User.Instance.Password]}]]}})}}))]
       (is (cn/instance-of? :Kernel/Policy p1))
       (is (cn/instance-of? :Kernel/Policy p2))
       (is (= [{:Disable [:INFO], :PagerThreshold
                {:WARN {:count 5, :duration-minutes 10},
                 :ERROR {:count 3, :duration-minutes 5}}}
               {:HideAttributes [[[:LP :User] [:Password]]
                                 [[:LP :Upsert_User] [:Instance :Password]]]}]
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
                :Rule [:q#
                       [[:Upsert :Lookup]
                        {:InvalidPolicyKey 123}]]}})}})))
       (let [evt (cn/make-instance
                  {:LP/Upsert_User
                   {:Instance
                    (cn/make-instance
                     {:LP/User
                      {:UserName "abc"
                       :Password "abc123"
                       :DOB "2000-03-20T00:00:00.000000"}})}})
             rules (pl/rules evt)]
         (is (= [[[:LP :User] [:Password]] [[:LP :Upsert_User] [:Instance :Password]]]
                (pl/hidden-attributes rules)))
         (is (= #{:WARN :ERROR :DEBUG} (pl/log-levels rules))))))))
