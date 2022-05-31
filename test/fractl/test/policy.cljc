(ns fractl.test.policy
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.policy :as policy]
            [fractl.rbac.role-assignment :as ra]
            [fractl.lang.datetime :as dt]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-506-policy-management
  (#?(:clj do
      :cljs cljs.core.async/go)
   (let [all-actions [:Upsert :Delete :Lookup]]
     (defcomponent :I556PP
       (entity
        :I556PP/E1
        {:X :Kernel/Int})
       (entity
        :I556PP/E2
        {:Y :Kernel/Int})
       (dataflow
        :I556PP/DefPolicies
        {:Kernel/Role
         {:Name "role-1"} :as :R1}
        {:Kernel/Policy
         {:Intercept "RBAC"
          :Resource "I556PP/E1"
          :Spec [:q#
                 {:actions all-actions
                  :role :R1}]}}
        {:Kernel/Role
         {:Name "role-2"} :as :R2}
        {:Kernel/Policy
         {:Intercept "RBAC"
          :Resource "I556PP/E1"
          :Spec [:q#
                 {:actions [:Lookup]
                  :role :R2}]}}
        {:Kernel/Policy
         {:Intercept "RBAC"
          :Resource "I556PP/E2"
          :Spec [:q#
                 {:actions all-actions
                  :role :R1}]}})))
   (let [r1 (tu/fresult
             (e/eval-all-dataflows
              (cn/make-instance
               {:I556PP/DefPolicies {}})))
         r2 (policy/lookup-policies :RBAC :I556PP/E1)]
     (is (cn/instance-of? :Kernel/Policy (first r1)))
     (is (= 2 (count r2)))
     (doseq [r r2]
       (is (= :I556PP/E1 (:Resource r)))))))

(deftest issue-506-role-management
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I506RM
     (entity
      :I506RM/User
      {:Username :Kernel/String})
     (record
      :I506RM/Result
      {:Data :Kernel/Any})
     (dataflow
      :I506RM/MakeUsers
      {:I506RM/User
       {:Username "abc"} :as :U1}
      {:I506RM/User
       {:Username "xyz"} :as :U2}
      {:I506RM/Result
       {:Data [:U1 :U2]}})
     (dataflow
      :I506RM/AssignRoles
      {:Kernel/Role
       {:Name "supervisor"} :as :R1}
      {:Kernel/Role
       {:Name "officer"} :as :R2}
      {:I506RM/User {:Username? "abc"} :as [:U1 :& :_]}
      {:I506RM/User {:Username? "xyz"} :as [:U2 :& :_]}
      {:Kernel/RoleAssignment
       {:Role :R1
        :Assignee :U1} :as :RA1}
      {:Kernel/RoleAssignment
       {:Role :R2
        :Assignee :U2} :as :RA2}
      {:I506RM/Result
       {:Data [:RA1 :RA2]}}))
   (let [r1 (tu/first-result
             {:I506RM/MakeUsers {}})
         r2 (tu/first-result
             {:I506RM/AssignRoles {}})
         users (:Data r1)
         u1 (get-in (first users) [:transition :to])
         u2 (get-in (second users) [:transition :to])]
     (is (= "supervisor" (first (ra/find-assigned-roles (:Id u1)))))
     (is (= "officer" (first (ra/find-assigned-roles (:Id u2))))))))
