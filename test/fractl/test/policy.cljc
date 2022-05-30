(ns fractl.test.policy
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.rbac.policy :as policy]
            [fractl.lang.datetime :as dt]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-506-policy-persistence
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
         {:Intercept :RBAC
          :Resource :I556PP/E1
          :Spec [:q#
                 {:actions all-actions
                  :role :R1}]}}
        {:Kernel/Role
         {:Name "role-2"} :as :R2}
        {:Kernel/Policy
         {:Intercept :RBAC
          :Resource :I556PP/E1
          :Spec [:q#
                 {:actions [:Lookup]
                  :role :R2}]}}
        {:Kernel/Policy
         {:Intercept :RBAC
          :Resource :I556PP/E2
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
