(ns fractl.policy
  (:require [fractl.lang :as f]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]))

(f/component :Kernel.Policy)

(f/dataflow
 :Kernel.Policy/LoadPolicies
 {:Kernel/Policy
  {:Intercept? :Kernel.Policy/LoadPolicies.Intercept
   :Resource? :Kernel.Policy/LoadPolicies.Resource}})

(defn lookup-policies [intercept resource]
  (let [result
        (ev/eval-all-dataflows
         (cn/make-instance
          {:Kernel.Policy/LoadPolicies
           {:Intercept (u/keyword-as-string intercept)
            :Resource (u/keyword-as-string resource)}}))]
    (ev/ok-result result true)))
