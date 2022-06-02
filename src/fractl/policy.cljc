(ns fractl.policy
  (:require [fractl.util :as u]
            [fractl.evaluator :as ev]
            #?(:cljs [fractl.ui.policy-resolver :as uip])))

(defn lookup-policies [intercept resource]
  #?(:cljs
     (uip/lookup-policies intercept resource)
     :clj
     (let [result
           (ev/eval-all-dataflows
            {:Kernel/LoadPolicies
             {:Intercept (u/keyword-as-string intercept)
              :Resource (u/keyword-as-string resource)}})]
       (ev/ok-result result true))))

(defn upsert-policy [intercept resource spec]
  (let [result
        (ev/eval-all-dataflows
         {:Kernel/Upsert_Policy
          {:Instance
           {:Kernel/Policy
            {:Intercept (u/keyword-as-string intercept)
             :Resource (u/keyword-as-string resource)
             :Spec [:q# spec]}}}})]
    (ev/ok-result result)))

(def spec :Spec)
