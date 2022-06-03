(ns fractl.policy
  (:require [fractl.util :as u]
            [fractl.evaluator :as ev]
            [fractl.component :as cn]
            #?(:cljs [fractl.ui.policy-resolver :as uip])))

(declare lookup-container-policies)

(defn lookup-policies [intercept resource]
  (or
   #?(:cljs
      (uip/lookup-policies intercept resource)
      :clj
      (let [result
            (ev/eval-all-dataflows
             {:Kernel/LoadPolicies
              {:Intercept (u/keyword-as-string intercept)
               :Resource (u/keyword-as-string resource)}})]
        (ev/ok-result result true)))
   (lookup-container-policies intercept resource)))

(defn- lookup-container-policies [intercept resource]
  (when-let [c (cn/fetch-container resource)]
    (lookup-policies intercept c)))

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
