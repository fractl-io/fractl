(ns agentlang.policy
  (:require [agentlang.util :as u]
            [agentlang.evaluator :as ev]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]
            #?(:cljs [agentlang.ui.policy-resolver :as uip])))

(defn- normalize-path [p]
  (if (li/parsed-path? p)
    (li/make-path p)
    p))

(declare lookup-parent-policies)

(defn lookup-policies [intercept resource]
  (or
   #?(:cljs
      (uip/lookup-policies intercept resource)
      :clj
      (let [result
            (ev/eval-all-dataflows
             {:Agentlang.Kernel.Lang/LoadPolicies
              {:Intercept (u/keyword-as-string intercept)
               :Resource (u/keyword-as-string (normalize-path resource))}})]
        (u/ok-result result true)))
   (lookup-parent-policies intercept resource)))

(defn- lookup-parent-policies [intercept resource]
  (when-let [[_ _ p] (cn/containing-parents resource)]
    (lookup-policies intercept p)))

(defn create-policy [intercept resource spec]
  (let [result
        (ev/eval-all-dataflows
         {:Agentlang.Kernel.Lang/Create_Policy
          {:Instance
           {:Agentlang.Kernel.Lang/Policy
            {:Intercept (u/keyword-as-string intercept)
             :Resource (u/keyword-as-string (normalize-path resource))
             :Spec [:q# spec]}}}})]
    (u/ok-result result)))

(def spec :Spec)
