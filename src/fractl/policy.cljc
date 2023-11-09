(ns fractl.policy
  (:require [fractl.util :as u]
            [fractl.evaluator :as ev]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            #?(:cljs [fractl.ui.policy-resolver :as uip])))

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
             {:Fractl.Kernel.Lang/LoadPolicies
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
         {:Fractl.Kernel.Lang/Create_Policy
          {:Instance
           {:Fractl.Kernel.Lang/Policy
            {:Intercept (u/keyword-as-string intercept)
             :Resource (u/keyword-as-string (normalize-path resource))
             :Spec [:q# spec]}}}})]
    (u/ok-result result)))

(def spec :Spec)
