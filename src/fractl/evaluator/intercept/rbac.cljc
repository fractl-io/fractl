(ns fractl.evaluator.intercept.rbac
  (:require [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.rbac.core :as rbac]
            [fractl.evaluator.intercept.internal :as ii]))

(defn- has-priv? [rbac-predic user data]
  (let [p (partial rbac-predic user)
        entity-name
        (cond
          (keyword? data) data
          (cn/an-instance? data) (cn/instance-type data)
          (li/parsed-path? data) (li/make-path data)
          :else (u/throw-ex (str "invalid argument for rbac interceptor - " data)))]
    (if entity-name
      (p entity-name)
      (let [rs (set (map cn/instance-type data))]
        (every? identity (map #(p %) rs))))))

(def ^:private apply-upsert-rules (partial has-priv? rbac/can-upsert?))
(def ^:private apply-read-rules (partial has-priv? rbac/can-read?))
(def ^:private apply-delete-rules (partial has-priv? rbac/can-delete?))
(def ^:private apply-eval-rules (partial has-priv? rbac/can-eval?))

(def ^:private actions
  {:upsert apply-upsert-rules
   :read apply-read-rules
   :delete apply-delete-rules
   :eval apply-eval-rules})

(defn- run [opr arg]
  (if (ii/data-input? arg)
    (if-let [f (opr actions)]
      (when (f (cn/event-context-user (ii/event arg))
               (ii/data arg))
        arg)
      arg)
    arg))

(defn make []
  (ii/make-interceptor :rbac run))
