(ns fractl.evaluator.intercept.rbac
  (:require [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.rbac.core :as rbac]
            [fractl.evaluator.intercept.internal :as ii]))

(defn- has-priv? [rbac-predic user data]
  (let [p (partial rbac-predic user)
        entity-name
        (cond
          (keyword? data) data
          (cn/an-instance? data) (cn/instance-type data))]
    (if entity-name
      (p entity-name)
      (let [rs (set (map cn/instance-type data))]
        (every? identity (map #(p user %) rs))))))

(def ^:private apply-upsert-rules (partial has-priv? rbac/can-upsert?))
(def ^:private apply-read-rules (partial has-priv? rbac/can-read?))
(def ^:private apply-delete-rules (partial has-priv? rbac/can-delete?))
(def ^:private apply-eval-rules (partial has-priv? rbac/can-eval?))

(def ^:private actions
  {:upsert apply-upsert-rules
   :lookup apply-read-rules
   :delete apply-delete-rules
   :eval apply-eval-rules})

(defn- run [opr arg]
  (if-let [f (opr actions)]
    (when (f (ii/user arg) (ii/data arg))
      arg)
    arg))

(defn make []
  (ii/make-interceptor :rbac run))
