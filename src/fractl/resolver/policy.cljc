(ns fractl.resolver.policy
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.resolver.core :as r]))

(def PRE-EVAL :PreEval)
(def POST-EVAL :PostEval)

(def ^:private policy-db (u/make-cell {:RBAC {} :Logging {}}))

(defn- compile-rule [r]
  (if (= :when (first r))
    (li/compile-event-trigger-pattern (second r))
    (u/throw-ex (str "invalid clause " (first r) " in rule - " r))))

(defn- save-rbac-policy [db policy]
  (let [rule (:Rule policy)
        stg (keyword (:InterceptStage policy))
        stage (if (= stg :Default)
                PRE-EVAL
                stg)]
    (loop [db db, rs (:Resource policy)]
      (if-let [r (first rs)]
        (recur
         (assoc
          db [r stage]
          (conj
           (get db r [])
           (compile-rule rule)))
         (rest rs))
        db))))

(defn- save-logging-policy [db policy]
  )

(def ^:private save-policy
  {:RBAC save-rbac-policy
   :Logging save-logging-policy})

(defn- policy-upsert [inst]
  (let [k (:Intercept inst)]
    (if-let [db (get @policy-db k)]
      (u/safe-set
       policy-db
       (assoc @policy-db k ((k save-policy) db inst)))
      (u/throw-ex (str "policy intercept not supported - " k)))
    inst))

(defn- policy-delete [inst]
  ;; TODO: implement delete
  (:Id inst))

(defn- policy-query [query]
  ;; TODO: implement delete
  nil)

(def ^:private resolver-fns
  {:upsert {:handler policy-upsert}
   :delete {:handler policy-delete}
   :query {:handler policy-query}})

(defn make [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))

(defn rbac-eval-rules [k]
  (get-in @policy-db [:RBAC [k PRE-EVAL]]))
