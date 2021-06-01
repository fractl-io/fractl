(ns fractl.resolver.policy
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]))

(def ^:private policy-db (u/make-cell {:RBAC {} :Logging {}}))

(defn- assoc-rbac-policy [db policy]
  (let [rule (:Rule policy)
        stg (:InterceptStage policy)
        stage (if (= stg :Default)
                :PreEval
                stg)]
    (loop [db db, rs (:Resource policy)]
      (if-let [r (first rs)]
        (recur (assoc db [r stage] (conj (get db r []) rule))
               (rest rs))
        db))))

(defn- assoc-logging-policy [db policy]
  )

(def ^:private assoc-policy
  {:RBAC assoc-rbac-policy
   :Logging assoc-logging-policy})

(defn- policy-upsert [inst]
  (let [k (:Intercept inst)]
    (if-let [db (get @policy-db k)]
      (u/safe-set
       policy-db
       (assoc @policy-db k ((k assoc-policy) db inst)))
      (u/throw-ex (str "policy intercept not supported - " k)))
    inst))

(defn- policy-delete [inst]
  ;; TODO: implement delete
  (:Id inst))

(defn- policy-query [query]
  ;; TODO: implement query
  nil)

(def ^:private resolver-fns
  {:upsert {:handler policy-upsert}
   :delete {:handler policy-delete}
   :query {:handler policy-query}})

(defn make [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))
