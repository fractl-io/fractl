(ns fractl.resolver.policy
  "Policy management"
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.lang.rule :as rl]
            [fractl.lang.internal :as li]
            [fractl.policy.logging-util :as lu]
            [fractl.resolver.core :as r]))

(def PRE-EVAL :PreEval)
(def POST-EVAL :PostEval)

(def ^:private policy-db (u/make-cell {:RBAC {} :Logging {}}))

(def ^:private store-opr-names #{:Upsert :Delete :Lookup})

(defn- compile-rbac-rule [r]
  (if (= :when (first r))
    (rl/compile-rule-pattern (second r))
    (u/throw-ex (str "invalid clause " (first r) " in rule - " r))))

(def ^:private compile-rule
  {:RBAC compile-rbac-rule
   :Logging lu/compile-logging-rule})

(defn- make-default-event-names
  "Return the default event names for the given entity"
  [oprs entity-name]
  (let [[a b] (map name (li/split-path entity-name))]
    (map #(keyword (str a "/" (name %) "_" b)) oprs)))

(declare install-policy)

(defn- install-event-policies
  "Install policies for the named events."
  [db policy event-names]
  (loop [db db, evt-names event-names]
    (if-let [ename (first evt-names)]
      (recur
       (install-policy
        db
        (assoc
         policy
         :Resource [ename])
        true)
       (rest evt-names))
      db)))

(defn- install-default-event-policies
  "Install policies for the default events like Upsert_entity and Lookup_entity"
  [db policy]
  (let [rule (:Rule policy)
        f (partial make-default-event-names (first rule))
        clause (second rule)]
    (install-event-policies
     db (assoc policy :Rule clause)
     (flatten (map #(f %) (:Resource policy))))))

(defn- install-policy
  "Add a policy to the store. If the compile? flag is ture, the policy
  rules are compiled with the help of the rule engine. A rule defined
  for an event resource is always compiled. Rules defined for entities
  are compiled when new dataflows are declared."
  [db policy compile?]
  (let [rule (:Rule policy)
        stg (keyword (:InterceptStage policy))
        stage (if (= stg :Default)
                PRE-EVAL
                stg)
        intercept (keyword (:Intercept policy))]
    (loop [db db, rs (:Resource policy)]
      (if-let [r (first rs)]
        (let [r (li/split-path r)
              k [r stage]]
          (recur
           (assoc
            db k
            (conj
             (get db k [])
             (if compile?
               ((compile-rule intercept) rule)
               rule)))
           (rest rs)))
        (if compile? db (install-default-event-policies db policy))))))

(defn- store-opr-name? [n]
  (some #{n} store-opr-names))

(defn- rule-on-store?
  "Return true if the rule specifies CRUD on an entity"
  [rule]
  (let [f (first rule)]
    (and (vector? f)
         (every? store-opr-name? f))))

(defn- save-any-policy [db policy]
  (install-policy
   db policy
   (not (rule-on-store? (:Rule policy)))))

(def ^:private save-policy
  {:RBAC save-any-policy
   :Logging save-any-policy})

(defn- normalize-policy [inst]
  ;; Remove the :q# (quote) prefix from rule.
  (let [rule (:Rule inst)]
    (if (li/quoted? rule)
      (assoc inst :Rule (second rule))
      inst)))

(defn policy-upsert
  "Add a policy object to the policy store"
  [inst]
  (let [inst (normalize-policy inst)
        k (keyword (:Intercept inst))
        save-fn (k save-policy)]
    (when-not save-fn
      (u/throw-ex (str "policy intercept not supported - " k)))
    (let [db (get @policy-db k {})]
      (u/safe-set
       policy-db
       (assoc @policy-db k ((k save-policy) db inst)))
      inst)))

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

(defn make
  "Create and return a policy resolver"
  [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))

(defn eval-rules
  "Return the RBAC polices stored at the key provided.
  Key should be a path."
  [intercept stage k]
  (get-in @policy-db [intercept [(li/split-path k) stage]]))

(def rbac-eval-rules (partial eval-rules :RBAC PRE-EVAL))
(def logging-eval-rules (partial eval-rules :Logging PRE-EVAL))
