(ns fractl.resolver.policy
  "Policy management"
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.compiler.rule :as rl]
            [fractl.lang.internal :as li]
            [fractl.policy.logging-util :as lu]
            [fractl.resolver.core :as r]))

(def PRE-EVAL :PreEval)
(def POST-EVAL :PostEval)

(def ^:private lock
  #?(:clj (java.util.concurrent.locks.ReentrantLock.)))

(def ^:private policy-db
  #?(:clj (java.util.HashMap.)
     :cljs (u/make-cell {:RBAC {} :Logging {}})))

(defn- with-lock [f]
  #?(:clj
     (do
       (.lock lock)
       (try
         (f)
         (finally
           (.unlock lock))))
     :cljs (f)))

(def ^:private store-opr-names #{:Upsert :Delete :Lookup})

(def ^:private allow-all (constantly true))

(declare compile-rbac-rule)

(defn- compile-crud-rules [rs]
  (loop [rs rs, result []]
    (if-let [hd (first rs)]
      (recur (nthrest rs 2)
             (conj
              result
              [(vec hd)
               (compile-rbac-rule (second rs) false)]))
      result)))

(defn- compile-rbac-rule [rs crud-rule]
  (if crud-rule
    (compile-crud-rules rs)
    (case (first rs)
      :when
      (rl/compile-rule-pattern (second rs))
      :allow-all
      (if (= 1 (count rs))
        allow-all
        (u/throw-ex (str "invalid rule " rs)))
      (u/throw-ex (str "invalid clause " (first rs) " in rule - " rs)))))

(def ^:private compile-rule
  {:RBAC compile-rbac-rule
   :Logging lu/compile-logging-rule})

(defn- make-default-event-names
  "Return the default event names for the given entity"
  [oprs entity-name]
  (let [[a b] (map name (li/split-path entity-name))]
    (map #(keyword (str a "/" (name %) "_" b)) oprs)))

(defn- install-policy
  "Add a policy to the store. The policy rules are compiled with the
  help of the rule engine."
  [db policy crud-rule]
  (let [rule (:Rule policy)
        stg (keyword (:InterceptStage policy))
        stage (if (= stg :Default)
                PRE-EVAL
                stg)
        intercept (keyword (:Intercept policy))
        add-rule (if crud-rule concat conj)
        cf (compile-rule intercept)
        compiled-rule (if cf (cf rule crud-rule) rule)]
    (loop [db db, rs (:Resource policy)]
      (if-let [r (first rs)]
        (let [r (li/split-path r)
              k [r stage]
              rls (get db k [])]
          (recur
           (assoc db k (add-rule rls compiled-rule))
           (rest rs)))
        db))))

(defn- store-opr-name? [n]
  (some #{n} store-opr-names))

(defn- crud-rule?
  "Return true if the rule specifies CRUD on an entity"
  [rule]
  (let [f (first rule)]
    (and (vector? f)
         (every? store-opr-name? f))))

(defn- save-any-policy [db policy]
  (install-policy
   db policy
   (crud-rule? (:Rule policy))))

(def ^:private save-policy
  {:RBAC save-any-policy
   :Logging save-any-policy})

(defn- normalize-policy [inst]
  ;; Remove the :q# (quote) prefix from rule.
  (let [rule (:Rule inst)]
    (if (li/quoted? rule)
      (assoc inst :Rule (second rule))
      inst)))

(defn- intercept-db [k]
  #?(:clj
     (or (.get policy-db k)
         {})
     :cljs (get @policy-db k {})))

(defn- write-policy! [k inst]
  (let [db ((k save-policy)
            (intercept-db k)
            inst)]
    #?(:clj
       (.put policy-db k db)
       :cljs
       (u/call-and-set
        policy-db
        #(assoc @policy-db k db)))))

(defn policy-upsert
  "Add a policy object to the policy store"
  [inst]
  (let [inst (normalize-policy inst)
        k (keyword (:Intercept inst))
        save-fn (k save-policy)]
    (when-not save-fn
      (u/throw-ex (str "policy intercept not supported - " k)))
    (with-lock
      #(write-policy! k inst))
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

(defn make
  "Create and return a policy resolver"
  [resolver-name config]
  (r/make-resolver resolver-name resolver-fns))

(defn fetch-rules
  "Return the polices stored at the key provided.
  Key should be a path."
  [intercept stage k]
  (let [pk [(li/split-path k) stage]]
    #?(:clj
       (with-lock
         #(let [db (intercept-db intercept)]
            (get db pk)))
       :cljs (get-in @policy-db [intercept pk]))))

(def rbac-rules (partial fetch-rules :RBAC PRE-EVAL))
(def logging-rules (partial fetch-rules :Logging PRE-EVAL))
