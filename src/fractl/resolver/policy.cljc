(ns fractl.resolver.policy
  "Policy management"
  (:require [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.lang.rule :as rl]
            [fractl.lang.internal :as li]
            [fractl.resolver.core :as r]))

(def PRE-EVAL :PreEval)
(def POST-EVAL :PostEval)

(def ^:private policy-db (u/make-cell {:RBAC {} :Logging {}}))

(def ^:private store-opr-names #{:Upsert :Delete :Lookup})

(defn- compile-rbac-rule [r]
  (if (= :when (first r))
    (rl/compile-rule-pattern (second r))
    (u/throw-ex (str "invalid clause " (first r) " in rule - " r))))

(def ^:private log-levels [:DEBUG :INFO :WARN :ERROR])

(defn- validate-logging-rule-keys [r]
  (doseq [k (keys r)]
    (when-not (some #{k} #{:Disable :PagerThreshold :ExcludeAttributes})
      (u/throw-ex (str "invalid logging rule - " k))))
  r)

(defn- validate-logging-disable-rule [r]
  (when-let [levels (:Disable r)]
    (let [levels (if (keyword? levels) [levels] levels)]
      (doseq [lvl levels]
        (when-not (some #{lvl} log-levels)
          (u/throw-ex (str "invalid log level - " lvl))))))
  r)

(defn- validate-pagerthreshold-rule [r]
  (when-let [pt (:PagerThreshold r)]
    (when-not (map? pt)
      (u/throw-ex (str ":PagerThreshold must be a map - " pt)))
    (doseq [lvl (keys pt)]
      (when-not (some #{lvl} log-levels)
        (u/throw-ex (str "invalid log level in :PagerThreshold - " lvl)))
      (doseq [k (keys (lvl pt))]
        (when-not (some #{k} #{:count :duration-minutes})
          (u/throw-ex (str "invalid :PagerThreshold entry - " [lvl k]))))))
  r)

(defn- validate-exclude-attribute-rule [r]
  (when-let [ea (:ExcludeAttributes r)]
    (doseq [n ea]
      (when-not (li/name? n)
        (u/throw-ex (str "invalid name in :ExcludeAttributes - " n)))))
  r)

(defn- compile-logging-rule
  "Parse a logging rule for validity, return the rule structure as is."
  [r]
  (-> r
      validate-logging-rule-keys
      validate-logging-disable-rule
      validate-pagerthreshold-rule
      validate-exclude-attribute-rule))

(def ^:private compile-rule {:RBAC compile-rbac-rule
                             :Logging compile-logging-rule})

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
        (let [r (li/split-path r)]
          (recur
           (assoc
            db [r stage]
            (conj
             (get db r [])
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

(defn policy-upsert
  "Add a policy object to the policy store"
  [inst]
  (let [k (keyword (:Intercept inst))
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
