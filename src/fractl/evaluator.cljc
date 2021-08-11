(ns fractl.evaluator
  "Helper functions for compiling and evaluating patterns."
  (:require [clojure.walk :as w]
            [fractl.component :as cn]
            [fractl.compiler :as c]
            [fractl.env :as env]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.util.seq :as su]
            [fractl.store :as store]
            [fractl.resolver.registry :as rr]
            [fractl.auth :as auth]
            [fractl.policy.rbac :as rbac]
            [fractl.policy.logging :as logging]
            [fractl.lang.internal :as li]
            [fractl.lang.opcode :as opc]
            [fractl.evaluator.internal :as i]
            [fractl.evaluator.root :as r]))

(def ^:private zero-trust-rbac-flag (u/make-cell false))
(def zero-trust-rbac! (partial u/safe-set zero-trust-rbac-flag))

(defn- dispatch-an-opcode [evaluator env opcode]
  (((opc/op opcode) i/dispatch-table)
   evaluator env (opc/arg opcode)))

(defn dispatch [evaluator env {opcode :opcode}]
  (if (map? opcode)
    (dispatch-an-opcode evaluator env opcode)
    (loop [opcs opcode, env env, result nil]
      (if-let [opc (first opcs)]
        (let [r (dispatch-an-opcode evaluator env opc)]
          (recur (rest opcs) (or (:env r) env) r))
        result))))

(def ok? i/ok?)
(def dummy-result i/dummy-result)

(defn- dispatch-opcodes [evaluator env opcodes]
  (if (map? opcodes)
    (dispatch evaluator env opcodes)
    (loop [dc opcodes, result (dummy-result env)]
      (if (or (ok? result)
              (cn/future-object? result))
        (if-let [opcode (first dc)]
          (recur (rest dc) (dispatch evaluator (:env result) opcode))
          result)
        result))))

(defn- deref-futures [result]
  (w/prewalk
   #(if (cn/future-object? %)
      (cn/deref-future-object %)
      %)
   result))

(defn eval-dataflow
  "Evaluate a compiled dataflow, triggered by event-instance, within the context
   of the provided environment. Each compiled pattern is dispatched to an evaluator,
   where the real evaluation is happening. Return the value produced by the resolver."
  ([evaluator env event-instance df]
   (let [env (if event-instance
               (env/bind-instance
                (env/bind-rbac-check
                 env
                 (partial
                  rbac/evaluate-opcode?
                  event-instance
                  @zero-trust-rbac-flag))
                (li/split-path (cn/instance-name event-instance))
                event-instance)
               env)
         [_ dc] (cn/dataflow-opcode df)
         result (deref-futures
                 (dispatch-opcodes evaluator env dc))]
     result))
  ([evaluator event-instance df]
   (eval-dataflow evaluator env/EMPTY event-instance df)))

(defn- remove-hidden-attributes [hidden-attrs inst]
  (if-let [r (:result inst)]
    (if (vector? r)
      (assoc
       inst
       :result
       (map
        (partial remove-hidden-attributes hidden-attrs)
        r))
      (remove-hidden-attributes hidden-attrs r))
    (loop [hs hidden-attrs, inst inst]
      (if-let [h (first hs)]
        (recur
         (rest hs)
         (if (cn/instance-of? (first h) inst)
           (su/dissoc-in inst (second h))
           inst))
        inst))))

(defn- log-event [hidden-attrs event-instance]
  (log/info
   (str "evaluating dataflow for event - "
        (remove-hidden-attributes hidden-attrs event-instance))))

(defn- log-result-object [hidden-attrs event-instance obj]
  (log/info
   (str "dataflow result for " (cn/instance-name event-instance)
        " - " (remove-hidden-attributes hidden-attrs obj))))

(defn- eval-dataflow-with-logs [evaluator env event-instance
                                log-info log-error hidden-attrs
                                df]
  (try
    (let [r (eval-dataflow evaluator env event-instance df)]
      (when log-info
        (log-result-object hidden-attrs event-instance r))
      r)
    (catch #?(:clj Exception :cljs :default) ex
      (do (when log-error
            (log/error
             (str "error in dataflow for "
                  (cn/instance-name event-instance)
                  " - " #?(:clj (.getMessage ex) :cljs ex))))
          (throw ex)))))

(defn- enrich-with-auth-owner
  "Query the :Authentication object using the :Id bound to
  :EventContext/Auth and using the information stored in that auth,
  load the :Owner object. Bind this object to :EventContext/Auth/Owner
  and return the updated event-instance."
  [event-instance]
  (if-let [auth-id (get-in event-instance [:EventContext :Auth])]
    (if-let [auth (auth/query auth-id)]
      (let [ctx (dissoc (:EventContext event-instance) :Auth)
            new-ctx (assoc-in ctx [:Auth :Owner] (:Owner auth))]
        (assoc event-instance :EventContext new-ctx))
      (u/throw-ex (str "no authorization bound to " auth-id)))
    event-instance))

(defn- run-dataflows
  "Compile and evaluate all dataflows attached to an event. The query-compiler
   and evaluator returned by a previous call to evaluator/make may be passed as
   the first two arguments."
  [compile-query-fn evaluator env event-instance]
  (let [event-instance (enrich-with-auth-owner event-instance)
        dfs (c/compile-dataflows-for-event
             compile-query-fn @zero-trust-rbac-flag
             event-instance)
        logging-rules (logging/rules event-instance)
        log-levels (logging/log-levels logging-rules)
        log-warn (some #{:WARN} log-levels)]
    (if (rbac/evaluate-dataflow? event-instance @zero-trust-rbac-flag)
      (let [log-info (some #{:INFO} log-levels)
            log-error (some #{:ERROR} log-levels)
            hidden-attrs (logging/hidden-attributes logging-rules)
            ef (partial
                eval-dataflow-with-logs evaluator
                env event-instance log-info log-error hidden-attrs)]
        (when log-info (log-event hidden-attrs event-instance))
        (doall (map ef dfs)))
      (let [msg (str "no authorization to evaluate dataflows on event - "
                     (cn/instance-name event-instance))]
        (when log-warn
          (log/warn msg))
        (u/throw-ex msg)))))

(defn- make
  "Use the given store to create a query compiler and pattern evaluator.
   Return the vector [compile-query-fn, evaluator]."
  [store]
  (let [cq (when store
             (partial store/compile-query store))]
    [cq (r/get-default-evaluator (partial run-dataflows cq) dispatch-opcodes eval-dataflow)]))

(defn store-from-config
  [store-or-store-config]
  (cond
    (or (nil? store-or-store-config)
        (map? store-or-store-config))
    (store/open-default-store store-or-store-config)

    (and (keyword? store-or-store-config)
         (= store-or-store-config :none))
    nil

    :else
    store-or-store-config))

(defn- resolver-from-config
  [resolver-or-resolver-config]
  (cond
    (nil? resolver-or-resolver-config)
    (rr/registered-resolvers)

    (map? resolver-or-resolver-config)
    (rr/register-resolvers resolver-or-resolver-config)

    (and (keyword? resolver-or-resolver-config)
         (= resolver-or-resolver-config :none))
    nil

    :else
    resolver-or-resolver-config))

(defn evaluator
  ([store-or-store-config resolver-or-resolver-config with-query-support]
   (let [store (store-from-config store-or-store-config)
         resolver (resolver-from-config resolver-or-resolver-config)
         [compile-query-fn evaluator] (make store)
         env (env/make store resolver)
         ef (partial run-dataflows compile-query-fn evaluator env)]
     (if with-query-support
       (fn [x]
         (if-let [qinfo (:Query x)]
           (r/find-instances env store (first qinfo) (second qinfo))
           (ef x)))
       ef)))
  ([store-or-store-config resolver-or-resolver-config]
   (evaluator store-or-store-config resolver-or-resolver-config false))
  ([] (evaluator nil nil)))

(defn- maybe-init-event [event-obj]
  (if (cn/event-instance? event-obj)
    event-obj
    (let [event-name (first (keys event-obj))]
      (cn/make-instance event-name (event-name event-obj)))))

(defn eval-all-dataflows
  ([event-obj store-or-store-config resolver-or-resolver-config]
   (doall ((evaluator store-or-store-config resolver-or-resolver-config)
           (maybe-init-event event-obj))))
  ([event-obj]
   (eval-all-dataflows event-obj nil nil)))

(defn eval-pure-dataflows
  "Facility to evaluate dataflows without producing any side-effects.
   This is useful for pure tasks like data-format transformation.
   An example: transforming data before being sent to a resolver to
   match the format requirements of the backend"
  [event-obj]
  (eval-all-dataflows event-obj :none :none))

(defn- filter-public-result [xs]
  (if (map? xs)
    (dissoc xs :env)
    (doall (map filter-public-result xs))))

(defn public-evaluator [store-config with-query-support]
  (comp filter-public-result (evaluator store-config nil with-query-support)))
