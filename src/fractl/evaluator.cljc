(ns fractl.evaluator
  "Helper functions for compiling and evaluating patterns."
  (:require [clojure.walk :as w]
            [fractl.component :as cn]
            [fractl.compiler :as c]
            [fractl.env :as env]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.util.seq :as su]
            [fractl.util.http :as uh]
            [fractl.store :as store]
            [fractl.resolver.registry :as rr]
            [fractl.policy.logging :as logging]
            [fractl.lang.internal :as li]
            [fractl.lang.opcode :as opc]
            ;; load kernel components
            [fractl.model.fractl.kernel.lang]
            [fractl.model.fractl.kernel.identity]
            [fractl.model.fractl.kernel.rbac]
            ;; :~
            [fractl.global-state :as gs]
            [fractl.evaluator.state :as es]
            [fractl.evaluator.internal :as i]
            [fractl.evaluator.root :as r]
            [fractl.evaluator.intercept.core :as interceptors]))

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

(def ^:private internal-event-flag
  #?(:clj (Object.)
     :cljs {:internal-event true}))

(def ^:private internal-event-key :-*-internal-event-*-)

(defn mark-internal [event-instance]
  (assoc event-instance internal-event-key internal-event-flag))

(defn internal-event? [event-instance]
  (when (identical? internal-event-flag (internal-event-key event-instance))
    true))

(declare eval-all-dataflows)

(defn- fire-post-events-for [tag insts]
  (doseq [inst insts]
    (when-let [[event-name r] (cn/fire-post-event eval-all-dataflows tag inst)]
      (when-not (u/safe-ok-result r)
        (log/warn r)
        (u/throw-ex (str "internal event " event-name " failed."))))))

(defn- fire-post-events [env]
  (let [srcs (env/post-event-trigger-sources env)]
    (doseq [tag [:create :update :delete]]
      (when-let [insts (seq (tag srcs))]
        (fire-post-events-for tag insts))))
  env)

(defn- eval-dataflow-in-transaction [evaluator env event-instance df txn]
  (binding [gs/active-event-context (or (li/event-context event-instance)
                                        gs/active-event-context)]
    (let [txn-set (atom false)]
      (when (and txn (not (gs/get-active-txn)))
        (gs/set-active-txn! txn)
        (reset! txn-set true))
      (try
        (let [is-internal (internal-event? event-instance)
              event-instance0 (if is-internal
                                (dissoc event-instance internal-event-key)
                                event-instance)
              event-instance (if-not (li/event-context event-instance0)
                               (assoc event-instance0 li/event-context gs/active-event-context)
                               event-instance0)
              env0 (if is-internal
                     (env/block-interceptors env)
                     (env/assoc-active-event env event-instance))
              continuation (fn [event-instance]
                             (let [env (if event-instance
                                         (env/assoc-active-event
                                          (env/bind-instance
                                           env0 (li/split-path (cn/instance-type event-instance))
                                           event-instance)
                                          event-instance)
                                         env0)
                                   [_ dc] (cn/dataflow-opcode
                                           df (or (env/with-types env)
                                                  cn/with-default-types))
                                   result (deref-futures (let [r (dispatch-opcodes evaluator env dc)]
                                                           (if (and (map? r) (not= :ok (:status r)))
                                                             (throw (ex-info "eval failed" {:eval-result r}))
                                                             r)))]
                               (fire-post-events (:env result))
                               result))]
          (interceptors/eval-intercept env0 event-instance continuation))
        (finally (when @txn-set (gs/set-active-txn! nil)))))))

(defn- maybe-init-event [event-obj]
  (if (cn/event-instance? event-obj)
    event-obj
    (let [event-name (first (keys event-obj))]
      (cn/make-instance event-name (event-name event-obj)))))

(defn eval-dataflow
  "Evaluate a compiled dataflow, triggered by event-instance, within the context
   of the provided environment. Each compiled pattern is dispatched to an evaluator,
   where the real evaluation is happening. Return the value produced by the resolver."
  ([evaluator env event-instance df]
   (let [event-instance (maybe-init-event event-instance)
         f (partial eval-dataflow-in-transaction evaluator env event-instance df)]
     (try
       (let [r (if-let [txn (gs/get-active-txn)]
                 (f txn)
                 (if-let [store (env/get-store env)]
                   (store/call-in-transaction store f)
                   (f nil)))]
         (r/merge-init-pending-components!)
         r)
       (catch #?(:clj Exception :cljs :default) ex
         (do (r/reset-init-pending-components!)
             (if-let [r (:eval-result (ex-data ex))]
               r
               (throw ex)))))))
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
  (log/debug
   (str "dataflow result for " (cn/instance-type event-instance)
        " - " (remove-hidden-attributes hidden-attrs obj))))

(defn- eval-dataflow-with-logs [evaluator env event-instance hidden-attrs df]
  (try
    (let [r (eval-dataflow evaluator env event-instance df)]
      (log-result-object hidden-attrs event-instance r)
      r)
    (catch #?(:clj Exception :cljs :default) ex
      (let [msg (str "error in dataflow for "
                     (or (cn/instance-type event-instance)
                         (li/record-name event-instance)
                         event-instance)
                     " - " #?(:clj (str (.getMessage ex)
                                        (ex-data ex))
                              :cljs ex))]
        (log/warn msg)
        (log/exception ex)
        (i/error msg)))))

(defn- run-dataflows
  "Compile and evaluate all dataflows attached to an event. The query-compiler
   and evaluator returned by a previous call to evaluator/make may be passed as
   the first two arguments."
  [compile-query-fn evaluator env event-instance]
  (let [dfs (c/compile-dataflows-for-event compile-query-fn event-instance)
        logging-rules (logging/rules event-instance)
        hidden-attrs (logging/hidden-attributes logging-rules)
        ef (partial eval-dataflow-with-logs evaluator
                    env event-instance hidden-attrs)]
    (log-event hidden-attrs event-instance)
    (mapv ef dfs)))

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
    (rr/root-registry)

    (map? resolver-or-resolver-config)
    (do (rr/register-resolvers resolver-or-resolver-config)
        (rr/root-registry))

    (and (keyword? resolver-or-resolver-config)
         (= resolver-or-resolver-config :none))
    nil

    :else
    (if (rr/registry? resolver-or-resolver-config)
      resolver-or-resolver-config
      (u/throw-ex (str "invalid resolver config " resolver-or-resolver-config)))))

(defn- evaluator
  ([store-or-store-config resolver-or-resolver-config with-query-support]
   (let [store (store-from-config store-or-store-config)
         resolver (resolver-from-config resolver-or-resolver-config)
         [compile-query-fn evaluator] (make store)
         env (env/make store resolver)
         ef (partial run-dataflows compile-query-fn evaluator env)
         result (if with-query-support
                  (fn [x]
                    (if-let [qinfo (:Query x)]
                      (r/find-instances env store (first qinfo) (second qinfo))
                      (ef x)))
                  ef)]
     (es/set-active-state! result store)
     result))
  ([store-or-store-config resolver-or-resolver-config]
   (evaluator store-or-store-config resolver-or-resolver-config false))
  ([] (evaluator (es/get-active-store) nil)))

(defn evaluate-pattern
  ([store-or-store-config resolver-or-resolver-config pattern]
   (let [store (if (nil? store-or-store-config)
                 (or (es/get-active-store)
                     (store-from-config store-or-store-config))
                 (store-from-config store-or-store-config))
         resolver (resolver-from-config resolver-or-resolver-config)
         [compile-query-fn evaluator] (make store)
         env (env/make store resolver)
         opcode (c/compile-standalone-pattern compile-query-fn pattern)]
     (dispatch evaluator env opcode)))
  ([pattern] (evaluate-pattern nil nil pattern)))

(defn eval-all-dataflows
  ([event-obj store-or-store-config resolver-or-resolver-config]
   (let [ef (evaluator store-or-store-config resolver-or-resolver-config)]
     (ef event-obj)))
  ([event-obj]
   (eval-all-dataflows event-obj (es/get-active-store) nil)))

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
    (mapv filter-public-result xs)))

(defn public-evaluator [store-or-config with-query-support]
  (comp filter-public-result (evaluator store-or-config nil with-query-support)))

(defn query-fn [store]
  (partial r/find-instances env/EMPTY store))

(defn safe-eval [event-obj]
  (u/safe-ok-result
   (eval-all-dataflows
    (cn/make-instance event-obj))))

(defn safe-eval-internal [event-obj]
  (u/safe-ok-result
   (eval-all-dataflows
    (mark-internal
     (cn/make-instance event-obj)))))

(defn safe-eval-pattern [pattern]
  (u/safe-ok-result
   (evaluate-pattern pattern)))
