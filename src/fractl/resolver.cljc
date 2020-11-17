(ns fractl.resolver
  "Pattern resolvers, aka evaluators"
  (:require [fractl.util :as u]
            [fractl.lang.opcode :as opc]
            [fractl.store :as store]
            [fractl.resolver.internal :as i]
            [fractl.resolver.root :as r]))

(defn- dispatch-an-opcode [env opcode resolver]
  (if-let [f ((opc/op opcode) i/dispatch-table)]
    (f (i/vm resolver) env (opc/arg opcode))
    (u/throw-ex (str "no dispatcher for opcode - " (opc/op opcode)))))

(defn dispatch [env {opcode :opcode resolver :resolver :as x}]
  (if (map? opcode)
    (dispatch-an-opcode env opcode resolver)
    (loop [opcs opcode, env env, result nil]
      (if-let [opc (first opcs)]
        (let [r (dispatch-an-opcode env opc resolver)]
          (recur (rest opcs) (:env r) r))
        result))))

(defn compile-query [resolver query-pattern]
  (store/compile-query (i/store resolver) query-pattern))

;; The database of registered resolvers.
#?(:clj (def ^:private db (ref {}))
   :cljs (def ^:private db (atom {})))

(defn resolver-for-path [path eval-event-dataflows]
  (or (get db path)
      (r/get-default-resolver eval-event-dataflows)))

(def ok? i/ok?)
(def dummy-result i/dummy-result)
