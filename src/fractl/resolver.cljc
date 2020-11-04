(ns fractl.resolver
  "Pattern resolvers, aka evaluators"
  (:require [fractl.util :as u]
            [fractl.util.seq :as us]
            [fractl.component :as cn]
            [fractl.compiler :as c]
            [fractl.lang.internal :as li]
            [fractl.lang.opcode :as opc]
            [fractl.env :as env]
            [fractl.resolver.protocol :as p]
            [fractl.resolver.root :as r]))

(defn- dispatch-an-opcode [env opcode resolver]
  (if-let [f ((opc/op opcode) p/dispatch-table)]
    (f resolver env (opc/arg opcode))
    (u/throw-ex (str "no dispatcher for opcode - " (opc/op opcode)))))

(defn dispatch [env {opcode :opcode resolver :resolver :as x}]
  (if (map? opcode)
    (dispatch-an-opcode env opcode resolver)
    (loop [opcs opcode, env env, result nil]
      (if-let [opc (first opcs)]
        (let [r (dispatch-an-opcode env opc resolver)]
          (recur (rest opcs) (:env r) r))
        result))))

(defn eval-dataflow
  ([env event-instance df]
   (let [n (li/split-path (cn/instance-name event-instance))
         env (env/bind-instance env n event-instance)
         [_ dc] (cn/dataflow-opcode df)]
     (loop [dc dc, result (p/dummy-result env)]
       (if (p/ok? result)
         (if-let [opcode (first dc)]
           (recur (rest dc) (dispatch (:env result) opcode))
           result)
         result))))
  ([event-instance df] (eval-dataflow env/EMPTY event-instance df)))

(declare resolver-for-path)

(defn eval-all-dataflows-for-event [event-instance]
  (let [dfs (c/compiled-dataflows-for-event
             resolver-for-path event-instance)]
    (map #(eval-dataflow event-instance %) dfs)))

;; The database of registered resolvers.
(def ^:private db (ref {}))

(defn resolver-for-path [path]
  (or (get db path)
      (r/get-default-resolver eval-all-dataflows-for-event)))
