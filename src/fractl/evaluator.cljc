(ns fractl.evaluator
  "Helper functions for compiling and evaluating patterns."
  (:require [fractl.component :as cn]
            [fractl.compiler :as c]
            [fractl.env :as env]
            [fractl.util :as u]
            [fractl.store :as store]
            [fractl.lang.internal :as li]
            [fractl.lang.opcode :as opc]
            [fractl.evaluator.internal :as i]
            [fractl.evaluator.root :as r]))

(defn- dispatch-an-opcode [evaluator env opcode]
  (if-let [f ((opc/op opcode) i/dispatch-table)]
    (f evaluator env (opc/arg opcode))
    (u/throw-ex (str "no dispatcher for opcode - " (opc/op opcode)))))

(defn dispatch [evaluator env {opcode :opcode}]
  (if (map? opcode)
    (dispatch-an-opcode evaluator env opcode)
    (loop [opcs opcode, env env, result nil]
      (if-let [opc (first opcs)]
        (let [r (dispatch-an-opcode evaluator env opc)]
          (recur (rest opcs) (:env r) r))
        result))))

(def ok? i/ok?)
(def dummy-result i/dummy-result)

(defn eval-dataflow
  "Evaluate a compiled dataflow, triggered by event-instance, within the context
   of the provided environment. Each compiled pattern is dispatched to an evaluator,
   where the real evaluation is happens. Return the value produced by the resolver."
  ([evaluator env event-instance df]
   (let [n (li/split-path (cn/instance-name event-instance))
         env (env/bind-instance env n event-instance)
         [_ dc] (cn/dataflow-opcode df)]
     (loop [dc dc, result (dummy-result env)]
       (if (ok? result)
         (if-let [opcode (first dc)]
           (recur (rest dc) (dispatch evaluator (:env result) opcode))
           result)
         result))))
  ([evaluator event-instance df] (eval-dataflow evaluator env/EMPTY event-instance df)))

(declare run-dataflows)

(defn make
  "Use the given store to create a query compiler and pattern evaluator.
   Return the vector [compile-query-fn, evaluator]."
  [store]
  (let [cq (partial store/compile-query store)]
    [cq (r/get-default-evaluator store (partial run-dataflows cq))]))

(defn run-dataflows
  "Compile and evaluate all dataflows attached to an event. The query-compiler
   and evaluator returned by a previous call to evaluator/make may be passed as
   the first two arguments."
  [compile-query-fn evaluator event-instance]
  (let [dfs (c/compile-dataflows-for-event compile-query-fn event-instance)]
    (map #(eval-dataflow evaluator event-instance %) dfs)))
