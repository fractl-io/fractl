(ns fractl.eval
  "Helper functions for compiling and evaluating patterns."
  (:require [fractl.component :as cn]
            [fractl.compiler :as c]
            [fractl.lang.internal :as li]
            [fractl.env :as env]
            [fractl.resolver :as r]))

(declare eval-all-dataflows-for-event)

(defn resolver-for-path [path]
  (r/resolver-for-path path eval-all-dataflows-for-event))

(defn eval-dataflow
  ([env event-instance df]
   (let [n (li/split-path (cn/instance-name event-instance))
         env (env/bind-instance env n event-instance)
         [_ dc] (cn/dataflow-opcode df)]
     (loop [dc dc, result (r/dummy-result env)]
       (if (r/ok? result)
         (if-let [opcode (first dc)]
           (recur (rest dc) (r/dispatch (:env result) opcode))
           result)
         result))))
  ([event-instance df] (eval-dataflow env/EMPTY event-instance df)))

(defn eval-all-dataflows-for-event [event-instance]
  (let [dfs (c/compiled-dataflows-for-event resolver-for-path event-instance)]
    (map #(eval-dataflow event-instance %) dfs)))
