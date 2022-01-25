(ns fractl.global-state
  (:require [fractl.evaluator :as e]
            [fractl.store :as store]))

(defn init [_]
  (e/global-dataflow-eval))
