(ns fractl.global-state
  (:require [fractl.evaluator :as e]
            [fractl.store :as store]))

(defn init [config]
  (e/global-dataflow-eval config))
