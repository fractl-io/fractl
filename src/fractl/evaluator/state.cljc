(ns fractl.evaluator.state
  (:require [fractl.util :as u]))

(def ^:private active-evaluators (u/make-cell {}))

(defn set-active-evaluator!
  ([k ev]
   (u/safe-set
    active-evaluators
    (assoc @active-evaluators k ev)))
  ([ev] (set-active-evaluator! :public ev)))

(defn get-active-evaluator
  ([k]
   (get @active-evaluators k))
  ([] (get-active-evaluator :public)))
