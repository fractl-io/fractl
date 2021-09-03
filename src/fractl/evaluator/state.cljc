(ns fractl.evaluator.state
  (:require [fractl.util :as u]))

(defn- set-active-state!
  ([db k obj]
   (u/safe-set
    db
    (assoc @db k obj)))
  ([db obj] (set-active-state! db :public obj)))

(defn- get-active-state
  ([db k]
   (get @db k))
  ([db] (get-active-state db :public)))

(def ^:private active-evaluators (u/make-cell {}))
(def ^:private active-stores (u/make-cell {}))

(def set-active-evaluator! (partial set-active-state! active-evaluators))
(def get-active-evaluator (partial get-active-state active-evaluators))

(def set-active-store! (partial set-active-state! active-stores))
(def get-active-store (partial get-active-state active-stores))
