(ns fractl.policy.logging
  (:require [fractl.policy.logging-util :as lu]))

(defn log-levels-for-event [event-instance]
  ;; TODO: consult policies for disabled levels
  lu/log-levels)
