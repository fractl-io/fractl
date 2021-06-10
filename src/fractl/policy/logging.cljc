(ns fractl.policy.logging
  (:require [clojure.set :as set]
            [fractl.policy.logging-util :as lu]
            [fractl.component :as cn]
            [fractl.resolver.policy :as rp]))

(defn rules [event-instance]
  (rp/logging-eval-rules
   (cn/instance-name event-instance)))

(defn log-levels [rules]
  (let [disabled-levels
        (set
         (flatten
          (filter
           identity
           (map :Disable rules))))]
    (set/difference lu/log-levels disabled-levels)))
