(ns agentlang.policy.logging
  (:require [clojure.set :as set]
            [agentlang.policy.logging-util :as lu]
            [agentlang.component :as cn]
            [agentlang.resolver.policy :as rp]))

(defn rules [event-instance]
  (rp/logging-rules
   (cn/instance-type event-instance)))

(defn- rules-with-key [rules k]
  (filter
   identity
   (map #(if (map? %)
           (k %)
           (k (second %)))
        rules)))

(defn log-levels [rules]
  (set/difference
   lu/log-levels
   (set
    (flatten
     (rules-with-key rules :Disable)))))

(defn hidden-attributes [rules]
  (first (rules-with-key rules :HideAttributes)))
