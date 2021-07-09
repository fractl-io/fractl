(ns fractl.policy.logging
  (:require [clojure.set :as set]
            [fractl.policy.logging-util :as lu]
            [fractl.component :as cn]
            [fractl.resolver.policy :as rp]))

(defn rules [event-instance]
  (rp/logging-eval-rules
   (cn/instance-name event-instance)))

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
