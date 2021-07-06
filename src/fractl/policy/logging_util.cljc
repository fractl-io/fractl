(ns fractl.policy.logging-util
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]))

(def log-levels #{:DEBUG :INFO :WARN :ERROR})

(defn- parse-logging-rule-keys [r]
  (let [crud-policy (not (map? r))
        rules (if crud-policy (second r) r)]
    (doseq [k (keys rules)]
      (when-not (some #{k} #{:Disable :PagerThreshold :HideAttributes})
        (u/throw-ex (str "invalid logging rule - " k))))
    (if crud-policy
      [(vec (first r)) rules]
      rules)))

(defn- parse-logging-disable-rule [r]
  (if-let [levels (:Disable r)]
    (let [levels (if (keyword? levels) [levels] levels)]
      (doseq [lvl levels]
        (when-not (some #{lvl} log-levels)
          (u/throw-ex (str "invalid log level - " lvl))))
      (assoc r :Disable levels))
    r))

(defn- parse-pagerthreshold-rule [r]
  (when-let [pt (:PagerThreshold r)]
    (when-not (map? pt)
      (u/throw-ex (str ":PagerThreshold must be a map - " pt)))
    (doseq [lvl (keys pt)]
      (when-not (some #{lvl} log-levels)
        (u/throw-ex (str "invalid log level in :PagerThreshold - " lvl)))
      (doseq [k (keys (lvl pt))]
        (when-not (some #{k} #{:count :duration-minutes})
          (u/throw-ex (str "invalid :PagerThreshold entry - " [lvl k]))))))
  r)

(defn- parse-hide-attribute-rule [r]
  (if-let [ns (:HideAttributes r)]
    (assoc
     r :HideAttributes
     (map #(do (when-not (li/name? %)
                 (u/throw-ex (str "invalid name in :HideAttributes - " %)))
               (let [[a b] (li/split-path %)
                     rs (li/split-ref b)]
                 [[a (first rs)] (rest rs)]))
          ns))
    r))

(defn compile-logging-rule
  "Parse a logging rule for validity, return the rule structure as is."
  [r _]
  (-> r
      parse-logging-rule-keys
      parse-logging-disable-rule
      parse-pagerthreshold-rule
      parse-hide-attribute-rule))
