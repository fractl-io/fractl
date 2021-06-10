(ns fractl.policy.logging-util
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]))

(def log-levels [:DEBUG :INFO :WARN :ERROR])

(defn- validate-logging-rule-keys [r]
  (doseq [k (keys r)]
    (when-not (some #{k} #{:Disable :PagerThreshold :ExcludeAttributes})
      (u/throw-ex (str "invalid logging rule - " k))))
  r)

(defn- validate-logging-disable-rule [r]
  (when-let [levels (:Disable r)]
    (let [levels (if (keyword? levels) [levels] levels)]
      (doseq [lvl levels]
        (when-not (some #{lvl} log-levels)
          (u/throw-ex (str "invalid log level - " lvl))))))
  r)

(defn- validate-pagerthreshold-rule [r]
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

(defn- validate-exclude-attribute-rule [r]
  (when-let [ea (:ExcludeAttributes r)]
    (doseq [n ea]
      (when-not (li/name? n)
        (u/throw-ex (str "invalid name in :ExcludeAttributes - " n)))))
  r)

(defn compile-logging-rule
  "Parse a logging rule for validity, return the rule structure as is."
  [r]
  (-> r
      validate-logging-rule-keys
      validate-logging-disable-rule
      validate-pagerthreshold-rule
      validate-exclude-attribute-rule))
