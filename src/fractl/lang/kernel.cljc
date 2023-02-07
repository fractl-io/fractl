(ns fractl.lang.kernel
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.lang.datetime :as dt]
            [fractl.component :as cn]))

(defn kernel-string?
  ([s rgex-s]
   (re-matches (re-pattern rgex-s) s))
  ([s] (string? s)))

(defn kernel-float? [x]
  #?(:clj
     (instance? Float x)
     :cljs
     (float? x)))

(defn kernel-double? [x]
  #?(:clj
     (instance? Double x)
     :cljs
     (float? x)))

(def date-time? dt/parse-date-time)
(def date? dt/parse-date)
(def time? dt/parse-time)

(defn UUID? [s]
  (or (u/uuid-from-string s) (uuid? s)))

(def any-obj? (constantly true))

(defn- edn? [x]
  (or (vector? x) (map? x)
      (symbol? x) (keyword? x)
      (string? x) (number? x)
      (boolean? x) (nil? x)
      (list? x) (set? x)))

(defn- path?
  "Encode a path in a fractl record. Examples:
     :C, :C/E, :C/E.R. Paths may also be represented
   as strings - \"C/E.R\""
  [x]
  (let [k (cond
            (string? x)
            (keyword x)

            (vector? x)
            (map #(if (string? %)
                    (keyword %)
                    %)
                 x)
            :else x)]
    (every?
     li/name?
     (li/split-path k))))

(def ^:private email-pattern
  #"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")

(defn email? [x]
  (and (string? x)
       (re-matches email-pattern x)))

(def types
  {:Kernel.Lang/String kernel-string?
   :Kernel.Lang/Keyword #(or (keyword? %) (string? %))
   :Kernel.Lang/Path path?
   :Kernel.Lang/DateTime date-time?
   :Kernel.Lang/Date date?
   :Kernel.Lang/Time time?
   :Kernel.Lang/UUID UUID?
   :Kernel.Lang/Int int?
   :Kernel.Lang/Int64 int?
   :Kernel.Lang/BigInteger integer?
   :Kernel.Lang/Float kernel-float?
   :Kernel.Lang/Double kernel-double?
   :Kernel.Lang/Decimal cn/decimal-value?
   :Kernel.Lang/Boolean boolean?
   :Kernel.Lang/Record cn/record-instance?
   :Kernel.Lang/Entity cn/entity-instance?
   :Kernel.Lang/Event cn/event-instance?
   :Kernel.Lang/Any any-obj?
   :Kernel.Lang/Email email?
   :Kernel.Lang/Password kernel-string?
   :Kernel.Lang/Map map?
   :Kernel.Lang/Edn edn?})

(def ^:private type-names (keys types))

(def ^:private plain-types
  (into {} (mapv (fn [t] [(second (li/split-path t)) t]) type-names)))

(defn kernel-type? [n]
  (some #{n} type-names))

(defn plain-kernel-type? [n]
  (n plain-types))

(defn normalize-kernel-type [t]
  (or (t plain-types) t))

(defn find-root-attribute-type [n]
  (if (kernel-type? n)
    n
    (when-let [ascm (cn/find-attribute-schema n)]
      (cond
        (:listof ascm)
        :Kernel.Lang/List

        (:oneof ascm)
        :Kernel.Lang/String

        :else
        (when-let [t (if (map? ascm) (:type ascm) ascm)]
          (if (kernel-type? t)
            t
            (find-root-attribute-type t)))))))

(def type-predicate first)
(def type-default-value second)

(def ^:private event-context-type [:Kernel.Lang/EventContext
                                   {:type :Kernel.Lang/Map
                                    :optional true}])

(defn event-context-attribute-name []
  (first event-context-type))

(defn event-context-attribute-schema []
  (second event-context-type))
