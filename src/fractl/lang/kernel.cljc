(ns fractl.lang.kernel
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.lang.datetime :as dt]
            [fractl.component :as cn]))

(defn kernel-string?
  ([s rgex-s]
   (re-matches (re-pattern rgex-s) s))
  ([s] (string? s)))

(def date-time? dt/parse-date-time)

(defn UUID? [s]
  (if (u/uuid-from-string s) true false))

(def any-obj? (constantly true))

(defn- edn? [x]
  (or (vector? x) (map? x)
      (symbol? x) (keyword? x)
      (string? x) (number? x)
      (boolean? x) (nil? x)
      (list? x) (set? x)))

(defn kernel-decimal? [x]
  #?(:clj
     (and (bigdec x) true)
     :cljs
     (float? x)))

(defn kernel-decimal [x]
  #?(:clj
     (bigdec x)
     :cljs
     (float x)))

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
  {:Kernel/String kernel-string?
   :Kernel/Keyword keyword?
   :Kernel/Path path?
   :Kernel/DateTime date-time?
   :Kernel/UUID UUID?
   :Kernel/Password identity
   :Kernel/Int int?
   :Kernel/Int64 integer?
   :Kernel/Integer integer?
   :Kernel/Float float?
   :Kernel/Double double?
   :Kernel/Decimal kernel-decimal?
   :Kernel/Boolean boolean?
   :Kernel/Record cn/record-instance?
   :Kernel/Entity cn/entity-instance?
   :Kernel/Event cn/event-instance?
   :Kernel/Any any-obj?
   :Kernel/Email email?
   :Kernel/Map map?
   :Kernel/Edn edn?})

(def ^:private type-names (keys types))

(defn kernel-type? [n]
  (some #{n} type-names))

(def type-predicate first)
(def type-default-value second)

(def ^:private kernel-bindings #{:String :DateTime :UUID
                                 :Int :Int64 :Integer
                                 :Float :Double :Decimal
                                 :Boolean :Record :Entity :Event})

(defn kernel-binding? [n]
  (some #{n} kernel-bindings))

(def ^:private event-context-type [:Kernel/_EventContext
                                   {:type :Kernel/Map
                                    :optional true}])

(defn event-context-attribute-name []
  (first event-context-type))

(defn event-context-attribute-schema []
  (second event-context-type))
