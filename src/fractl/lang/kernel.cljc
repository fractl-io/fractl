(ns fractl.lang.kernel
  (:require [fractl.util :as u]
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

(def ^:private email-pattern
  #"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")

(defn email? [x]
  (and (string? x)
       (re-matches email-pattern x)))

(defn- kt [type-name predic default-value]
  [type-name [predic default-value]])

(def types
  (into {} [(kt :Kernel/String kernel-string? "")
            (kt :Kernel/Keyword keyword? :undef)
            (kt :Kernel/DateTime date-time? dt/now)
            (kt :Kernel/UUID UUID? u/uuid-string)
            (kt :Kernel/Int int? 0)
            (kt :Kernel/Int64 integer? 0)
            (kt :Kernel/Integer integer? 0)
            (kt :Kernel/Float float? 0.0)
            (kt :Kernel/Double double? 0.0)
            (kt :Kernel/Decimal kernel-decimal? (kernel-decimal 0.0))
            (kt :Kernel/Boolean boolean? false)
            (kt :Kernel/Record cn/record-instance? nil)
            (kt :Kernel/Entity cn/entity-instance? nil)
            (kt :Kernel/Event cn/event-instance? nil)
            (kt :Kernel/Any any-obj? {})
            (kt :Kernel/Email email? nil)
            (kt :Kernel/Map map? {})]))

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
