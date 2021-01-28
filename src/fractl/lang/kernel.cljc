(ns fractl.lang.kernel
  (:require [fractl.util :as u]
            [fractl.lang.datetime :as dt]
            [fractl.component :as cn]
            #?(:cljs [reagent.core :as r])))

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

(def ^:private email-pattern
  #"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?")

(defn email? [x]
  (and (string? x)
       (re-matches email-pattern x)))

(defn- cell? [x]
  #?(:clj
     (= (type x) clojure.lang.Atom)
     :cljs
     (= (type x) reagent.ratom/RAtom)))

(defn cell [v]
  #?(:clj
     (atom v)
     :cljs
     (r/atom v)))

(def types
  {:Kernel/String kernel-string?
   :Kernel/Keyword keyword?
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
   :Kernel/Edn edn?
   :Kernel/Cell cell?})

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
