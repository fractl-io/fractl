(ns fractl.lang.kernel
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.lang.datetime :as dt]
            [fractl.component :as cn]))

(def kernel-lang-component :Fractl.Kernel.Lang)

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

(defn edn? [x]
  (or (vector? x) (map? x)
      (symbol? x) (keyword? x)
      (string? x) (number? x)
      (boolean? x) (nil? x)
      (list? x) (set? x)))

(defn path?
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

(def numeric-types
  [:Fractl.Kernel.Lang/Int
   :Fractl.Kernel.Lang/Int64
   :Fractl.Kernel.Lang/BigInteger
   :Fractl.Kernel.Lang/Float
   :Fractl.Kernel.Lang/Double
   :Fractl.Kernel.Lang/Decimal])

;; TODO: load types from the kernel model by calling
;; appropriate component namespace (cn) functions
(def type-names
  (concat
   numeric-types
   [:Fractl.Kernel.Lang/String
    :Fractl.Kernel.Lang/Keyword
    :Fractl.Kernel.Lang/Path
    :Fractl.Kernel.Lang/DateTime
    :Fractl.Kernel.Lang/Date
    :Fractl.Kernel.Lang/Time
    :Fractl.Kernel.Lang/UUID
    :Fractl.Kernel.Lang/Boolean
    :Fractl.Kernel.Lang/Record
    :Fractl.Kernel.Lang/Entity
    :Fractl.Kernel.Lang/Event
    :Fractl.Kernel.Lang/Any
    :Fractl.Kernel.Lang/Email
    :Fractl.Kernel.Lang/Password
    :Fractl.Kernel.Lang/Map
    :Fractl.Kernel.Lang/Edn
    :Fractl.Kernel.Lang/EventContext
    :Fractl.Kernel.Lang/Identity
    :Fractl.Kernel.Lang/Now]))

(def ^:private plain-types
  (into {} (mapv (fn [t] [(second (li/split-path t)) t]) type-names)))

(defn kernel-type? [n]
  (some #{n} type-names))

(defn plain-kernel-type? [n]
  (n plain-types))

(defn normalize-kernel-type [t]
  (or (t plain-types) t))

(defn numeric-type? [t]
  (if t
    (if (some #{t} numeric-types)
      true
      false)
    false))

(defn find-root-attribute-type [n]
  (if (kernel-type? n)
    n
    (when-let [ascm (cn/find-attribute-schema n)]
      (cond
        (:listof ascm)
        :Fractl.Kernel.Lang/List

        (:oneof ascm)
        :Fractl.Kernel.Lang/String

        :else
        (when-let [t (if (map? ascm) (:type ascm) ascm)]
          (if (kernel-type? t)
            t
            (find-root-attribute-type t)))))))

(def type-predicate first)
(def type-default-value second)

(def ^:private event-context-type [:Fractl.Kernel.Lang/EventContext
                                   {:type :Fractl.Kernel.Lang/Map
                                    :optional true}])

(defn event-context-attribute-name []
  (first event-context-type))

(defn event-context-attribute-schema []
  (second event-context-type))
