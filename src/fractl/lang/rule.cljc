(ns fractl.lang.rule
  "Parsing and compilations of the embedded rule language"
  (:require [fractl.lang.internal :as li]))

(def ^:private oprs (concat li/cmpr-oprs [:and :or]))

(defn- operator? [x]
  (some #{x} oprs))

(defn- operator-name [x]
  (symbol (name x)))

(defn- accessor-expression [n]
  (let [parts (li/split-ref n)
        r (vec (rest parts))
        [a b :as ab] (li/split-path (first parts))
        p (if (and a b) ab (or a b))]
    `(get-in (get ~(symbol "-arg-map-") ~p) ~r)))

(defn compile-one-rule-pattern [pat]
  (map
   (fn [p]
     (cond
       (operator? p) (operator-name p)
       (li/name? p) (accessor-expression p)
       (vector? p) (compile-one-rule-pattern p)
       :else p))
   pat))

(defn compile-rule-pattern
  "Compile the dataflow match pattern into a predicate"
  [pat]
  (let [expr (compile-one-rule-pattern [pat])
        fexpr `(fn [~(symbol "-arg-map-")]
                 (try
                   ~@expr
                   (catch Exception ex#
                     (taoensso.timbre/error
                      "cannot execute conditional event predicate"
                      ex#)
                     nil)))]
    (eval fexpr)))
