(ns fractl.compiler.rule
  "Parsing and compilations of the embedded rule language"
  (:require [fractl.util.logger :as log]
            [fractl.lang.internal :as li]))

(defn in [xs x]
  (some #{x} xs))

(defn lt [a b]
  (= -1 (compare a b)))

(defn lteq [a b]
  (let [c (compare a b)]
    (or (= c 0) (= c -1))))

(defn gt [a b]
  (= 1 (compare a b)))

(defn gteq [a b]
  (let [c (compare a b)]
    (or (= c 0) (= c 1))))

(defn between [a b x]
  (and (gt x a)
       (lt x b)))

(defn- operator-name [x]
  (case x
    :< 'fractl.compiler.rule/lt
    :<= 'fractl.compiler.rule/lteq
    :> 'fractl.compiler.rule/gt
    :>= 'fractl.compiler.rule/gteq
    :in 'fractl.compiler.rule/in
    :between 'fractl.compiler.rule/between
    (symbol (name x))))

(defn- accessor-expression
  "Parse the name into a path and return an expression that
  fetches the value at the path from the runtime argument map.
  This expression is used for accessing arguments of the predicate,
  generated by compiling a rule."
  [n]
  (let [parts (li/split-ref n)
        r (vec (rest parts))
        [a b :as ab] (li/split-path (first parts))
        p (if (and a b) ab (or a b))]
    `(get-in (~(symbol "-arg-map-") ~p) ~r)))

(defn compile-one-rule-pattern [pat]
  (map
   (fn [p]
     (cond
       (li/operator? p) (operator-name p)
       (li/name? p) (accessor-expression p)
       (vector? p)
       (if (li/operator? (first p))
         (compile-one-rule-pattern p)
         p)
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
                     (log/error
                      (str "cannot execute conditional event predicate"
                           ex#))
                     nil)))]
    (eval fexpr)))
