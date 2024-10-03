(ns agentlang.inference.service.planner
  (:require [agentlang.util :as u]
            [agentlang.lang.internal :as li]))

(defn- validate-record-expr [[n attrs :as expr] alias]
  (when-not (li/name? n)
    (u/throw-ex (str "Invalid record name in " expr)))
  (when-not (map? attrs)
    (u/throw-ex (str "Attributes must be a map: " expr)))
  (when alias
    (if (vector? alias)
      (when-not (every? keyword? alias)
        (u/throw-ex (str "Invalid alias " alias " for expression " expr)))
      (when-not (li/name? alias)
        (u/throw-ex (str "Invalid alias " alias " for expression " expr)))))
  n)

(defn- rename-attrs-for-query [attrs]
  ;; TODO: rename attr-keys with a ?
  )

(defn- parse-value-refs-and-exprs [attrs]
  ;; TODO: parse attributes refs like `(:AttrName var)` and comparison expressions like `[= 500]` and `[> (:AttrName varname)]`
  )

(defn- parse-make [[n attrs :as expr] alias]
  (when (validate-record-expr expr alias)
    {n (parse-value-refs-and-exprs attrs) :as alias}))

(defn- parse-lookup [[n attrs :as expr] alias]
  (when (validate-record-expr expr alias)
    {n (parse-value-refs-and-exprs (rename-attrs-for-query attrs)) :as alias}))

(defn- parse-lookup-one [expr alias]
  (parse-lookup expr [alias]))

(def ^:private parse-lookup-many parse-lookup)

(defn- parse-cond [expr alias]
  ;; TODO parse conditional expression into a :match clause.
  )

(defn- parse-binding-error [expr _]
  (u/throw-ex (str "Cannot parse expression " expr)))

(defn- parse-binding [expr alias]
  ((case (first expr)
     make parse-make
     cond parse-cond
     lookup-one parse-lookup-one
     lookup-many parse-lookup-many
     parse-binding-error)
   (rest expr) alias))

(declare expressions-to-patterns)

(defn expression-to-pattern [expr]
  (if (seqable? expr)
    (case (first expr)
      def (parse-binding (nth expr 2) (second expr))
      cond (parse-cond (second expr 2) nil)
      do (expressions-to-patterns expr)
      (u/throw-ex (str "Cannot convert expression to pattern: " expr)))
    (u/throw-ex (str "Invalid expression: " expr))))

(defn expressions-to-patterns [exprs]
  (mapv expression-to-pattern (rest exprs)))
