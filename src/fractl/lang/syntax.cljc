(ns fractl.lang.syntax
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]))

(defn- get-spec-val [k spec]
  (let [v (get spec k :not-found)]
    (if (= v :not-found)
      (u/throw-ex (str "required key " k " not found"))
      v)))

(def tag :tag)
(def exp-fn :fn)
(def exp-args :args)

(def ^:private $fn (partial get-spec-val exp-fn))
(def ^:private $args (partial get-spec-val exp-args))
(def ^:private $tag (partial get-spec-val tag))

(defn- valid-arg? [x]
  (or (li/name? x) (li/literal? x)))

(defn exp
  "Return the intermediate representation (ir)
  for a compound expression - required keys are -
   :fn - function name, a symbol like '+
   :args - function arguments, a vector of attribute names,
           constant values etc"  
  ([spec]
   (when (not= 2 (count spec))
     (u/throw-ex (str "invalid compound-expression spec " spec)))
   (exp ($fn spec) ($args spec)))
  ([fnname args]
   (when-not (symbol? fnname)
     (u/throw-ex (str "fn-name must be a symbol - " fnname)))
   (when-not (every? valid-arg? args)
     (u/throw-ex (str "invalid argument in " args)))
   {tag :exp exp-fn fnname exp-args args}))

(defn- raw-exp [ir]
  `'(~(exp-fn ir) ~@(:args ir)))

(defn- introspect-exp [pattern]
  (let [p (if (= 'quote (first pattern))
            (second pattern)
            pattern)]
    (exp (first p) (vec (rest p)))))

(defn introspect [pattern]
  (if (seqable? pattern)
    (cond
      (list? pattern)
      (introspect-exp pattern)

      :else
      (u/throw-ex (str "invalid pattern " pattern)))))

(defn raw
  "Consume an intermediate representation object,
  return raw fractl syntax"
  [ir]
  (case ($tag ir)
    :exp (raw-exp ir)
    (u/throw-ex (str "invalid expression tag - " (:tag ir)))))
