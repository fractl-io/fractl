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
(def record :record)
(def attrs :attrs)
(def alias-name :alias)

(def ^:private $fn (partial get-spec-val exp-fn))
(def ^:private $args (partial get-spec-val exp-args))
(def ^:private $tag (partial get-spec-val tag))
(def ^:private $record (partial get-spec-val record))
(def ^:private $attrs (partial get-spec-val attrs))

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

(defn exp-object? [x]
  (and (map? x) (= :exp (tag x))))

(defn- raw-exp [ir]
  `'(~(exp-fn ir) ~@(:args ir)))

(defn- introspect-exp [pattern]
  (let [p (if (= 'quote (first pattern))
            (second pattern)
            pattern)]
    (exp (first p) (vec (rest p)))))

(defn- valid-attr-spec? [[k v]]
  (and (li/name? k)
       (or (li/name? v)
           (li/literal? v)
           (exp-object? v))))

(defn upsert [spec]
  (let [cnt (count spec)]
    (when (or (< cnt 2) (> cnt 3))
      (u/throw-ex (str "invalid upsert spec - " spec))))
  (let [recname ($record spec)
        ats ($attrs spec)
        als (alias-name spec)]
    (when-not (li/name? recname)
      (u/throw-ex (str "invalid record name - " recname)))
    ;; TODO: check if recname is query - if so raise error
    (when-not (and (map? ats) (every? valid-attr-spec? ats))
      (u/throw-ex (str "invalid attribute spec - " ats)))
    (when als
      (when-not (li/name? als)
        (u/throw-ex (str "invalid alias - " als))))
    ;; TODO: if any name in ats is query, make tag :`query-upsert
    ;; if all names in ats are queries, raise error
    (merge {tag :upsert} spec)))

(defn- raw-upsert [ir]
  (merge
   {($record ir)
    (into {} ($attrs ir))}
   (when-let [als (alias-name ir)]
     {:alias als})))

(defn introspect [pattern]
  (if (seqable? pattern)
    (cond
      (list? pattern)
      (introspect-exp pattern)

      ;; TODO: upsert query-upsert introspection
      :else
      (u/throw-ex (str "invalid pattern " pattern)))))

(defn raw
  "Consume an intermediate representation object,
  return raw fractl syntax"
  [ir]
  (case ($tag ir)
    :exp (raw-exp ir)
    :upsert (raw-upsert ir)
    (u/throw-ex (str "invalid expression tag - " (:tag ir)))))
