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
(def attributes :attrs)
(def query-pattern :query)
(def alias-name :alias)

(def ^:private $fn (partial get-spec-val exp-fn))
(def ^:private $args (partial get-spec-val exp-args))
(def ^:private $tag (partial get-spec-val tag))
(def ^:private $record (partial get-spec-val record))
(def ^:private $attrs (partial get-spec-val attributes))

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

(defn upsert
  ([spec]
   (let [cnt (count spec)]
     (when (or (< cnt 2) (> cnt 3))
       (u/throw-ex (str "invalid upsert spec - " spec))))
   (upsert ($record spec) ($attrs spec) (alias-name spec)))
  ([recname rec-attrs rec-alias]
   (when-not (li/name? recname)
     (u/throw-ex (str "invalid record name - " recname)))
   (when (li/query-pattern? recname)
     (u/throw-ex (str "looks like a query-only pattern - " recname)))
   (when-not (and (map? rec-attrs) (every? valid-attr-spec? rec-attrs))
     (u/throw-ex (str "invalid attribute spec - " rec-attrs)))
   (when (some li/query-pattern? (keys rec-attrs))
     (u/throw-ex (str "query attributes cannot be specified in upsert - " rec-attrs)))
   (when (and rec-alias (not (li/name? rec-alias)))
     (u/throw-ex (str "invalid alias - " rec-alias)))
   (merge
    {tag :upsert record recname
     attributes rec-attrs}
    (when rec-alias
      {alias-name rec-alias}))))

(defn- raw-upsert [ir]
  (merge
   {($record ir)
    (into {} ($attrs ir))}
   (when-let [als (alias-name ir)]
     {:alias als})))

(defn- query-attrs? [obj]
  (if-let [attrs (attributes obj)]
    (some li/query-pattern? (keys attrs))
    (:where (query-pattern obj))))

(defn query
  ([spec]
   (let [cnt (count spec)]
     (when (or (< cnt 2) (> cnt 3))
       (u/throw-ex (str "invalid query spec - " spec))))
   (let [attrs (attributes spec)
         query-pat (when-not attrs (query-pattern spec))]
     (when-not (and attrs query-pat)
       (u/throw-ex (str "no valid query-pattern or attributes - " spec)))
     (query
      ($record spec)
      (if attrs
        {attributes attrs}
        {query-pattern query-pat})
      (alias-name spec))))
  ([recname attrs-or-query-pat rec-alias]
   (when-not (or (li/query-pattern? recname)
                 (query-attrs? attrs-or-query-pat))
     (u/throw-ex
      (str "not a valid query pattern - " [recname attrs-or-query-pat])))
   (when (and rec-alias (not (li/name? rec-alias)))
     (u/throw-ex (str "invalid alias - " rec-alias)))
   (merge
    {tag :query
     record recname}
    attrs-or-query-pat
    (when rec-alias
      {alias-name rec-alias}))))

(defn- introspect-query-upsert [pattern]
  (let [pat (li/normalize-upsert-pattern pattern)
        recname (first (keys pat))
        attrs (recname pat)]
    (when-not (li/name? recname)
      (u/throw-ex (str "invalid record name - " recname)))
    (when-not (map? attrs)
      (u/throw-ex (str "attributes must be a map - " attrs)))
    ((if (or (li/query-pattern? recname)
             (some li/query-pattern? (keys attrs))
             (:where attrs))
       query upsert)
     recname attrs (alias-name pattern))))

(defn introspect [pattern]
  (if (seqable? pattern)
    (cond
      (list? pattern)
      (introspect-exp pattern)

      (map? pattern)
      (introspect-query-upsert pattern)

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
