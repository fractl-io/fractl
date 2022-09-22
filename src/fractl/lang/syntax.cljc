(ns fractl.lang.syntax
  (:require [clojure.walk :as w]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.datafmt.json :as json]
            [fractl.datafmt.transit :as t]))

(defn- get-spec-val [k spec]
  (let [v (get spec k :not-found)]
    (if (= v :not-found)
      (u/throw-ex (str "required key " k " not found"))
      v)))

(def syntax-object-tag :-*-syntax-*-)
(def tag :tag)
(def exp-fn :fn)
(def exp-args :args)
(def record :record)
(def attributes :attrs)
(def query-pattern :query)
(def alias-name :alias)
(def cases :cases)

(def ^:private $fn (partial get-spec-val exp-fn))
(def ^:private $args (partial get-spec-val exp-args))
(def ^:private $tag (partial get-spec-val tag))
(def ^:private $record (partial get-spec-val record))
(def ^:private $attrs (partial get-spec-val attributes))
(def ^:private $cases (partial get-spec-val cases))

(defn as-syntax-object [tg obj]
  (assoc obj tag tg syntax-object-tag true))

(defn syntax-object? [obj]
  (when (map? obj)
    (syntax-object-tag obj)))

(defn- valid-arg? [x]
  (or (li/name? x) (li/literal? x)))

(defn- alias? [x]
  (or (li/name? x) (every? li/name? x)))

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
   (as-syntax-object
    :exp
    {exp-fn fnname
     exp-args args})))

(defn exp-object? [x]
  (and (map? x) (= :exp (tag x))))

(defn- raw-exp [ir]
  `'(~(exp-fn ir) ~@(:args ir)))

(declare raw)

(defn- raw-walk [obj]
  (w/postwalk raw obj))

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

(declare introspect introspect-attrs)

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
   (when (and rec-alias (not (alias? rec-alias)))
     (u/throw-ex (str "invalid alias - " rec-alias)))
   (as-syntax-object
    :upsert
    (merge
     {record recname
      attributes (introspect-attrs rec-attrs)}
     (when rec-alias
       {alias-name rec-alias})))))

(defn- raw-upsert [ir]
  (merge
   {($record ir)
    (raw-walk ($attrs ir))}
   (when-let [als (alias-name ir)]
     {alias-name als})))

(defn- query-attrs? [obj]
  (if-let [attrs (attributes obj)]
    (some li/query-pattern? (keys attrs))
    (:where (query-pattern obj))))

(defn- normalize-query-attrs [attrs]
  (cond
    (or (attributes attrs) (query-pattern attrs))
    attrs

    (:where attrs) {query-pattern attrs}
    :else {attributes attrs}))

(defn- query-record-name [recname]
  (if (li/query-pattern? recname)
    recname
    (li/name-as-query-pattern recname)))

(defn query
  ([spec]
   (let [cnt (count spec)]
     (when (or (< cnt 2) (> cnt 3))
       (u/throw-ex (str "invalid query spec - " spec))))
   (let [attrs (attributes spec)
         query-pat (when-not attrs (query-pattern spec))]
     (when-not (or attrs query-pat)
       (u/throw-ex (str "no valid query-pattern or attributes - " spec)))
     (query
      ($record spec)
      (if attrs
        {attributes attrs}
        {query-pattern query-pat})
      (alias-name spec))))
  ([recname attrs-or-query-pat rec-alias]
   (let [attrs-or-query-pat (normalize-query-attrs attrs-or-query-pat)]
     (when-not (or (li/query-pattern? recname)
                   (query-attrs? attrs-or-query-pat))
       (u/throw-ex
        (str "not a valid query pattern - " {recname attrs-or-query-pat})))
     (when (and rec-alias (not (alias? rec-alias)))
       (u/throw-ex (str "invalid alias - " rec-alias)))
     (as-syntax-object
      :query
      (merge
       {record (if (query-pattern attrs-or-query-pat)
                 (query-record-name recname)
                 recname)}
       (if-let [attrs (attributes attrs-or-query-pat)]
         {attributes (introspect-attrs attrs)}
         attrs-or-query-pat)
       (when rec-alias
         {alias-name rec-alias}))))))

(defn- raw-query [ir]
  (let [obj (or (attributes ir)
                (query-pattern ir))]
    (when-not obj
      (u/throw-ex (str "expected query attributes or pattern not found - " ir)))
    (merge
     {($record ir)
      (raw-walk obj)}
     (when-let [als (alias-name ir)]
       {alias-name als}))))

(defn- introspect-query-upsert [pattern]
  (let [pat (li/normalize-upsert-pattern pattern)
        recname (first (keys pat))
        attrs (recname pat)]
    (when-not (li/name? recname)
      (u/throw-ex (str "invalid record name - " recname)))
    (when-not (map? attrs)
      (u/throw-ex (str "attributes must be a map - " attrs)))
    (let [is-where-clause (:where attrs)
          qpat (or is-where-clause
                   (li/query-pattern? recname)
                   (some li/query-pattern? (keys attrs)))
          formatted-attrs
          (cond
            is-where-clause {query-pattern attrs}
            qpat {attributes attrs}
            :else attrs)]
      ((if qpat query upsert)
       recname formatted-attrs
       (alias-name pattern)))))

(defn- verify-cases! [cs]
  (loop [cs cs]
    (when-let [[k v :as c] (first cs)]
      (when (and (nil? v) (not (seq (rest cs))))
        (u/throw-ex (str "default case must be the last entry - " k)))
      (when (and v (not (or (li/name? k) (li/literal? k))))
        (u/throw-ex (str "invalid key " k " in " c)))
      (recur (rest cs)))))

(defn- introspect-case-vals [cs]
  (mapv (fn [[k v]]
          (if (nil? v)
            [(introspect k)]
            [k (introspect v)]))
        cs))

(defn _match [spec]
  (let [cs ($cases spec)]
    (verify-cases! cs)
    (as-syntax-object
     :match
     {cases (introspect-case-vals cs)})))

(defn _for-each
  "{:header <introspectable-pattern>
    :body <introspectable-pattern>
    :alias alias?}"
  [spec]
  )

(defn _try
  "{:body <introspectable-pattern>
    :cases <same-as-match-cases>
    :alias alias?}"
  [spec]
  )

(defn _delete
  "{:record name?
    :attrs map-of-names
    :alias alias?}"
  [spec]
  )

(defn _query
  "{:query name-or-where-query-pattern
    :alias alias?}"
  [spec]
  )

(defn _eval
  "{:exp syntax-object-exp
    :check name?
    :alias alias?}"
  [spec]
  )

(defn _await [spec]
  )

(defn _entity [spec]
  )

(defn- introspect-attrs [attrs]
  (let [rs (mapv (fn [[k v]]
                   [k (introspect v)])
                 attrs)]
    (into {} rs)))

(defn introspect [pattern]
  (cond
    (syntax-object? pattern) pattern
    (seqable? pattern)
    (cond
      (or (list? pattern) (= 'quote (first pattern)))
      (introspect-exp pattern)

      (map? pattern)
      (introspect-query-upsert pattern)

      :else pattern)
    :else pattern))

(def introspect-json (comp introspect json/decode))
(def introspect-transit (comp introspect t/decode))

(defn raw
  "Consume an intermediate representation object,
  return raw fractl syntax"
  [ir]
  (if (syntax-object? ir)
    (case ($tag ir)
      :exp (raw-exp ir)
      :upsert (raw-upsert ir)
      :query (raw-query ir)
      (u/throw-ex (str "invalid syntax-object tag - " (tag ir))))
    ir))

(def raw-json (comp json/encode raw))
(def raw-transit (comp t/encode raw))
