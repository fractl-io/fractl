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
(def type-tag :type)
(def exp-fn-tag :fn)
(def exp-args-tag :args)
(def exp-tag :exp)
(def record-tag :record)
(def attrs-tag :attrs)
(def query-tag :query)
(def alias-tag :alias)
(def value-tag :value)
(def cases-tag :cases)
(def body-tag :body)
(def check-tag :check)

(def attributes attrs-tag)
(def query-pattern query-tag)

(def ^:private $fn (partial get-spec-val exp-fn-tag))
(def ^:private $args (partial get-spec-val exp-args-tag))
(def ^:private $exp (partial get-spec-val exp-tag))
(def ^:private $type (partial get-spec-val type-tag))
(def ^:private $record (partial get-spec-val record-tag))
(def ^:private $attrs (partial get-spec-val attrs-tag))
(def ^:private $value (partial get-spec-val value-tag))
(def ^:private $cases (partial get-spec-val cases-tag))
(def ^:private $body (partial get-spec-val body-tag))
(def ^:private $query (partial get-spec-val query-tag))

(defn as-syntax-object [t obj]
  (assoc obj type-tag t syntax-object-tag true))

(defn syntax-object? [obj]
  (when (map? obj)
    (syntax-object-tag obj)))

(defn has-type? [t obj]
  (and (syntax-object? obj)
       (= t (type-tag obj))))

(defn- valid-arg? [x]
  (or (li/name? x) (li/literal? x)))

(defn- invalid-arg [x]
  (when-not (valid-arg? x)
    x))

(defn- maybe-alias? [x]
  (if-not x
    true
    (or (li/name? x) (every? li/name? x))))

(defn- validate-alias! [x]
  (when-not (maybe-alias? x)
    (u/throw-ex (str "invalid alias - " x))))

(defn- mark [x]
  ['--> x '<--])

(defn- mark-exp-error [fnname args mark-at]
  (loop [exp `(~fnname ~@args), result []]
    (if-let [x (first exp)]
      (if (= x mark-at)
        (concat result [(mark x)] (rest exp))
        (recur (rest exp) (conj result x)))
      (seq result))))

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
     (u/throw-ex
      (str
       "fn-name must be a symbol - "
       (mark-exp-error fnname args fnname))))
   (when-let [invarg (some invalid-arg args)]
     (u/throw-ex
      (str
       "invalid argument in "
       (mark-exp-error args))))
   (as-syntax-object
    :exp
    {exp-fn-tag fnname
     exp-args-tag args})))

(def exp? (partial has-type? :exp))

(defn- raw-exp [ir]
  `'(~(exp-fn-tag ir) ~@(exp-args-tag ir)))

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
           (exp? v))))

(defn- query-attr-name [n]
  (when (li/query-pattern? n)
    n))

(defn- mark-attr-name [attrs name-to-mark]
  (let [rs (mapv (fn [[k v]]
                   [(if (= k name-to-mark)
                      (mark k)
                      k)
                    v])
                 attrs)]
    (into {} rs)))

(declare introspect introspect-attrs)

(defn upsert
  ([spec]
   (let [cnt (count spec)]
     (when (or (< cnt 2) (> cnt 3))
       (u/throw-ex (str "invalid upsert spec - " spec))))
   (upsert ($record spec) ($attrs spec) (alias-tag spec)))
  ([recname rec-attrs rec-alias]
   (when-not (li/name? recname)
     (u/throw-ex (str "invalid record name - " recname)))
   (when (li/query-pattern? recname)
     (u/throw-ex (str "looks like a query-only pattern - " recname)))
   (when-not (and (map? rec-attrs) (every? valid-attr-spec? rec-attrs))
     (u/throw-ex (str "invalid attribute spec - " rec-attrs)))
   (when-let [aname (some query-attr-name (keys rec-attrs))]
     (u/throw-ex
      (str "query attributes cannot be specified in upsert - "
           (mark-attr-name rec-attrs aname))))
   (validate-alias! rec-alias)
   (as-syntax-object
    :upsert
    (merge
     {record-tag recname
      attrs-tag (introspect-attrs rec-attrs)}
     (when rec-alias
       {alias-tag rec-alias})))))

(def upsert? (partial has-type? :upsert))

(defn- raw-upsert [ir]
  (merge
   {($record ir)
    (raw-walk ($attrs ir))}
   (when-let [als (alias-tag ir)]
     {alias-tag als})))

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

(defn query-upsert
  ([spec]
   (let [cnt (count spec)]
     (when (or (< cnt 2) (> cnt 3))
       (u/throw-ex (str "invalid query spec - " spec))))
   (let [attrs (attributes spec)
         query-pat (when-not attrs (query-pattern spec))]
     (when-not (or attrs query-pat)
       (u/throw-ex (str "no valid query-pattern or attributes - " spec)))
     (query-upsert
      ($record spec)
      (if attrs
        {attributes attrs}
        {query-pattern query-pat})
      (alias-tag spec))))
  ([recname attrs-or-query-pat rec-alias]
   (let [attrs-or-query-pat (normalize-query-attrs attrs-or-query-pat)]
     (when-not (or (li/query-pattern? recname)
                   (query-attrs? attrs-or-query-pat))
       (u/throw-ex
        (str "not a valid query pattern - " {recname attrs-or-query-pat})))
     (validate-alias! rec-alias)
     (as-syntax-object
      :query-upsert
      (merge
       {record-tag (if (query-pattern attrs-or-query-pat)
                     (query-record-name recname)
                     recname)}
       (if-let [attrs (attributes attrs-or-query-pat)]
         {attrs-tag (introspect-attrs attrs)}
         attrs-or-query-pat)
       (when rec-alias
         {alias-tag rec-alias}))))))

(def query-upsert? (partial has-type? :query-upsert))

(defn- raw-query-upsert [ir]
  (let [obj (or (attributes ir)
                (query-pattern ir))]
    (when-not obj
      (u/throw-ex (str "expected query attributes or pattern not found - " ir)))
    (merge
     {($record ir)
      (raw-walk obj)}
     (when-let [als (alias-tag ir)]
       {alias-tag als}))))

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
      ((if qpat query-upsert upsert)
       recname formatted-attrs
       (alias-tag pattern)))))

(def as-query-object (partial as-syntax-object :query-object))
(def query-object? (partial has-type? :query-object))
(def query-object (comp as-query-object query-upsert))
(def raw-query-object raw-query-upsert)
(def introspect-query-object (partial as-query-object introspect-query-upsert))

(defn- verify-cases! [cs]
  (loop [cs cs]
    (when-let [[k v :as c] (first cs)]
      (when (and (nil? v) (seq (rest cs)))
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

(defn match
  ([spec]
   (match ($value spec) ($cases spec) (alias-tag spec)))
  ([valpat cases match-alias]
   (when-not (or (li/name? valpat)
                 (li/literal? valpat))
     (u/throw-ex (str "invalid match value - " valpat)))
   (validate-alias! match-alias)
   (verify-cases! cases)
   (as-syntax-object
    :match
    {value-tag (introspect valpat)
     cases-tag (introspect-case-vals cases)
     alias-tag match-alias})))

(def match? (partial has-type? :match))

(def ^:private not-as #(not= :as %))

(defn- upto-alias [exps]
  (take-while not-as exps))

(defn- special-form-alias [obj]
  (second (drop-while not-as obj)))

(defn- raw-special-form [ir cmd-vec]
  (vec
   (concat
    cmd-vec
    (when-let [a (alias-tag ir)]
      [:as a]))))

(defn- extract-match-cases [obj]
  (let [cs (take-while not-as obj)]
    (partition-all 2 cs)))

(defn- introspect-match [obj]
  (let [body (nthrest obj 2)]
    (match (second obj)
           (extract-match-cases body)
           (special-form-alias body))))

(defn- raw-case [[k v]]
  (if v
    [(raw k) (raw-walk v)]
    [(raw-walk k)]))

(defn- raw-match [ir]
  (raw-special-form
   ir
   `[:match ~(raw ($value ir))
     ~@(vec (flatten (mapv raw-case ($cases ir))))]))

(defn- name-or-map? [x]
  (or (map? x) (li/name? x)))

(defn for-each
  ([spec]
   (for-each ($value spec) ($body spec) (alias-tag spec)))
  ([valpat body val-alias]
   (validate-alias! val-alias)
   (when-not (name-or-map? valpat)
     (u/throw-ex (str "invalid value pattern in for-each - " valpat)))
   (when-not (every? name-or-map? body)
     (u/throw-ex (str "invalid for-each body - " body)))
   (as-syntax-object
    :for-each
    {value-tag (introspect valpat)
     body-tag (mapv introspect body)
     alias-tag val-alias})))

(def for-each? (partial has-type? :for-each))

(def ^:private for-each-body upto-alias)
(def ^:private for-each-alias special-form-alias)

(defn- introspect-for-each [obj]
  (let [valpat (second obj)
        exps (rest (rest obj))
        body (for-each-body exps)
        als (for-each-alias exps)]
    (for-each valpat body als)))

(defn- raw-for-each [ir]
  (raw-special-form
   ir
   `[:for-each
     ~(raw-walk ($value ir))
     ~@(mapv raw-walk ($body ir))]))

(defn- introspect-try-cases [cases]
  (mapv (fn [[k v]]
          (when-not (or (keyword? k)
                        (every? keyword? k))
            (u/throw-ex (str "invalid case in try - " k)))
          [k (introspect v)])
        cases))

(defn _try
  ([spec]
   (_try ($body spec) ($cases spec) (alias-tag spec)))
  ([body cases val-alias]
   (when-not (name-or-map? body)
     (u/throw-ex (str "invalid body for try - " body)))
   (validate-alias! val-alias)
   (as-syntax-object
    :try
    {body-tag (introspect body)
     cases-tag (introspect-try-cases cases)
     alias-tag val-alias})))

(def try? (partial has-type? :try))

(defn- introspect-try [obj]
  (let [exps (rest obj)]
    (_try (first exps)
          (partition-all 2 (upto-alias (rest exps)))
          (special-form-alias (rest exps)))))

(defn- raw-try [ir]
  (raw-special-form
   ir
   `[:try
     ~(raw-walk ($body ir))
     ~@(apply concat (mapv (fn [[k v]] [k (raw-walk v)]) ($cases ir)))]))

(defn delete
  ([spec]
   (delete ($record spec) ($attrs spec) (alias-tag spec)))
  ([recname attrs result-alias]
   (when-not (li/name? recname)
     (u/throw-ex (str "invalid record-name in delete - " recname)))
   (when-not (and (map? attrs) (every? valid-attr-spec? attrs))
     (u/throw-ex (str "invalid attribute spec in delete - " attrs)))
   (validate-alias! result-alias)
   (as-syntax-object
    :delete
    {record-tag recname
     attrs-tag (introspect-attrs attrs)
     alias-tag result-alias})))

(def delete? (partial has-type? :delete))

(defn- introspect-delete [obj]
  (delete (second obj)
          (nth obj 2)
          (special-form-alias obj)))

(defn- raw-delete [ir]
  (raw-special-form
   ir
   [:delete ($record ir)
    (raw-walk ($attrs ir))]))

(defn query
  ([spec]
   (query ($query spec) (alias-tag spec)))
  ([query-pat result-alias]
   (when-not (or (li/name? query-pat) (map? query-pat))
     (u/throw-ex (str "invalid query - " query-pat)))
   (validate-alias! result-alias)
   (as-syntax-object
    :query
    {query-tag
     (cond
       (query-object? query-pat) query-pat
       (query-upsert? query-pat) query-pat
       (map? query-pat)
       (let [recname (first (keys query-pat))]
         (query-object recname (recname query-pat) nil))
       :else
       query-pat)
     alias-tag result-alias})))

(def query? (partial has-type? :query))

(defn- introspect-query [obj]
  (query (second obj) (special-form-alias obj)))

(defn- raw-query [ir]
  (raw-special-form
   ir
   [:query
    (raw-walk ($query ir))]))

(defn _eval
  ([spec]
   (_eval ($exp spec) (check-tag spec) (alias-tag spec)))
  ([exp check result-alias]
   (when-not (li/name? check)
     (u/throw-ex (str "invalid value for check - " check)))
   (validate-alias! result-alias)
   (as-syntax-object
    :eval
    {exp-tag (introspect exp)
     check-tag check
     alias-tag result-alias})))

(def eval? (partial has-type? :eval))

(def ^:private not-check #(not= :check %))

(defn- introspect-eval [obj]
  (_eval (second obj)
         (second (drop-while not-check obj))
         (special-form-alias obj)))

(defn- raw-eval [ir]
  (raw-special-form
   ir
   (vec
    (concat
     [:eval (raw ($exp ir))]
     (when-let [c (check-tag ir)]
       [:check c])))))

(defn _await [spec]
  )

(defn- introspect-await [obj]
  )

(defn entity [spec]
  )

(defn- introspect-entity [obj]
  )

(def special-form-introspectors
  {:match introspect-match
   :try introspect-try
   :for-each introspect-for-each
   :query introspect-query
   :delete introspect-delete
   :await introspect-await
   :eval introspect-eval
   :entity introspect-entity})

(defn- introspect-attrs [attrs]
  (let [rs (mapv (fn [[k v]]
                   [k (introspect v)])
                 attrs)]
    (into {} rs)))

(defn- introspect-special-form [pat]
  (if-let [h (special-form-introspectors (first pat))]
    (h pat)
    pat))

(defn introspect [pattern]
  (cond
    (syntax-object? pattern) pattern
    (seqable? pattern)
    (cond
      (or (list? pattern) (= 'quote (first pattern)))
      (introspect-exp pattern)

      (map? pattern)
      (introspect-query-upsert pattern)

      (vector? pattern)
      (introspect-special-form pattern)

      :else pattern)
    :else pattern))

(def introspect-json (comp introspect json/decode))
(def introspect-transit (comp introspect t/decode))

(def ^:private raw-handler
  {:exp raw-exp
   :upsert raw-upsert
   :query-upsert raw-query-upsert
   :query-object raw-query-object
   :match raw-match
   :for-each raw-for-each
   :try raw-try
   :query raw-query
   :delete raw-delete
   :eval raw-eval})

(defn raw
  "Consume an intermediate representation object,
  return raw fractl syntax"
  [ir]
  (if (syntax-object? ir)
    (if-let [h (raw-handler ($type ir))]
      (h ir)
      (u/throw-ex (str "invalid syntax-object tag - " (type-tag ir))))
    ir))

(def raw-json (comp json/encode raw))
(def raw-transit (comp t/encode raw))
