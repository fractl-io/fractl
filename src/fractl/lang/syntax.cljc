(ns fractl.lang.syntax
  (:require [clojure.walk :as w]
            [clojure.string :as s]
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
(def alias-tag :as)
(def value-tag :value)
(def cases-tag :cases)
(def body-tag :body)
(def check-tag :check)
(def path-tag :path)
(def name-tag :name)

(def rel-tag li/rel-tag)
(def timeout-ms-tag li/timeout-ms-tag)

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

(defn fn-name? [x]
  (or (symbol? x)
      (some #{x} li/oprs)))

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
   (when-not (fn-name? fnname)
     (u/throw-ex
      (str
       "fn-name must be a symbol - "
       (mark-exp-error fnname args fnname))))
   (when-let [invarg (some invalid-arg args)]
     (u/throw-ex
      (str
       "invalid argument in "
       (mark-exp-error fnname args fnname))))
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

(defn- introspect-rels [r]
  (mapv introspect r))

(defn- introspect-relationship [r]
  (if (vector? (first r))
    (mapv introspect-rels r)
    (u/throw-ex (str "invalid relationship object, expected vector - " (first r)))))

(defn upsert
  ([spec]
   (let [cnt (count spec)]
     (when (or (< cnt 2) (> cnt 4))
       (u/throw-ex (str "invalid upsert spec - " spec))))
   (upsert ($record spec) ($attrs spec) (alias-tag spec) (rel-tag spec)))
  ([recname rec-attrs rec-alias]
   (upsert recname rec-attrs rec-alias nil))
  ([recname rec-attrs rec-alias rel]
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
     (when rel
       {rel-tag (introspect-relationship rel)})
     (when rec-alias
       {alias-tag rec-alias})))))

(def upsert? (partial has-type? :upsert))

(declare raw-relationship)

(defn- raw-upsert [ir]
  (merge
   {($record ir)
    (raw-walk ($attrs ir))}
   (when-let [rel (rel-tag ir)]
     {rel-tag (raw-relationship rel)})
   (when-let [als (alias-tag ir)]
     {alias-tag als})))

(defn- query-attrs? [attrs]
  (or (li/proper-path? attrs) (some li/query-pattern? (keys attrs))))

(defn- query-record-name [recname]
  (if (li/query-pattern? recname)
    recname
    (li/name-as-query-pattern recname)))

(defn query-upsert
  ([spec]
   (let [cnt (count spec)]
     (when (or (< cnt 2) (> cnt 4))
       (u/throw-ex (str "invalid query-upsert spec - " spec))))
   (let [attrs (attributes spec)]
     (query-upsert ($record spec) attrs (alias-tag spec) (rel-tag spec))))
  ([recname attrs rec-alias]
   (query-upsert recname attrs rec-alias nil))
  ([recname attrs rec-alias rel]
   (when-not (or (li/query-pattern? recname)
                 (query-attrs? attrs))
     (u/throw-ex
      (str "not a valid query-upsert pattern - " {recname attrs})))
   (validate-alias! rec-alias)
   (as-syntax-object
    :query-upsert
    (merge
     {record-tag recname}
     (if (map? attrs)
       {attrs-tag (introspect-attrs attrs)}
       {path-tag attrs})
     (when rel
       {rel-tag (introspect-relationship rel)})
     (when rec-alias
       {alias-tag rec-alias})))))

(def query-upsert? (partial has-type? :query-upsert))

(defn- raw-query-upsert [ir]
  (let [path (path-tag ir)
        obj (when-not path (attributes ir))]
    (when-not (or obj path)
      (u/throw-ex (str "expected query-upsert attributes or path not found - " ir)))
    (merge
     {($record ir)
      (or path (raw-walk obj))}
     (when-let [rel (rel-tag ir)]
       {rel-tag (raw-relationship rel)})
     (when-let [als (alias-tag ir)]
       {alias-tag als}))))

(def raw-relationship raw-walk)

(defn- maybe-assoc-relationship [obj pattern]
  (if-let [r (rel-tag pattern)]
    (assoc obj rel-tag (introspect-relationship r))
    obj))

(defn- introspect-query-upsert [pattern]
  (let [pat (li/normalize-upsert-pattern pattern)
        recname (first (keys pat))
        attrs (recname pat)]
    (when-not (li/name? recname)
      (u/throw-ex (str "invalid record name - " recname)))
    (when-not (or (map? attrs) (li/proper-path? attrs))
      (u/throw-ex (str "expected a map or a path query - " attrs)))
    (let [attr-names (and (map? attrs ) (seq (keys attrs)))
          qpat (if attr-names
                 (some li/query-pattern? attr-names)
                 (li/query-pattern? recname))]
      (maybe-assoc-relationship
       ((if qpat query-upsert upsert)
        recname attrs
        (alias-tag pattern))
       pattern))))

(defn query-object
  ([spec]
   (when (> (count spec) 3)
     (u/throw-ex (str "invalid query spec - " spec)))
   (query-object ($record spec) (query-pattern spec) (alias-tag spec)))
  ([recname query-pat rec-alias]
   (when (and query-pat (not (:where query-pat)))
     (u/throw-ex
      (str "not a valid query pattern - " {recname query-pat})))
   (validate-alias! rec-alias)
   (as-syntax-object
    :query-object
    (merge
     {record-tag recname}
     (when query-pat
       {query-tag query-pat})
     (when rec-alias
       {alias-tag rec-alias})))))

(def query-object? (partial has-type? :query-object))

(defn- raw-query-object [ir]
  (if-let [obj (query-pattern ir)]
    (merge
     {($record ir)
      (raw-walk obj)}
     (when-let [als (alias-tag ir)]
       {alias-tag als}))
    (let [n ($record ir)]
      (if (li/query-pattern? n)
        n
        (li/name-as-query-pattern n)))))

(defn- introspect-query-object [pattern]
  (let [pat (li/normalize-upsert-pattern pattern)
        recname (first (keys pat))
        qpat (recname pat)]
    (when-not (li/name? recname)
      (u/throw-ex (str "invalid record name - " recname)))
    (when-not (:where qpat)
      (u/throw-ex (str "invalid query pattern - " qpat)))
    (query-object
     recname qpat
     (alias-tag pattern))))

(def relationship-object rel-tag)

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

(def ^:private not-as #(not= alias-tag %))

(defn- upto-alias [exps]
  (take-while not-as exps))

(defn- special-form-alias [obj]
  (second (drop-while not-as obj)))

(defn- raw-special-form [ir cmd-vec]
  (vec
   (concat
    cmd-vec
    (when-let [a (alias-tag ir)]
      [alias-tag a]))))

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

(defn- macro-call? [x]
  (and (vector? x)
       (li/registered-macro? (first x))))

(defn- dataflow-pattern? [x]
  (or (name-or-map? x)
      (macro-call? x)))

(defn for-each
  ([spec]
   (for-each ($value spec) ($body spec) (alias-tag spec)))
  ([valpat body val-alias]
   (validate-alias! val-alias)
   (when-not (name-or-map? valpat)
     (u/throw-ex (str "invalid value pattern in for-each - " valpat)))
   (when-not (every? dataflow-pattern? body)
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
   (when-not (dataflow-pattern? body)
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

(defn- relspec-for-delete? [obj]
  (and (vector? obj) (= rel-tag (first obj))))

(defn delete
  ([spec]
   (delete
    ($record spec) (attrs-tag spec)
    (alias-tag spec)
    (when-let [rel (rel-tag spec)]
      `[~rel-tag ~@rel])))
  ([recname attrs result-alias]
   (delete recname attrs result-alias nil))
  ([recname attrs result-alias rel]
   (let [amap (map? attrs)
         rel (or rel (if-not amap attrs rel))]
     (when-not (li/name? recname)
       (u/throw-ex (str "invalid record-name in delete - " recname)))
     (if amap
       (when-not (every? valid-attr-spec? attrs)
         (u/throw-ex (str "invalid attribute spec in delete - " attrs)))
       (when-not (relspec-for-delete? rel)
         (u/throw-ex (str "invalid relationship delete spec - " rel))))
     (validate-alias! result-alias)
     (as-syntax-object
      :delete
      (merge
       {record-tag recname}
       (if amap
         {attrs-tag (introspect-attrs attrs)}
         {rel-tag (introspect-relationship (rest rel))})
       {alias-tag result-alias})))))

(def delete? (partial has-type? :delete))

(defn- introspect-delete [obj]
  (delete (second obj)
          (nth obj 2)
          (special-form-alias obj)))

(defn- raw-delete [ir]
  (raw-special-form
   ir
   [:delete ($record ir)
    (if-let [attrs (attrs-tag ir)]
      (raw-walk attrs)
      `[~rel-tag ~@(raw-relationship (rel-tag ir))])]))

(defn query
  ([spec]
   (if (attrs-tag spec)
     (assoc (query-upsert spec) type-tag query-tag)
     (query ($query spec) (alias-tag spec))))
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
  (if (query-tag ir)
    (raw-special-form
     ir
     [:query
      (raw-walk ($query ir))])
    (raw-query-upsert ir)))

(defn _eval
  ([spec]
   (_eval ($exp spec) (check-tag spec) (alias-tag spec)))
  ([exp check result-alias]
   (when (and check (not (li/name? check)))
     (u/throw-ex (str "invalid value for check - " check)))
   (validate-alias! result-alias)
   (as-syntax-object
    :eval
    (merge
     {exp-tag (introspect exp)}
     (when check {check-tag check})
     (when result-alias {alias-tag result-alias})))))

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

(defn- pure-query-map? [obj]
  (let [pat (li/normalize-upsert-pattern obj)
        attrs ((first (keys pat)) pat)]
    (and (map? attrs) (:where attrs))))

(defn- introspect-name [pattern]
  (if (li/query-pattern? pattern)
    (as-syntax-object :query-object {record-tag (li/normalize-name pattern)})
    (as-syntax-object :reference {name-tag pattern})))

(defn reference [n]
  (if (keyword? n)
    (as-syntax-object :reference {name-tag n})
    (u/throw-ex (str "cannot make reference, not a name - " n))))

(def ^:private raw-reference name-tag)

(def reference? (partial has-type? :reference))

(defn- maybe-query [obj]
  (let [ks (seq (keys (attrs-tag obj)))]
    (cond
      (and ks (every? li/query-pattern? ks))
      (assoc obj type-tag query-tag)

      (and (not ks) (li/query-pattern? (record-tag obj)))
      (assoc obj type-tag query-tag)

      :else
      obj)))

(defn introspect [pattern]
  (cond
    (syntax-object? pattern) pattern

    (seqable? pattern)
    (cond
      (not (seq pattern))
      pattern

      (or (list? pattern) (= 'quote (first pattern)))
      (introspect-exp pattern)

      (map? pattern)
      (if (pure-query-map? pattern)
        (introspect-query-object pattern)
        (maybe-query (introspect-query-upsert pattern)))

      (vector? pattern)
      (introspect-special-form pattern)

      :else pattern)

    (li/name? pattern)
    (introspect-name pattern)

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
   :reference raw-reference
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

(defn fully-qualified?
  ([n]
   (if (second (li/split-path n))
     true
     false))
  ([model-name n]
   (cond
     (= model-name n) false
     (s/starts-with? (str n) (str model-name)) true
     :else false)))

(defn unqualified-name [x]
  (cond
    (li/name? x)
    (let [[c n] (li/split-path x)]
      (or n c))

    (li/parsed-path? x)
    (second x)))

(defn- ref-path-name-info [model-name n]
  (let [root-parts (li/split-ref n)
        parts-count (count root-parts)
        recname (when (= parts-count 1) (first root-parts))
        model-name-parts (when model-name (li/split-ref model-name))
        model-parts (when (> parts-count 1)
                      (if model-name-parts
                        (take (count model-name-parts) root-parts)
                        [(first root-parts)]))
        comp-parts (when (> parts-count 1)
                     (if model-parts
                       (drop (count model-parts) root-parts)
                       root-parts))
        cname (when comp-parts (li/make-ref comp-parts))
        mname (when model-parts (li/make-ref model-parts))]
    (if (and model-name (not= model-name mname))
      nil
      {:model mname :component cname :record recname})))

(defn- full-path-name-info [model-name n]
  (let [{c :component r :record} (li/path-parts n)]
    (if (li/ref-path-name? c)
      (when-let [info (ref-path-name-info model-name c)]
        (merge info {:record r}))
      {:component c :record r})))

(defn name-info
  ([model-name n]
   (cond
     (li/full-path-name? n) (full-path-name-info model-name n)
     (li/ref-path-name? n) (ref-path-name-info model-name n)
     (li/name? n) {:record n}
     :else nil))
  ([n] (name-info nil n)))
