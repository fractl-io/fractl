(ns fractl.compiler
  "Compile dataflow patterns to calls into the resolver protocol."
  (:require [clojure.walk :as w]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [fractl.util :as u]
            [fractl.util.graph :as ug]
            [fractl.util.seq :as us]
            [fractl.util.logger :as log]
            [fractl.lang.internal :as li]
            [fractl.lang.opcode :as op]
            [fractl.lang.syntax :as ls]
            [fractl.compiler.context :as ctx]
            [fractl.component :as cn]
            [fractl.env :as env]
            [fractl.store :as store]
            [fractl.store.util :as stu]
            [fractl.compiler.rule :as rule]
            [fractl.compiler.validation :as cv]
            [fractl.compiler.internal :as i]
            [fractl.compiler.expr.ui :as uic]))

(def make-context ctx/make)

(def ^:private emit-load-literal op/load-literal)
(def ^:private emit-load-instance-by-name op/load-instance)

(defn- emit-load-references [[rec-name alias :as n] refs]
  (when (cv/validate-references rec-name refs)
    (op/load-references [n refs])))

(defn- emit-match [match-pattern-code cases-code alternative-code alias]
  (op/match [match-pattern-code cases-code alternative-code alias]))

(defn- emit-for-each [bind-pattern-code elem-alias body-code alias]
  (op/for-each [bind-pattern-code elem-alias body-code alias]))

(defn- emit-delete [recname id-pat-code]
  (op/delete-instance [recname id-pat-code]))

(defn- emit-try [body handlers alias-name]
  (op/try_ [body handlers alias-name]))

(def ^:private runtime-env-var '--env--)
(def ^:private current-instance-var '--inst--)

(declare expr-with-arg-lookups)

(defn- reference-lookup-call [n]
  (let [parts (li/path-parts n)]
    (cond
      (:path parts)
      `(if-let [r# (get ~current-instance-var ~n)]
         r#
         (let [result# (fractl.env/lookup-by-alias ~runtime-env-var ~(:path parts))
               r# (if (map? result#) result#
                      (if (i/const-value? result#)
                        result#
                        (first result#)))]
           (if-let [refs# '~(seq (:refs parts))]
             (get-in r# refs#)
             result#)))

      (seq (:refs parts))
      `(first (fractl.env/follow-reference ~runtime-env-var ~parts))

      :else
      `(fractl.env/lookup-instance ~runtime-env-var [(:component ~parts) (:record ~parts)]))))

(defn- arg-lookup [arg]
  (cond
    (i/const-value? arg) arg

    (vector? arg)
    (vec (expr-with-arg-lookups arg))

    (seqable? arg)
    (expr-with-arg-lookups arg)

    (li/name? arg)
    (reference-lookup-call arg)

    (symbol? arg)
    `(fractl.env/lookup-variable ~runtime-env-var ~arg)

    :else arg))

(defn- map-values-as-exprs [m]
  (let [r (mapv (fn [[k v]]
                  [k (arg-lookup v)])
                m)]
    (into {} r)))

(defn- make-map-expr [expr]
  (let [inst-pat? (li/instance-pattern? expr)
        m (if inst-pat? (li/instance-pattern-attrs expr) expr)
        m-with-exprs (map-values-as-exprs m)]
    (if inst-pat?
      `(fractl.component/make-instance
        ~(li/instance-pattern-name expr)
        ~m-with-exprs)
      m-with-exprs)))

(defn- expr-with-arg-lookups [expr]
  (cond
    (i/const-value? expr) expr
    (map? expr) (make-map-expr expr)
    (seqable? expr)
    (let [final-args (map arg-lookup (rest expr))]
      `(~(first expr) ~@final-args))
    :else (arg-lookup expr)))

(defn- expr-as-fn [expr]
   (li/evaluate
    `(fn ~[runtime-env-var current-instance-var]
       ~expr)))

(defn- query-param-lookup [p]
  (let [r (arg-lookup p)]
    (if (i/const-value? r)
      r
      (expr-as-fn r))))

(declare query-param-process)

(defn- param-process-seq-query [attrname query]
  (if (vector? (first query))
    (mapv #(query-param-process [attrname %]) query)
    (concat
     [(first query)]
     (concat [attrname] (mapv query-param-lookup (rest query))))))

(defn- logical-query? [q]
  (and (seqable? q)
       (let [f (first q)]
         (or (= :and f) (= :or f)))))

(defn- query-param-process [[k v]]
  (if (logical-query? v)
    (let [opr (first v)]
      `[~opr ~@(map #(query-param-process [k %]) (rest v))])
    (cond
      (i/const-value? v) [k v]
      (seqable? v) (vec (param-process-seq-query k v))
      :else [k (query-param-lookup v)])))

(defn- fetch-compile-query-fn [ctx]
  (or (ctx/fetch-compile-query-fn ctx)
      (store/get-default-compile-query)))

(defn- recname-from-opcode [opc]
  (first (:arg opc)))

(defn- compiled-query-from-opcode [opc]
  (stu/compiled-query (second (:arg opc))))

(defn- as-opcode-map [opc]
  (let [r (if (map? opc)
            (:opcode opc)
            (:opcode (first opc)))]
    (if (map? r)
      r
      (first r))))

(defn- merge-queries [ctx main-entity-name main-query filters-opcode]
  (let [fopcode (as-opcode-map (first filters-opcode))
        sopcode (as-opcode-map (second filters-opcode))
        rel-name (recname-from-opcode fopcode)
        node-entity-name (recname-from-opcode sopcode)]
    (when-not (cn/in-relationship? main-entity-name rel-name)
      (u/throw-ex (str main-entity-name " not in relationship - " rel-name)))
    (when-not (cn/in-relationship? node-entity-name rel-name)
      (u/throw-ex (str node-entity-name " not in relationship - " rel-name)))
    (let [rel-scm (cn/fetch-relationship-schema rel-name)
          ent-scm (cn/fetch-entity-schema node-entity-name)
          attrs (concat
                 [(cn/identity-attribute-name main-entity-name)]
                 (cn/relationship-attribute-names
                  main-entity-name node-entity-name))
          rel-q (compiled-query-from-opcode fopcode)
          node-q (compiled-query-from-opcode sopcode)]
      ((fetch-compile-query-fn ctx)
       {:filter-in-sequence [[main-query rel-q node-q] attrs]}))))

(declare compile-pattern)

(defn- compile-relational-entity-query [ctx entity-name query query-filter]
  (let [q (i/expand-query
           entity-name
           (mapv query-param-process query))
        cq ((fetch-compile-query-fn ctx) q)
        final-cq (if query-filter
                   (merge-queries
                    ctx entity-name cq
                    (mapv (partial compile-pattern ctx) query-filter))
                   cq)]
    (stu/package-query q final-cq)))

(defn compile-query [ctx entity-name query query-filter]
  (let [q (compile-relational-entity-query
           ctx entity-name query query-filter)]
    (ctx/put-fresh-record! ctx entity-name {})
    q))

(defn- compound-expr-as-fn
  "Compile compound expression to a function.
   Arguments are tranlated to lookup calls on
   the runtime context."
  [expr]
  (expr-as-fn (expr-with-arg-lookups expr)))

(defn- build-dependency-graph [attr-pats ctx schema graph]
  ;; make parser/build-dependency-graph callable from util/apply->
  (let [result-graph (i/build-dependency-graph attr-pats ctx schema graph)]
    [ctx schema result-graph]))

(def ^:private appl (partial u/apply-> last))

(defn- parse-attributes
  "Classify attributes in the pattern as follows:
    1. computable - values can be computed at compile-time, e.g literals.
    2. references - values are references to other attributes, that can be reached from the dataflow.
    3. compound - values has to be computed at runtime, by invoking a function.
    4. queries - a query has to be issued on this attribute.

    Any value or reference is validated against the schema, raise an exception on failure.

    A graph of dependencies is prepared for each attribute. If there is a cycle in the graph,
    raise an error. Otherwise return a map with each attribute group and their attached graphs."
  [ctx pat-name pat-attrs schema args]
  (if (:full-query? args)
    {:attrs (assoc pat-attrs :query (compile-query ctx pat-name pat-attrs (:query-filter args)))}
    (let [{computed :computed refs :refs
           compound :compound query :query
           :as cls-attrs} (i/classify-attributes ctx pat-attrs schema)
          fs (map #(partial build-dependency-graph %) [refs compound query])
          deps-graph (appl fs [ctx schema ug/EMPTY])
          compound-exprs (map (fn [[k v]] [k (compound-expr-as-fn v)]) compound)
          parsed-refs (map (fn [[k v]] [k (if (symbol? v) {:refs v} (li/path-parts v))]) refs)
          compiled-query (when query (compile-query ctx pat-name query (:query-filter args)))
          final-attrs (if (seq compiled-query)
                        (assoc cls-attrs :query compiled-query)
                        cls-attrs)]
      {:attrs (assoc final-attrs :compound compound-exprs :refs parsed-refs)
       :deps deps-graph})))

(def ^:private set-attr-opcode-fns {:computed op/set-literal-attribute
                                    :refs op/set-ref-attribute
                                    :compound op/set-compound-attribute})

(defn- begin-build-instance [rec-name attrs]
  (if-let [q (:query attrs)]
    (op/query-instances [rec-name q])
    (op/new-instance rec-name)))

(declare compile-list-literal)

(defn- set-literal-attribute [ctx [aname valpat :as attr]]
  (if (vector? valpat)
    (compile-list-literal ctx aname valpat)
    (op/set-literal-attribute attr)))

(defn- build-record-for-upsert? [attrs]
  (when (or (seq (:compound attrs))
            (seq (:computed attrs))
            (seq (:sorted attrs)))
    true))

(defn- emit-build-record-instance [ctx rec-name attrs schema args]
  (let [alias (:alias args)
        event? (= (cn/type-tag-key args) :event)
        timeout-ms (:timeout-ms args)]
    (concat [(begin-build-instance rec-name attrs)]
            (mapv (partial set-literal-attribute ctx)
                  (:computed attrs))
            (let [f (:compound set-attr-opcode-fns)]
              (mapv #(f %) (:compound attrs)))
            (mapv (fn [[k v]]
                    ((k set-attr-opcode-fns) v))
                  (:sorted attrs))
            (mapv (fn [arg]
                    (op/set-ref-attribute arg))
                  (:refs attrs))
            [(if event?
               (op/intern-event-instance
                [rec-name alias (ctx/fetch-with-types ctx)
                 timeout-ms])
               (op/intern-instance
                (vec
                 (concat
                  [rec-name alias]
                  (if (ctx/build-partial-instance? ctx)
                    [false false]
                    [true (and (cn/entity? rec-name)
                               (build-record-for-upsert? attrs))])))))])))

(defn- sort-attributes-by-dependency [attrs deps-graph]
  (let [sorted (i/sort-attributes-by-dependency attrs deps-graph)
        compound (i/left-out-from-sorted :compound attrs sorted)]
    (assoc attrs :sorted sorted :compound compound)))

(defn- emit-realize-instance
  "Emit opcode for realizing a fully-built instance of a record, entity or event.
  It is assumed that the opcodes for setting the individual attributes were emitted
  prior to this."
  [ctx pat-name pat-attrs schema args]
  (when-let [xs (cv/invalid-attributes pat-attrs schema)]
    (if (= (first xs) cn/id-attr)
      (if (= (get schema cn/type-tag-key) :record)
        (u/throw-ex (str "Invalid attribute " cn/id-attr " for type record: " pat-name))
        (u/throw-ex (str "Wrong reference of id in line: " pat-attrs "of " pat-name)))
      (u/throw-ex (str "Invalid attributes in pattern - " xs))))
  (let [{attrs :attrs deps-graph :deps} (parse-attributes ctx pat-name pat-attrs schema args)
        sorted-attrs (sort-attributes-by-dependency attrs deps-graph)]
    (emit-build-record-instance ctx pat-name sorted-attrs schema args)))

(defn- emit-dynamic-upsert [ctx pat-name pat-attrs _ args]
  (op/dynamic-upsert
   [pat-name pat-attrs (partial compile-pattern ctx) (:alias args)]))

(defn- emit-realize-map-literal [ctx pat]
  ;; TODO: implement support for map literals.
  (u/throw-ex (str "cannot compile map literal " pat)))

(defn- compile-fetch-all-query
  "Generate code for the wildcard query pattern (:EntityName?) to retrieve
  all instances of an entity. For an SQL backend, this will make the store do
  a `SELECT * FROM entity_table`."
  [ctx pat]
  (let [entity-name (li/split-path (li/query-target-name pat))
        q (compile-query ctx entity-name nil nil)]
    (op/query-instances [entity-name q])))

(defn- compile-pathname
  ([ctx pat alias]
   (if (li/query-pattern? pat)
     (compile-fetch-all-query ctx pat)
     (let [{component :component record :record refs :refs
            path :path :as parts} (if (map? pat) pat (li/path-parts pat))]
       (if path
         (if-let [p (ctx/dynamic-type ctx (ctx/aliased-name ctx path))]
           (if (= path pat)
             (emit-load-instance-by-name [path path])
             (compile-pathname ctx (assoc (li/path-parts p) :refs refs) path))
           (if (= path pat)
             (u/throw-ex (str "ambiguous reference - " pat))
             (compile-pathname ctx parts)))
         (let [n (ctx/dynamic-type ctx [component record])
               opc (and (cv/find-schema n)
                        (if refs
                          (emit-load-references [n alias] refs)
                          (emit-load-instance-by-name [n alias])))]
           (ctx/put-record! ctx n {})
           opc)))))
  ([ctx pat] (compile-pathname ctx pat nil)))

(defn- process-complex-query [v]
  (if (li/name? v)
    (let [parts (li/path-parts v)]
      (if (seq (:refs parts))
        (expr-as-fn (arg-lookup v))
        v))
    v))

(defn- complex-query-pattern? [pat]
  (when-not (ls/rel-tag pat)
    (let [ks (keys (li/normalize-instance-pattern pat))]
      (and (= 1 (count ks))
           (s/ends-with? (str (first ks)) "?")))))

(defn- query-entity-name [k]
  (let [sk (str k)]
    (when-not (s/ends-with? sk "?")
      (u/throw-ex (str "queried entity-name must end with a `?` - " k)))
    (keyword (subs (apply str (butlast sk)) 1))))

(defn- compile-complex-query
  "Compile a complex query. Invoke the callback
  function with the compiled query as argument.
  The default behavior is to pass the compiled query
  to the query-instances opcode generator"
  ([ctx pat callback]
   (let [k (first (keys pat))
         n (ctx/dynamic-type ctx (query-entity-name k))]
     (when-not (cn/find-entity-schema n)
       (u/throw-ex (str "cannot query undefined entity - " n)))
     (let [q (k pat)
           w (when (seq (:where q)) (w/postwalk process-complex-query (:where q)))
           c (stu/package-query q ((fetch-compile-query-fn ctx) (assoc q :from n :where w)))]
       (callback [(li/split-path n) c]))))
  ([ctx pat]
   (compile-complex-query ctx pat op/query-instances)))

(defn- query-map->command [pat]
  (if-let [alias (:as pat)]
    [(dissoc pat :as) :as alias]
    [pat]))

(defn- fetch-with-types [pat]
  (when-let [wt (ctx/with-types-tag pat)]
    (when-not (map? wt)
      (u/throw-ex (str "with-types expects a map " - wt)))
    (doseq [[base-type subtype] wt]
      (when-not (cn/inherits? base-type subtype)
        (u/throw-ex
         (str "error in with-types - "
              subtype " is not a subtype of "
              base-type " in " wt))))
    wt))

(defn- from-pattern-typename [pat]
  (first (keys (li/normalize-upsert-pattern pat))))

(defn- from-pattern? [pat]
  (and (:from pat)
       (map? ((from-pattern-typename pat) pat))))

(defn- compile-from-pattern [ctx pat]
  (let [typ (from-pattern-typename pat)]
    (when-not (cn/find-object-schema typ)
      (u/throw-ex (str "undefined type " typ " in " pat)))
    (let [f (:from pat)
          inst-alias (:as pat)
          opcode (if (or (li/pathname? f) (map? f))
                   (compile-pattern ctx f)
                   (u/throw-ex (str "invalid :from specification " f)))]
      (when inst-alias
        (ctx/add-alias! ctx inst-alias))
      (op/instance-from
       [(li/split-path (ctx/dynamic-type ctx typ))
        (let [np (li/normalize-upsert-pattern pat)]
          (when-let [p (seq (first (vals np)))]
            (ctx/build-partial-instance! ctx)
            (let [cp (compile-pattern ctx np)]
              (ctx/clear-build-partial-instance! ctx)
              cp)))
        opcode inst-alias]))))

(defn- package-opcode [code]
  (if (and (map? code) (:opcode code))
    code
    {:opcode code}))

(defn- ensure-relationship-name [n]
  (when-not (cn/fetch-relationship-schema (li/normalize-name n))
    (u/throw-ex (str "relationship " n " not found")))
  n)

(defn normalize-recname-in-relationship [n]
  (if (li/parsed-path? n)
    n
    (li/normalize-name (or (and (map? n) (:path n)) n))))

(defn- compile-intern-relationship [ctx recname pat]
  (let [rel (first pat)
        is-obj (map? rel)
        n (if is-obj
            (first (keys rel))
            rel)
        recname (normalize-recname-in-relationship recname)]
    (ensure-relationship-name n)
    (when-not (some #{n} (cn/find-relationships recname))
      (u/throw-ex (str "relationship " n " not found for " recname)))
    [[rel (li/split-path n) is-obj] (compile-pattern ctx (second pat))]))

(defn- compile-relationship-pattern [ctx recname intern-rec-opc pat]
  (let [c (partial compile-intern-relationship ctx recname)
        rel-opcs (if (vector? (first pat))
                   (mapv c pat)
                   [(c pat)])]
    (op/intern-relationship-instance
     [(package-opcode intern-rec-opc) rel-opcs])))

(defn- ensure-relationship-full-query [x]
  (when-not (and (li/query-pattern? x)
                 (ensure-relationship-name
                  (li/query-target-name x)))
    (u/throw-ex (str "not a relationship query - " x)))
  x)

(defn- ensure-some-query-attrs [attrs]
  (when-not (some li/query-pattern? (keys attrs))
    (u/throw-ex (str "no query pattern in relationship - " attrs)))
  attrs)

(defn- ensure-relationship-query [pat]
  (let [relq (first pat)]
    (cond
      (keyword? relq) (ensure-relationship-full-query relq)
      (map? relq) (and (ensure-relationship-name (first (keys relq)))
                       (ensure-some-query-attrs (first (vals relq))))
      :else (u/throw-ex (str "invalid relationship query pattern - " relq))))
  pat)

(declare compile-query-command)

(defn- compile-map [ctx pat]
  (cond
    (complex-query-pattern? pat)
    (compile-query-command ctx (query-map->command pat))

    (from-pattern? pat)
    (compile-from-pattern ctx pat)

    (li/instance-pattern? pat)
    (let [orig-nm (ctx/dynamic-type
                   ctx
                   (li/instance-pattern-name pat))
          full-nm (li/normalize-name orig-nm)
          {component :component record :record
           path :path refs :refs :as parts} (li/path-parts full-nm)
          refs (seq refs)
          nm (if (or path refs)
               parts
               [component record])
          attrs (li/instance-pattern-attrs pat)
          alias (:as pat)
          timeout-ms (ls/timeout-ms-tag pat)
          [tag scm] (if (or path refs)
                      [:dynamic-upsert nil]
                      (cv/find-schema nm full-nm))
          relpat (ls/rel-tag pat)
          is-query-upsert (or (li/query-pattern? orig-nm)
                              (some li/query-pattern? (keys attrs)))
          is-relq (and relpat is-query-upsert)]
      (when is-relq
        (ensure-relationship-query relpat))
      (let [c (case tag
                (:entity :record) emit-realize-instance
                :event (do
                         (when-let [wt (fetch-with-types pat)]
                           (ctx/bind-with-types! ctx wt))
                         emit-realize-instance)
                :dynamic-upsert emit-dynamic-upsert
                (u/throw-ex (str "not a valid instance pattern - " pat)))
            args0 (merge {:alias alias cn/type-tag-key tag
                          :full-query? (and (= tag :entity)
                                            (li/query-pattern? orig-nm))}
                         (when timeout-ms
                           {:timeout-ms timeout-ms}))
            args (if is-relq
                   (assoc
                    args0 :query-filter relpat)
                   args0)
            opc (c ctx nm attrs scm args)]
        (ctx/put-record! ctx nm pat)
        (when alias
          (let [alias-name (ctx/alias-name alias)]
            (ctx/add-alias! ctx (or nm alias-name) alias)))
        (if (and relpat (not is-relq))
          (compile-relationship-pattern ctx nm opc relpat)
          opc)))

    :else
    (emit-realize-map-literal ctx pat)))

(defn- compile-user-macro [ctx pat]
  (let [m (first pat)]
    (if (li/macro-name? m)
      (u/throw-ex (str "macro not found - " m))
      (u/throw-ex (str "not a valid macro name - " m)))))

(defn- special-form-alias [pat]
  (let [rpat (reverse pat)]
    (if (= :as (second rpat))
      [(vec (reverse (nthrest rpat 2))) (first rpat)]
      [pat nil])))

(defn- compile-for-each-body [ctx body-pats]
  (ctx/add-alias! ctx :% :%)
  (let [code (loop [body-pats body-pats, body-code []]
               (if-let [body-pat (first body-pats)]
                 (recur (rest body-pats)
                        (conj body-code [(compile-pattern ctx body-pat)]))
                 body-code))]
    code))

(defn- parse-for-each-match-pattern [pat]
  (if (vector? pat)
    (if (= :as (second pat))
      [(first pat) (nth pat 2)]
      [pat nil])
    [pat nil]))

(defn- compile-for-each-match-pattern [ctx pat]
  (let [[pat alias] (parse-for-each-match-pattern pat)]
    (when alias
      (ctx/add-alias! ctx alias))
    [(compile-pattern ctx pat) alias]))

(defn- compile-for-each [ctx pat]
  (let [[bind-pat-code elem-alias]
        (compile-for-each-match-pattern ctx (first pat))
        [body-pats alias] (special-form-alias (rest pat))
        body-code (compile-for-each-body ctx body-pats)]
    (when alias
      (ctx/add-alias! ctx alias))
    (emit-for-each bind-pat-code elem-alias body-code alias)))

(defn- extract-match-clauses [pat]
  (let [[pat alias] (special-form-alias pat)]
    (loop [pat pat, result []]
      (if (seq pat)
        (let [case-pat (first pat), conseq (first (rest pat))]
          (if conseq
            (recur (nthrest pat 2) (conj result [case-pat conseq]))
            [result case-pat alias]))
        [result nil alias]))))

(defn- compile-maybe-pattern-list [ctx pat]
  (if (vector? pat)
    (mapv #(compile-pattern ctx %) pat)
    (compile-pattern ctx pat)))

(defn- compile-match-cases [ctx cases]
  (loop [cases cases, cases-code []]
    (if-let [[case-pat conseq] (first cases)]
      (recur
       (rest cases)
       (conj
        cases-code
        [[(compile-pattern ctx case-pat)]
         [(compile-maybe-pattern-list ctx conseq)]]))
      cases-code)))

(defn- case-match?
  "If the first component of the match is a name or literal, it's a
  normal match expression (similar to Clojure `case`),
  otherwise it's a conditional expression.
  Return true for a normal match expression."
  [pat]
  (let [f (first pat)]
    (or (li/name? f)
        (li/literal? f))))

(defn- generate-cond-code [ctx pat]
  (loop [clauses pat, code []]
    (if-let [c (first clauses)]
      (if-not (seq (rest clauses))
        {:clauses code :else (compile-pattern ctx c)}
        (recur (nthrest clauses 2)
               (conj
                code
                [(rule/compile-rule-pattern c)
                 (compile-pattern ctx (second clauses))])))
      {:clauses code})))

(defn- compile-match-cond [ctx pat]
  (let [[pat alias] (special-form-alias pat)
        code (generate-cond-code ctx pat)]
    (when alias
      (ctx/add-alias! ctx alias))
    (emit-match nil (:clauses code) (:else code) alias)))

(defn- compile-match [ctx pat]
  (if (case-match? pat)
    (let [match-pat-code (compile-pattern ctx (first pat))
          [cases alternative alias] (extract-match-clauses (rest pat))
          cases-code (compile-match-cases ctx cases)
          alt-code (when alternative (compile-maybe-pattern-list ctx alternative))]
      (when alias
        (ctx/add-alias! ctx alias))
      (emit-match [match-pat-code] cases-code [alt-code] alias))
    (compile-match-cond ctx pat)))

(defn- compile-try-handler [ctx [k pat]]
  (when-not (op/result-tag? k)
    (u/throw-ex (str "invalid try handler " k)))
  [k (compile-pattern ctx pat)])

(defn- distribute-handler-keys [handler-spec]
  (loop [hs handler-spec, final-spec {}]
    (if-let [[k v] (first hs)]
      (recur (rest hs)
             (if (vector? k)
               (reduce #(assoc %1 %2 v) final-spec k)
               (assoc final-spec k v)))
      final-spec)))

(defn- compile-construct-with-handlers [ctx pat]
  (let [body (compile-pattern ctx (first pat))
        handler-pats (distribute-handler-keys
                      (into {} (map vec (partition 2 (rest pat)))))
        handlers (mapv (partial compile-try-handler ctx) handler-pats)]
    (when-not (seq handlers)
      (u/throw-ex "proper handlers are required for :try"))
    [body (into {} handlers)]))

(defn- try-alias [pat]
  (let [rpat (reverse pat)]
    (if (= :as (second rpat))
      [(vec (reverse (nthrest rpat 2))) (first rpat)]
      [pat nil])))

(defn- compile-try [ctx pat]
  (let [[pat alias-name] (try-alias pat)
        [body handlers] (compile-construct-with-handlers ctx pat)]
    (when alias-name
      (ctx/add-alias! ctx alias-name))
    (emit-try body handlers alias-name)))

(defn- valid-alias-name? [alias]
  (if (vector? alias)
    (every? #(if (vector? %)
               (every? li/name? %)
               (li/name? %))
            alias)
    (li/name? alias)))

(defn- compile-query-pattern [ctx query-pat alias]
  (let [[nm refs] (when (li/name? query-pat)
                    (let [{component :component
                           record :record refs :refs}
                          (li/path-parts query-pat)]
                      [[component record] refs]))]
    (when-not (or (map? query-pat) (li/name? query-pat))
      (u/throw-ex (str "invalid query pattern - " query-pat)))
    (when alias
      (when-not (valid-alias-name? alias)
        (u/throw-ex (str "not a valid name - " alias)))
      (let [alias-name (ctx/alias-name alias)]
        (ctx/add-alias! ctx (or nm alias-name) alias)))
    (op/evaluate-query
     [#(compile-complex-query
        ctx
        (if (map? query-pat)
          query-pat
          (%2 nm refs))
        identity)
      alias])))

(defn- query-by-function [query-pat]
  (when (map? query-pat)
    (let [k (query-entity-name (first (keys query-pat)))
          f (first (vals query-pat))]
      (when (and (li/name? k) (fn? f))
        [k f]))))

(defn- compile-query-command
  "Compile the command [:query pattern :as result-alias].
   `pattern` could be a query pattern or a reference, making
  it possible to dynamically execute queries received via events.
  If `result-alias` is provided, the query result is bound to that name
  in the local environment"
  [ctx pat]
  (let [query-pat (first pat)
        alias (when (= :as (second pat))
                (nth pat 2))]
    (if-let [[entity-name qfn] (query-by-function query-pat)]
      (op/evaluate-query [(fn [env _]
                            (let [q (stu/package-query (qfn (partial env/lookup env)))]
                              [(li/split-path entity-name)
                               (if (string? q)
                                 [q]
                                 q)]))
                          alias])
      (compile-query-pattern ctx query-pat alias))))

(defn- compile-delete [ctx [recname & id-pat]]
  (if (= (vec id-pat) [:*])
    (emit-delete (li/split-path recname) :*)
    (let [p (first id-pat)
          qpat (if (map? p)
                 p
                 [[(cn/identity-attribute-name recname) p]])
          alias (when (> (count id-pat) 1)
                  (if (= :as (second id-pat))
                    (nth id-pat 2)
                    (u/throw-ex (str "expected alias declaration, found " (second id-pat)))))
          q (compile-query
             ctx recname
             (if (map? qpat)
               (into [] qpat)
               qpat) nil)]
      (when alias
        (ctx/add-alias! ctx recname alias))
      (emit-delete (li/split-path recname) (merge q {ls/alias-tag alias})))))

(defn- compile-quoted-expression [ctx exp]
  (if (li/unquoted? exp)
    (if (> (count exp) 2)
      (u/throw-ex (str "cannot compile rest of unquoted expression - " exp))
      (compile-pattern ctx (second exp)))
    exp))

(defn- compile-quoted-list [ctx pat]
  (w/prewalk (partial compile-quoted-expression ctx) pat))

(defn- compile-await [ctx pat]
  (op/await_ (compile-construct-with-handlers ctx pat)))

(defn- compile-entity-definition [_ pat]
  (op/entity-def (first pat)))

(defn- compile-eval [ctx pat]
  (let [m (us/split-to-map (rest pat))
        ret-type (:check m)
        result-alias (:as m)]
    (when (keyword? ret-type)
      (ctx/put-fresh-record! ctx (li/split-path ret-type) {}))
    (when result-alias
      (ctx/add-alias! ctx (or ret-type result-alias) result-alias))
    (op/eval_
     [(expr-as-fn (expr-with-arg-lookups (first pat)))
      ret-type result-alias])))

(def ^:private special-form-handlers
  {:match compile-match
   :try compile-try
   :for-each compile-for-each
   :query compile-query-command
   :delete compile-delete
   :await compile-await
   :eval compile-eval
   :entity compile-entity-definition})

(defn- compile-special-form
  "Compile built-in special-forms (or macros) for performing basic
  conditional and iterative operations."
  [ctx pat]
  (if-let [h ((first pat) special-form-handlers)]
    (h ctx (rest pat))
    (compile-user-macro ctx pat)))

(defn- compile-list-literal [ctx attr-name pat]
  (let [quoted? (li/quoted? pat)]
    (op/set-list-attribute
     [attr-name
      (if quoted?
        (compile-quoted-list ctx (second pat))
        (mapv #(compile-pattern ctx %) pat))
      quoted?])))

(defn- compile-vector [ctx pat]
  (if (li/registered-macro? (first pat))
    (compile-special-form ctx pat)
    (compile-list-literal ctx nil pat)))

(defn- compile-literal [_ pat]
  (emit-load-literal pat))

(defn- compile-fncall-expression [_ pat]
  (op/call-function (compound-expr-as-fn pat)))

(defn compile-pattern [ctx pat]
  (if-let [c (cond
               (li/pathname? pat) compile-pathname
               (map? pat) compile-map
               (vector? pat) compile-vector
               (i/const-value? pat) compile-literal
               (seqable? pat) compile-fncall-expression)]
    (let [code (c ctx pat)]
      (package-opcode code))
    (u/throw-ex (str "cannot compile invalid pattern - " pat))))

(defn- maybe-mark-conditional-df [ctx evt-pattern]
  (when (li/name? evt-pattern)
    (when (cn/conditional-event? evt-pattern)
      (ctx/bind-variable! ctx i/conditional-dataflow-tag true)))
  ctx)

(defn- error-pattern-as-string [pat]
  (with-out-str (pp/pprint pat)))

(def ^:private error-marker (keyword "-^--- ERROR in pattern"))

(defn- report-compiler-error [all-patterns pattern-index ex]
  (loop [pats all-patterns, n pattern-index,
         marker-set false, result []]
    (if-let [p (first pats)]
      (let [f (neg? n)]
        (recur (rest pats)
               (dec n)
               f (if f
                   (conj result error-marker p)
                   (conj result p))))
      (let [err-pat (if marker-set result (conj result error-marker))]
        (log/error (str "error in expression " ex))
        (log/error (error-pattern-as-string err-pat))
        (throw ex)))))

(defn- compile-with-error-report [all-patterns compile-fn
                                  pattern-to-compile n]
  (try
    (compile-fn pattern-to-compile)
    #?(:clj
       (catch Exception e
         (report-compiler-error all-patterns n e))
       :cljs
       (catch js/Error e
         (report-compiler-error all-patterns n e)))))

(defn- compile-dataflow [ctx evt-pattern df-patterns]
  (let [c (partial
           compile-pattern
           (maybe-mark-conditional-df ctx evt-pattern))
        ec (c evt-pattern)
        ename (if (li/name? evt-pattern)
                evt-pattern
                (first (keys evt-pattern)))
        safe-compile (partial compile-with-error-report df-patterns c)
        result [ec (mapv safe-compile df-patterns (range (count df-patterns)))]]
    result))

(defn maybe-compile-dataflow
  ([compile-query-fn with-types df]
   (when-not (cn/dataflow-opcode df with-types)
     (let [ctx (make-context with-types)]
       (ctx/bind-compile-query-fn! ctx compile-query-fn)
       (cn/set-dataflow-opcode!
        df (compile-dataflow
            ctx (cn/dataflow-event-pattern df)
            (cn/dataflow-patterns df))
        with-types)))
   df)
  ([compile-query-fn df]
   (maybe-compile-dataflow compile-query-fn cn/with-default-types df)))

(defn compile-dataflows-for-event [compile-query-fn event]
  (let [evt (dissoc event ctx/with-types-tag)
        wt (get event ctx/with-types-tag cn/with-default-types)]
    (mapv (partial maybe-compile-dataflow compile-query-fn wt)
          (cn/dataflows-for-event evt))))

(defn- reference-attributes [attrs refrec]
  (when-let [result (cn/all-reference-paths attrs)]
    (let [[attr-name path] (first result)
          {refs :refs} (li/path-parts (:ref path))]
      [attr-name (first refs)])))

(defn- assert-unique! [entity-name attr-name ref-attr]
  (let [scm (cn/entity-schema entity-name)]
    (when-not (cn/unique-attribute? scm attr-name)
      (u/throw-ex (str "Reference not valid - " ref-attr " - " [entity-name attr-name]
                       " is not unique")))))

(defn- arg-lookup-fn [rec-name attrs attr-names aname arg]
  (cond
    (li/literal? arg)
    arg

    (= aname arg)
    (u/throw-ex (str "self-reference in attribute expression - " [rec-name aname]))

    (some #{arg} attr-names)
    `(~arg ~current-instance-var)

    :else
    (if-let [[refattr attr] (li/split-ref arg)]
      (if-let [refpath (:ref (get attrs refattr))]
        (let [{component :component rec :record refs :refs} (li/path-parts refpath)
              ukattr (first refs)]
          (assert-unique! [component rec] ukattr refattr)
          ;; The referenced instance is auto-loaded into the environment by the
          ;; evaluator, before the following code executes.
          ;; See evaluator/root/do-load-references
          `(~attr
            (first
             (fractl.env/lookup-instances-by-attributes
              ~runtime-env-var
              ~[component rec] [[~ukattr (~refattr ~current-instance-var)]]))))
        (u/throw-ex (str "not a reference attribute " [rec-name aname arg])))
      (u/throw-ex (str "invalid reference attribute " [rec-name aname arg])))))

(defn compile-attribute-expression [rec-name attrs aname aval]
  (when-not aval
    (u/throw-ex (str "attribute expression cannot be nil - " [rec-name aname])))
  (let [fexprs (map (partial arg-lookup-fn rec-name attrs (keys attrs) aname) (rest aval))
        exp `(fn [~runtime-env-var ~current-instance-var]
               (~(first aval) ~@fexprs))]
    (li/evaluate exp)))

(def expression-compiler-registry (u/make-cell {:ui uic/compile-ui-spec}))

(defn register-expression-compiler [tag compile-fn]
  (u/safe-set
   expression-compiler-registry
   (assoc @expression-compiler-registry tag compile-fn)))

(defn expression-compiler [n]
  (let [[a b] (li/split-path n)
        tag (or b a)]
    (if-let [c (tag @expression-compiler-registry)]
      [c tag]
      (u/throw-ex (str tag " - unsupported expression tag")))))
