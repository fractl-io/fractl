(ns fractl.compiler
  "Compile dataflow patterns to calls into the resolver protocol."
  (:require [fractl.util :as u]
            [fractl.util.graph :as ug]
            [fractl.util.seq :as us]
            [fractl.lang.internal :as li]
            [fractl.lang.opcode :as op]
            [fractl.compiler.context :as ctx]
            [fractl.namespace :as n]
            [fractl.compiler.validation :as cv]
            [fractl.compiler.parser :as p]))

(def make-context ctx/make)

(defn- emit-load-instance-by-name [_ rec-name]
  (op/load-instance rec-name))

(defn- emit-match-event-by-name [_ event-name]
  (op/match-instance event-name))

(defn- emit-load-references [_ rec-name refs]
  (when (cv/validate-references rec-name refs)
    (op/load-references [rec-name refs])))

(defn- build-dependency-graph [attr-pats ctx schema graph]
  ;; makes parser/build-dependency-graph callable from util/apply->
  (let [result-graph (p/build-dependency-graph attr-pats ctx schema graph)]
    [ctx schema result-graph]))

(def ^:private runtime-env-var '--env--)
(def ^:private current-instance-var '--inst--)

(declare expr-with-arg-lookups)

(defn- reference-lookup-call [n]
  (let [parts (li/path-parts n)]
    (cond
      (:path parts)
      `(v8dml.runtime.env/peek-obj-attribute ~runtime-env-var ~n)

      (seq (:refs parts))
      `(v8dml.runtime.env/lookup-reference ~runtime-env-var ~parts)

      :else
      `(v8dml.runtime.env/lookup-instance ~runtime-env-var [(:namespace parts) (:record parts)]))))

(defn- arg-lookup [arg]
  (cond
    (seqable? arg)
    (expr-with-arg-lookups arg)

    (li/name? arg)
    (reference-lookup-call arg)

    (symbol? arg)
    `(v8dml.runtime.env/lookup-variable ~runtime-env-var ~arg)

    :else arg))

(defn- expr-with-arg-lookups [expr]
  (let [final-args (map arg-lookup (rest expr))]
    `(~(first expr) ~@final-args)))

(defn- expr-as-fn
  "Compile compound expression to a function.
   Arguments are tranlated to lookup calls on
   the runtime context."
  [expr]
  (eval `(fn [~runtime-env-var ~current-instance-var]
           ~(expr-with-arg-lookups expr))))

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
  [ctx pat-attrs schema]
  (let [{computed :computed refs :refs
         compound :compound query :query
         :as cls-attrs} (p/classify-attributes ctx pat-attrs schema)
        fs (map #(partial build-dependency-graph %) [refs compound query])
        deps-graph (appl fs [ctx schema ug/EMPTY])
        compound2 (map (fn [[k v]] [k (expr-as-fn v)]) compound)
        refs2 (map (fn [[k v]] [k (li/path-parts v)]) refs)]
    ;; TODO: Compile queries - this should be query-parse calls to the resolver.
    ;;       All queries should be merged into one using AND.
    {:attrs (assoc cls-attrs :compound compound2 :refs refs2)
     :deps deps-graph}))

(def ^:private set-attr-opcode-fns {:computed op/set-literal-attribute
                                    :refs op/set-ref-attribute
                                    :compound op/set-compound-attribute})

(defn- begin-build-instance [rec-name attrs]
  (if-let [qattrs (:query attrs)]
    (op/query-instance [rec-name qattrs])
    (op/new-instance rec-name)))

(defn- emit-build-entity-instance [ctx rec-name attrs schema event?]
  (concat [(begin-build-instance rec-name attrs)]
          (map #(op/set-literal-attribute %) (:computed attrs))
          (map (fn [[k v]]
                 ((k set-attr-opcode-fns) v))
               (:sorted attrs))
          [(if event?
             (op/intern-event-instance rec-name)
             (op/intern-instance rec-name))]))

(defn- sort-attributes-by-dependency [attrs deps-graph]
  (let [sorted (p/sort-attributes-by-dependency attrs deps-graph)]
    (assoc attrs :sorted sorted)))

(defn- emit-realize-instance [ctx pat-name pat-attrs schema event?]
  (when-let [xs (cv/invalid-attributes pat-attrs schema)]
    (u/throw-ex (str "invalid attributes in pattern - " xs)))
  (let [{attrs :attrs deps-graph :deps} (parse-attributes ctx pat-attrs schema)
        sorted-attrs (sort-attributes-by-dependency attrs deps-graph)]
    (emit-build-entity-instance ctx pat-name sorted-attrs schema event?)))

(defn- emit-realize-entity-instance [ctx pat-name pat-attrs schema]
  (emit-realize-instance ctx pat-name pat-attrs schema false))

(defn- emit-realize-record-instance [ctx pat-name pat-attrs schema]
  (emit-realize-instance ctx pat-name pat-attrs schema false))

(defn- emit-realize-event-instance [ctx pat-name pat-attrs schema]
  (emit-realize-instance ctx pat-name pat-attrs schema true))

(defn- emit-realize-map [ctx pat]
  )

(defn- compile-pathname [ctx pat]
  (let [{namespace :namespace record :record refs :refs
         path :path} (li/path-parts pat)
        n (or path [namespace record])
        opc (and (cv/find-schema n)
                 (if refs
                   (emit-load-references ctx n refs)
                   (emit-load-instance-by-name ctx n)))]
    (ctx/put-record! ctx n {})
    opc))

(defn- compile-map [ctx pat]
  (if (li/instance-pattern? pat)
    (let [full-nm (li/instance-pattern-name pat)
          {namespace :namespace record :record} (li/path-parts full-nm)
          nm [namespace record]
          attrs (li/instance-pattern-attrs pat)
          [tag scm] (cv/find-schema nm full-nm)]
      (let [c (case tag
                :entity emit-realize-entity-instance
                :record emit-realize-record-instance
                :event emit-realize-event-instance
                (u/throw-ex (str "not a valid instance pattern - " pat)))
            opc (c ctx nm attrs scm)]
        (ctx/put-record! ctx nm pat)
        opc))
    (emit-realize-map ctx pat)))

(defn- compile-command [ctx pat]
  )

(defn compile-pattern [lookup-resolver ctx pat]
  (if-let [[c resolver-path]
           (cond
             (li/pathname? pat) [compile-pathname pat]
             (map? pat) [compile-map (when (li/instance-pattern? pat)
                                       (li/instance-pattern-name pat))]
             (vector? pat) [compile-command (first pat)])]
    {:opcode (c ctx pat) :resolver (lookup-resolver resolver-path)}
    (u/throw-ex (str "cannot compile invalid pattern - " pat))))

(defn- compile-dataflow [lookup-resolver evt-pattern df-patterns]
  (let [cmp (partial compile-pattern lookup-resolver (make-context))
        ec (cmp evt-pattern)
        pc (map cmp df-patterns)]
    [ec pc]))

(defn compiled-dataflows-for-event [lookup-resolver event]
  (let [dfs (n/dataflows-for-event event)]
    (doseq [df dfs]
      (when-not (n/dataflow-opcode df)
        (n/set-dataflow-opcode!
         df (compile-dataflow
             lookup-resolver
             (n/dataflow-event-pattern df)
             (n/dataflow-patterns df)))))
    dfs))

(defn- reference-attributes [attrs refrec]
  (when-let [result (seq (filter (partial n/attribute-unique-reference-path refrec) attrs))]
    (let [[attr-name path] (first result)
          {refs :refs} (li/path-parts (:ref path))]
      [attr-name (first refs)])))

(defn- arg-lookup-fn [rec-name attrs attr-names aname arg]
  (cond
    (= aname arg)
    (u/throw-ex (str "self-reference in attribute expression - " [rec-name aname]))

    (some #{arg} attr-names)
    `(~arg ~current-instance-var)

    :else
    (let [{namespace :namespace rec :record refs :refs} (li/path-parts arg)]
      (if-let [[refattr ukattr] (reference-attributes attrs [namespace rec])]
        `(~(first refs)
          (first
           (v8dml.runtime.env/lookup-instances-by-attributes
            ~runtime-env-var
            ~[namespace rec] [[~ukattr (~refattr ~current-instance-var)]])))
        (u/throw-ex (str "no unique reference can be traced from " [rec-name aname arg]))))))

(defn compile-attribute-expression [rec-name attrs aname aval]
  (when-not aval
    (u/throw-ex (str "attribute expression cannot be nil - " [rec-name aname])))
  (let [fexprs (map (partial arg-lookup-fn rec-name attrs (keys attrs) aname) (rest aval))
        exp `(fn [~runtime-env-var ~current-instance-var]
               (~(first aval) ~@fexprs))]
    (eval exp)))
