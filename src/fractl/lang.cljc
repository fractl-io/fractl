(ns fractl.lang
  "The core constructs of the modeling language."
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.walk :as w]
            [fractl.util :as u]
            [fractl.util.seq :as us]
            [fractl.meta :as mt]
            [fractl.util.logger :as log]
            [fractl.lang.internal :as li]
            [fractl.lang.kernel :as k]
            [fractl.lang.raw :as raw]
            [fractl.lang.rbac :as lr]
            [fractl.component :as cn]
            [fractl.compiler :as c]
            [fractl.compiler.rule :as rl]
            [fractl.evaluator.state :as es]
            [fractl.compiler.context :as ctx]
            [fractl.resolvers]))

(defn- normalize-imports [imports]
  (let [imps (rest imports)]
    (if (keyword? (first imps))
      [imps]
      imps)))

(def ^:private component-spec-validators
  {:clj-import li/validate-clj-imports})

(defn- validate-component-spec [spec]
  (into
   {}
   (mapv
    (fn [[k v]]
      (let [vf (or (k component-spec-validators)
                   identity)]
        [k (vf v)]))
    spec)))

(def ^:private models (u/make-cell {}))

(defn model [spec]
  (let [n (:name spec)]
    (when-not n
      (u/throw-ex "model name is required"))
    (let [v (:version spec)
          version (or v "0.0.1")
          spec (if-not v (assoc spec :version version) spec)]
      (u/safe-set models (assoc @models n spec))
      (log/info (str "model: " (name n) ", " version))
      n)))

(defn fetch-model [name] (get @models name))
(defn model-version [name] (:version (fetch-model name)))

(defn component
  "Create and activate a new component with the given name."
  ([n spec]
   (let [ns-name (li/validate-name n)]
     (when (cn/component-exists? ns-name)
       (cn/remove-component ns-name))
     (let [r (cn/create-component
              ns-name
              (when spec
                (validate-component-spec spec)))]
       (when-let [imps (:clj-import spec)]
         (li/do-clj-import imps))
       (and (raw/component n spec)
            r))))
  ([n] (component n nil)))

(defn- attribute-type? [nm]
  (or (k/kernel-type? nm)
      (cn/find-attribute-schema nm)
      (cn/find-record-schema nm)))

(defn- rewrite-ref-path [scm]
  (if-let [p (:ref scm)]
    (assoc scm :ref (li/path-parts p))
    scm))

(defn- normalize-attribute-schema [scm]
  (rewrite-ref-path (if (fn? scm)
                      {:check scm}
                      scm)))

(defn- normalize-oneof-values [xs]
  (map #(if (keyword? %)
          (name %)
          %)
       xs))

(defn- assoc-oneof-default [scm]
  (if-let [d (:default scm)]
    (assoc scm :default
           (if (keyword? d)
             (name d)
             d))
    scm))

(defn- oneof-as-check
  "Convert a {:oneof [a b c]} to {:check #(some #{%} [a b c])}."
  [attr-schema]
  (if-let [xs (seq (:oneof attr-schema))]
    (let [values (set (normalize-oneof-values xs))]
      (assoc (assoc-oneof-default attr-schema)
             :check (fn [x] (some #{x} values))))
    attr-schema))

(defn- reference-exists? [path]
  (if-let [scm (cn/find-attribute-schema path)]
    true
    false))

(defn- query-pattern? [a-map]
  (when-not (:eval a-map)
    (let [ks (keys a-map)
          k (first ks)]
      (and (= 1 (count ks))
           (li/name? k)
           (map? (get a-map k))))))

(defn- fn-or-name? [x]
  (or (fn? x) (li/name? x)))

(defn- encryption? [x]
  ;; true/:default means use the default encryption algorithm.
  ;; In future, specific algorithms may be supported
  ;; as an enum of keywords, e.g - :bcrypt, :sha512 etc.
  (or (= true x)
      (= :default x)))

(def ^:private eval-block-keys #{:patterns :refresh-ms :timeout-ms :opcode})

(defn- eval-block? [x]
  (and (map? x)
       (every?
        #(some #{%} eval-block-keys)
        (keys x))))

(defn- listof-spec? [x]
  (cond
    (keyword? x)
    (attribute-type? x)

    (map? x)
    (reference-exists? (:ref x))

    :else (u/throw-ex (str "invalid :listof specification - " x))))

(defn- meta-specs [attrs]
  (let [meta (:meta attrs)
        ui-spec (:ui attrs)
        rbac-spec (:rbac attrs)
        meta-ui (merge (:ui meta) ui-spec)
        meta-rbac ((if (map? rbac-spec) merge concat) (:rbac meta) rbac-spec)]
    [(merge meta (when (seq meta-ui) {:ui meta-ui})
            (when (seq meta-rbac) {:rbac meta-rbac}))
     (dissoc attrs :meta :ui :rbac)]))

(defn- merge-attribute-meta [scm]
  (let [[meta scm] (meta-specs scm)]
    (assoc scm :meta meta)))

(defn- finalize-raw-attribute-schema [scm]
  (doseq [[k v] scm]
    (case k
      (:unique :immutable :optional :identity
      :path-identity :indexed :write-only :read-only
      :cascade-on-delete :var :secure-hash)
      (li/validate-bool k v)

      :check (li/validate fn? ":check is not a predicate" v)
      :default (when-not (fn? v)
                 (when-let [predic (:check scm)]
                   (li/validate predic "invalid value for :default" v)))
      :type (li/validate attribute-type? "invalid :type" v)
      :expr (li/validate fn-or-name? ":expr has invalid value" v)
      :eval (li/validate eval-block? ":eval has invalid value" v)
      :query (li/validate fn? ":query must be a compiled pattern" v)
      :format (li/validate string? ":format must be a textual pattern" v)
      :listof (li/validate listof-spec? ":listof has invalid type" v)
      :setof (li/validate attribute-type? ":setof has invalid type" v)
      :type-in-store (li/validate string? ":type-in-store must be specified as a string" v)
      :ref (li/validate reference-exists? ":ref is invalid" v)
      :writer (li/validate fn? ":writer must be a function" v)
      :oneof v
      :label (li/validate symbol? ":label must be a symbol" v)
      (:ui :rbac :meta) v
      (u/throw-ex (str "invalid constraint in attribute definition - " k))))
  (merge-attribute-meta
   (merge
    {:unique false :immutable false}
    (if-let [fmt (:format scm)]
      (assoc scm :format (partial re-matches (re-pattern fmt)) :format-str fmt)
      scm))))

(defn- find-ref-type [path]
  (when-let [scm (cn/find-attribute-schema path)]
    (or (:type scm)
        (when-let [rpath (:ref scm)]
          (find-ref-type rpath)))))

(defn- maybe-assoc-ref-type [attrscm]
  (if (:type attrscm)
    attrscm
    (if-let [rpath (and (not (:listof attrscm)) (:ref attrscm))]
      (if-let [tp (find-ref-type rpath)]
        (assoc attrscm :type tp)
        attrscm)
      attrscm)))

(defn- attr-type-spec [attr-spec]
  (when-let [p (some #{:type :listof :setof} (keys attr-spec))]
    [p (p attr-spec)]))

(defn- normalize-kernel-types [attrs]
  (let [r (mapv (fn [[k v]]
                  [k (cond
                       (keyword? v)
                       (k/normalize-kernel-type v)

                       (map? v)
                       (if-let [[p t] (attr-type-spec v)]
                         (assoc v p (k/normalize-kernel-type t))
                         v)

                       :else v)])
                attrs)]
    (into {} r)))

(defn- validate-attribute-schema-map-keys [scm]
  (let [newscm (maybe-assoc-ref-type
                (finalize-raw-attribute-schema
                 (oneof-as-check
                  (normalize-kernel-types scm))))]
    (cond
      (:unique newscm)
      (assoc newscm :indexed true)

      (:identity newscm)
      (assoc newscm :indexed true :unique true)

      :else newscm)))

(defn- validate-attribute-schema [n scm]
  (if (fn? scm)
    scm
    (validate-attribute-schema-map-keys
     (li/validate map? (str n " - attribute specification should be a map") scm))))

(defn- validated-canonical-type-name
  ([validate-name n]
   (let [validate-name (or validate-name li/validate-name)
         canon (cn/canonical-type-name n)
         [c n] (li/split-path canon)]
     (when (and (not= c k/kernel-lang-component) (k/plain-kernel-type? n))
       (log/warn (str "redefinition of kernel type "
                      n " will always require the fully-qualified name - "
                      canon)))
     (validate-name canon)))
  ([n] (validated-canonical-type-name nil n)))

(defn- intern-attribute
  "Add a new attribute definition to the component."
  ([validate-name n scm]
   (let [r (cn/intern-attribute
            (validate-name n)
            (normalize-attribute-schema
             (validate-attribute-schema n scm)))]
     (and (raw/attribute n scm) r)))
  ([n scm]
   (intern-attribute li/validate-name-relaxed n scm)))

(def attribute (partial
                intern-attribute
                (partial validated-canonical-type-name li/validate-name-relaxed)))

(defn- validate-attributes [attrs]
  (doseq [[k v] attrs]
    (li/validate-name k)
    (cond
      (keyword? v) (when-not (attribute-type? v)
                     (u/throw-ex (str "type not defined - " v)))
      (map? v) (validate-attribute-schema-map-keys v)
      (not (list? v)) (u/throw-ex (str "invalid attribute specification - " v))))
    attrs)

(defn- query-eval-fn [recname attrs k v]
  ;; TODO: implement the query->fn compilation phase.
  (u/throw-ex (str k " - inline compilation for :query not implemented")))

(defn- attref? [n]
  (let [[[_ _] a] (li/ref-as-names n)]
    (if a true false)))

(defn- compile-eval-block [recname attrs evblock]
  (let [ctx (ctx/make)]
    (ctx/put-record! ctx (li/split-path recname) attrs)
    (if-let [opcode (mapv (partial c/compile-pattern ctx) (:patterns evblock))]
      opcode
      (u/throw-ex (str recname " - failed to compile eval-block")))))

(defn- normalize-eval-block [evblock]
  (when evblock
    (if (and (map? evblock) (:patterns evblock))
      evblock
      {:patterns [evblock]})))

(defn- normalize-compound-attr [recname attrs nm [k v]]
  (if-let [ev (normalize-eval-block (:eval v))]
    (attribute
     nm
     (assoc
      v
      :eval (assoc ev :opcode (compile-eval-block recname attrs ev))
      :optional true))
    (when-let [expr (:expr v)]
      (if (fn? expr)
        (attribute nm v)
        (attribute
         nm
         (merge (if-let [t (:type v)]
                  {:type t}
                  (u/throw-ex (str ":type is required for attribute " k " with compound expression")))
                {:expr (c/compile-attribute-expression recname attrs k expr)}))))))

(defn- normalize-attr [recname attrs fqn [k v]]
  (let [newv
        (cond
          (map? v)
          (let [v (if (:read-only v) (assoc v :optional true) v)
                nm (fqn (li/unq-name))]
            (if (query-pattern? v)
              (attribute nm {:query (query-eval-fn recname attrs k v)})
              (or (normalize-compound-attr recname attrs nm [k v])
                  (attribute nm v))))
          (list? v)
          (attribute
           (fqn (li/unq-name))
           {:expr (c/compile-attribute-expression
                   recname attrs k v)})
          :else
          (let [fulln (fqn v)]
            (if (attref? fulln)
              (attribute
               (fqn (li/unq-name))
               {:ref fulln})
              fulln)))]
    [k newv]))

(defn- required-attribute-names [attrs]
  (map first
       (filter (fn [[a v]]
                 (if (map? v)
                   (not (or (:optional v) (:default v)))
                   (if-let [scm (and (keyword? v) (cn/find-attribute-schema v))]
                     (not (or (:optional scm) (:default scm)))
                     true)))
               attrs)))

(defn- infer-default [attr-name attr-def dict?]
  (let [type-name (if dict? (:type attr-def) attr-def)
        scm (cn/find-attribute-schema type-name)]
    (if scm
      (if-let [d (:default scm)]
        (if dict?
          (assoc attr-def :default d)
          {:type type-name
           :default d})
        (u/throw-ex (str attr-name " - no default defined for " type-name)))
      (u/throw-ex (str attr-name " - undefined type - " type-name)))))

(defn- assoc-defaults [req-attrs [aname adef]]
  (let [optional? (not (some #{aname} req-attrs))
        dict? (map? adef)]
    (when (and (not optional?) dict? (:optional adef))
      (u/throw-ex (str aname " - cannot be marked :optional")))
    [aname (if optional?
             (if (and dict? (:default adef))
               adef
               (if dict?
                 (assoc adef :optional true)
                 {:type adef :optional true}))
             adef)]))

(defn- merge-unique-flags [attrs uq-attr-names]
  (map (fn [[k v]]
         (if (some #{k} uq-attr-names)
           [k (if (keyword? v)
                {:type v
                 :unique true}
                (assoc v :unique true))]
           [k v]))
       attrs))

(defn- fetch-inherited-schema [type-name child-record-type]
  (if-let [scm (case child-record-type
                 (:entity :record)
                 (or (raw/find-entity type-name)
                     (raw/find-record type-name))
                 :event (or (raw/find-event type-name)
                            (raw/find-record type-name)))]
    (let [final-scm (into {} (mapv (fn [[k v]]
                                     (if (keyword? v)
                                       [k (k/normalize-kernel-type v)]
                                       [k v]))
                                   scm))]
      (if-let [inherits (:inherits (:meta scm))]
        (merge final-scm (fetch-inherited-schema inherits child-record-type))
        final-scm))
    (u/throw-ex (str "parent type not found - " type-name))))

(defn- preproc-for-built-in-attrs [attrs]
  (let [attrs (mapv (fn [[k v]]
                      [k (if (keyword? v)
                           (cond
                             (or (= v :Identity)
                                 (= v :Fractl.Kernel.Lang/Identity))
                             (cn/find-attribute-schema :Fractl.Kernel.Lang/Identity)

                             (or (= v :Now)
                                 (= v :Fractl.Kernel.Lang/Now))
                             (cn/find-attribute-schema :Fractl.Kernel.Lang/Now)

                             :else (let [[c n] (li/split-path v)]
                                     (if (and c n (not= :Fractl.Kernel.Lang c))
                                       (or (raw/find-attribute v) v)
                                       v)))
                           v)])
                    attrs)]
    (into {} attrs)))

(defn- normalized-attributes [rectype recname orig-attrs]
  (let [f (partial cn/canonical-type-name (cn/get-current-component))
        orig-attrs (normalize-kernel-types orig-attrs)
        [meta base-attrs] (meta-specs orig-attrs)
        inherits (:inherits meta)
        inherited-scm (when inherits (fetch-inherited-schema inherits rectype))
        req-inherited-attrs (or (:required-attributes (:meta inherited-scm))
                                (required-attribute-names inherited-scm))
        attrs (if inherited-scm
                (merge (li/only-user-attributes inherited-scm) base-attrs)
                base-attrs)
        req-orig-attrs (or (:required-attributes meta)
                           (required-attribute-names base-attrs))
        req-attrs (concat req-orig-attrs req-inherited-attrs)
        attrs-with-defaults (into {} (map (partial assoc-defaults req-attrs) attrs))
        newattrs (map (partial normalize-attr recname attrs f) attrs-with-defaults)
        final-attrs (into {} (validate-attributes newattrs))]
    (assoc final-attrs :meta (assoc meta :required-attributes req-attrs :record-type rectype))))

(defn- parse-and-define [f schema]
  (let [n (first (keys schema))]
    (f n (get schema n))))

(defn record
  "Add a new record definition to the component."
  ([n attrs]
   (if (map? attrs)
     (let [cn (validated-canonical-type-name n)
           r (cn/intern-record
              cn (normalized-attributes :record cn attrs))]
       (when r
         (and (cn/raw-definition cn attrs)
              (if-not (:contains (:meta attrs))
                (raw/record n attrs)
                n)
              r)))
     (u/throw-ex (str "Syntax error in record. Check record: " n))))
  ([schema]
   (parse-and-define record schema)))

(defn- event-internal
  ([n attrs verify-name?]
   (let [cn (if verify-name?
              (validated-canonical-type-name n)
              (cn/canonical-type-name n))
         r (cn/intern-event
            cn
            (if (cn/inferred-event-schema? attrs)
              attrs
              (normalized-attributes :event cn attrs)))]
     (when r
       (and (cn/raw-definition cn attrs) r))))
  ([n attrs]
   (event-internal n attrs false)))

(defn- ensure-no-reserved-event-attrs! [attrs]
  (when (some #(= li/event-context (first %)) attrs)
    (u/throw-ex "li/event-context is a reserved attribute name")))

(defn event
  "An event record with timestamp and other auto-generated meta fields."
  ([n attrs]
   (ensure-no-reserved-event-attrs! attrs)
   (let [r (event-internal
            n (assoc attrs li/event-context (k/event-context-attribute-name))
            true)]
     (and (raw/event n attrs) r)))
  ([schema]
   (parse-and-define event schema)))

(defn- intern-inferred-event [nm]
  (event nm cn/inferred-event-schema))

(defn ensure-event! [x]
  (if-let [n (li/record-name x)]
    (when-not (cn/find-event-schema n)
      (intern-inferred-event n))
    (u/throw-ex (str "not an event - " x))))

(defn ensure-dataflow-pattern! [x]
  (cond
    (keyword? x) (li/validate-name x)
    (or (map? x) (li/special-form? x) (symbol? x)) x
    :else (u/throw-ex (str "Invalid dataflow pattern. Possible syntax error - " x))))

(defn ensure-dataflow-patterns! [xs]
  (doseq [x xs] (ensure-dataflow-pattern! x)))

(declare normalize-event-pattern)

(defn- normalize-event-pattern-attribute [[k v]]
  (cond
    (map? v)
    [k (normalize-event-pattern v)]

    (symbol? v)
    [k `(quote ~v)]

    :else [k v]))

(defn- normalize-event-pattern [pattern]
  (if (map? pattern)
    (let [attrs (map normalize-event-pattern-attribute (first (vals pattern)))]
      (into {(first (keys pattern)) (into {} attrs)}))
    pattern))

(defn- extract-on-and-where [match-pat]
  (if (= (count match-pat) 7)
    (do
      (when-not (= :on (nth match-pat 3))
        (u/throw-ex (str ":on keyword not found - " match-pat)))
      (when-not (= :where (nth match-pat 5))
        (u/throw-ex (str ("where clause not found - " match-pat))))
      [(li/validate-on-clause (nth match-pat 4))
       (li/validate-where-clause (nth match-pat 6))])
    (u/throw-ex (str ":on and :where clauses expected - " match-pat))))

(defn- install-event-trigger-pattern [match-pat]
  (let [event-name (first match-pat)]
    (when-not (li/name? event-name)
      (u/throw-ex (str "not a valid event name - " event-name)))
    (when-not (= :when (second match-pat))
      (u/throw-ex (str "expected keyword :when not found - " match-pat)))
    (let [pat (nth match-pat 2)
          predic (rl/compile-rule-pattern pat)
          rnames (li/referenced-record-names pat)
          [on where] (when (> (count rnames) 1)
                       (extract-on-and-where match-pat))
          event-attrs (li/references-to-event-attributes rnames)
          evt-name (event event-name event-attrs)]
      (cn/install-triggers!
       (or on rnames)
       event-name predic where rnames)
      evt-name)))

(defn- concat-refs [n refs]
  (keyword (str (subs (str n) 1) "."
                (s/join "." (mapv name refs)))))

(defn- rewrite-on-event-patterns [pats recname evtname]
  (let [sn (li/split-path recname)
        refs-prefix [:Instance]]
    (w/postwalk
     #(if (li/name? %)
        (let [{c :component n :record
               p :path r :refs} (li/path-parts %)
              refs (seq r)
              cn [c n]]
          (if (or (= cn sn) (= sn (li/split-path p)))
            (if (not refs)
              (concat-refs evtname refs-prefix)
              (concat-refs evtname (concat refs-prefix refs)))
            %))
        %)
     pats)))

(defn- event-self-ref-pattern [event-name]
  (if-let [scm (:schema (cn/find-event-schema event-name))]
    (let [prefix (subs (str event-name) 1)
          attrs (mapv (fn [[k _]]
                        [k (keyword (str prefix "." (name k)))])
                      scm)]
      [{event-name (into {} attrs)}])
    (u/throw-ex (str "cannot auto-generate dataflow patterns, event schema not found - " event-name))))

(defn dataflow
  "A declarative data transformation pipeline."
  [match-pat & patterns]
  (let [r (if (not (seq patterns))
            (apply dataflow match-pat (event-self-ref-pattern match-pat))
            (do
              (ensure-dataflow-patterns! patterns)
              (if (vector? match-pat)
                (apply
                 dataflow
                 (install-event-trigger-pattern match-pat)
                 patterns)
                (let [hd (:head match-pat)]
                  (if-let [mt (and hd (:on-entity-event hd))]
                    (cn/register-entity-dataflow mt hd patterns)
                    (let [event (normalize-event-pattern (if hd (:on-event hd) match-pat))]
                      (do (ensure-event! event)
                          (cn/register-dataflow event hd patterns))))))))]
    (when r
      (raw/dataflow match-pat patterns)
      r)))

(def ^:private crud-evname cn/crud-event-name)

(defn- crud-event-attr-accessor
  ([evtname use-name? attr-name]
   (keyword (str (if use-name? (name evtname) (subs (str evtname) 1)) "." (name attr-name))))
  ([evtname attr-name]
   (crud-event-attr-accessor evtname false attr-name)))

(defn- crud-event-subattr-accessor [evtname attr sub-attr]
  (keyword (str (name evtname) (str "." (name attr))
                (when sub-attr
                  (str "." (name sub-attr))))))

(defn- crud-event-inst-accessor
  ([evtname canonical? inst-attr]
   (let [r (crud-event-subattr-accessor evtname :Instance inst-attr)]
     (if canonical? (cn/canonical-type-name r) r)))
  ([evtname inst-attr] (crud-event-inst-accessor evtname true inst-attr))
  ([evtname] (crud-event-inst-accessor evtname true nil)))

(defn- direct-id-accessor [evtname id-attr]
  (cn/canonical-type-name
   (keyword (str (name evtname) "." (name id-attr)))))

(defn- identity-attribute-name [recname]
  (or (cn/identity-attribute-name recname)
      cn/id-attr))

(defn- identity-attribute-type [attr-name attrs]
  (if (= attr-name cn/id-attr)
    cn/id-attr-type
    (let [t (attr-name attrs)]
      (if (keyword? t)
        (if-let [ascm (cn/find-attribute-schema t)]
          (:type ascm)
          t)
        (:type t)))))

(defn- crud-event-delete-pattern [evtname entity-name]
  (let [id-attr (identity-attribute-name entity-name)]
    [:delete entity-name
     {id-attr (direct-id-accessor evtname id-attr)}]))

(defn- crud-event-lookup-pattern [evtname entity-name]
  (let [id-attr (identity-attribute-name entity-name)]
    {entity-name
     {(keyword (str (name id-attr) "?"))
      (direct-id-accessor evtname id-attr)}}))

(defn- implicit-entity-event-dfexp
  "Construct a dataflow expressions for an implicit dataflow
  lifted from the :on-entity-event property of an entity
  definition."
  [ename event-spec]
  `(dataflow {:head {:on-entity-event {~ename {cn/id-attr 'id}}
                     :when ~(:when event-spec)}}
             ~@(:do event-spec)))

(defn- lift-implicit-entity-events
  "Pick out the :on-entity-event definitions from the attributes
  into independent dataflows."
  [ename attrs]
  (if-let [especs (:on-entity-event attrs)]
    [(dissoc attrs :on-entity-event)
     (map (partial implicit-entity-event-dfexp ename) especs)]
    [attrs nil]))

(defn- entity-event [entity-name event-name event-type _]
  (let [attrs (if (= event-type :OnDelete)
                {:Instance entity-name}
                {:Instance entity-name
                 :OldInstance entity-name})]
    (event-internal event-name attrs)))

(defn- has-identity-attribute? [attrs]
  (some (fn [[_ v]]
          (cn/identity-attribute?
           (if (keyword? v)
             (cn/find-attribute-schema v)
             v)))
        attrs))

(defn- maybe-assoc-id [entity-name attrs]
  (if (or (cn/entity-schema-predefined? entity-name)
          (has-identity-attribute? attrs))
    attrs
    (let [attrs (assoc
                 attrs cn/id-attr
                 (cn/canonical-type-name cn/id-attr))
          meta (:meta attrs)
          req-attrs (:required-attributes meta)]
      (assoc attrs :meta
             (assoc meta :required-attributes
                    (set (conj req-attrs cn/id-attr)))))))

(defn- load-ref-pattern [evt-name evt-ref entity-name attr-name attr-schema]
  (let [[c _] (li/split-path entity-name)
        r (:ref attr-schema)]
    {(li/make-path (:component r) (:record r))
     {(keyword (str (name (first (:refs r))) "?"))
      (keyword (str (name c) "/" (name evt-name) "." (name evt-ref) "." (name attr-name)))}}))

(defn- serialize-record [f cn attrs raw-attrs]
  (when-let [r (f cn attrs)]
    (and (cn/raw-definition cn raw-attrs) r)))

(def ^:private intern-rec-fns
  {:entity (partial serialize-record cn/intern-entity)
   :relationship (partial serialize-record cn/intern-relationship)})

(defn- maybe-add-curd-meta [attrs]
  (let [meta (:meta attrs)]
    (if (some #{li/owner-exclusive-crud} (keys meta))
      attrs
      (assoc attrs :meta (assoc meta li/owner-exclusive-crud true)))))

(defn- serializable-record
  ([rectype n attrs raw-attrs]
   (if-let [intern-rec (rectype intern-rec-fns)]
     (if (map? attrs)
       (let [rec-name (validated-canonical-type-name
                       (when (cn/system-defined? attrs) identity)
                       n)
             [attrs dfexps] (lift-implicit-entity-events rec-name attrs)
             attrs (maybe-add-curd-meta (assoc attrs li/meta-attr li/meta-attr-spec))
             result (intern-rec
                     rec-name
                     (maybe-assoc-id
                      rec-name
                      (normalized-attributes
                       rectype rec-name attrs))
                     raw-attrs)
             ev (partial crud-evname n)
             ctx-aname (k/event-context-attribute-name)
             id-attr (identity-attribute-name rec-name)
             id-attr-type (or (identity-attribute-type id-attr attrs)
                              :Fractl.Kernel.Lang/Any)
             id-evattrs {id-attr id-attr-type
                         li/event-context ctx-aname}
             cr-evattrs {:Instance n li/event-context ctx-aname}
             up-id-attr id-attr
             up-evattrs {up-id-attr id-attr-type
                         :Data :Fractl.Kernel.Lang/Map}
             ;; Define CRUD events and dataflows:
             crevt (ev :Create)
             upevt (ev :Update)
             delevt (ev :Delete)
             lookupevt (ev :Lookup)
             lookupevt-internal (ev cn/lookup-internal-event-prefix)
             lookupallevt (ev :LookupAll)]
         (cn/for-each-entity-event-name
          rec-name (partial entity-event rec-name))
         (event-internal delevt id-evattrs)
         (cn/register-dataflow delevt [(crud-event-delete-pattern delevt rec-name)])
         (event-internal crevt cr-evattrs)
         (event-internal upevt up-evattrs)
         (event-internal lookupevt-internal id-evattrs)
         (event-internal lookupevt id-evattrs)
         (event-internal lookupallevt {})
         (let [rs (mapv (fn [[k v]]
                          (let [s (cn/find-attribute-schema v)]
                            [(load-ref-pattern crevt :Instance rec-name k s)
                             (load-ref-pattern upevt :Instance rec-name k s)]))
                        (cn/ref-attribute-schemas (cn/fetch-schema rec-name)))
               cr-ref-pats (mapv first rs)
               up-ref-pats (mapv second rs)]
           (cn/register-dataflow crevt `[~@cr-ref-pats ~(crud-event-inst-accessor crevt)])
           (cn/register-dataflow
            upevt
            (concat [up-ref-pats]
                    [{rec-name
                      {(li/name-as-query-pattern id-attr) (crud-event-attr-accessor upevt (name up-id-attr))}
                      :from (crud-event-attr-accessor upevt "Data")}])))
         (cn/register-dataflow lookupevt-internal [(crud-event-lookup-pattern lookupevt-internal rec-name)])
         (cn/register-dataflow lookupevt [(crud-event-lookup-pattern lookupevt rec-name)])
         (cn/register-dataflow lookupallevt [(li/name-as-query-pattern rec-name)])
         ;; Install dataflows for implicit events.
         (when dfexps (mapv eval dfexps))
         (let [rbac-spec (:rbac attrs)
               is-rel (:relationship (:meta attrs))]
           (lr/rbac rec-name rbac-spec))
         result)
       (u/throw-ex (str "Syntax error. Check " (name rectype) ": " n)))
     (u/throw-ex (str "Not a serializable record type: " (name rectype)))))
  ([rectype n attrs]
   (serializable-record rectype n attrs attrs)))

(def serializable-entity (partial serializable-record :entity))

(defn entity
  "A record that can be persisted with a unique id."
  ([n attrs raw-attrs]
   (when-let [r (serializable-entity n (preproc-for-built-in-attrs attrs))]
     (and (if raw-attrs (raw/entity n raw-attrs) true) r)))
  ([n attrs]
   (let [raw-attrs attrs
         attrs (if-not (seq attrs)
                 {li/id-attr :Fractl.Kernel.Lang/Identity}
                 attrs)]
     (entity n attrs raw-attrs)))
  ([schema] (parse-and-define entity schema)))

(defn- parse-relationship-member-spec [spec]
  (let [elems [(first spec) (second spec)]
        meta (seq (rest (rest spec)))]
    (if meta
      [elems (us/wrap-to-map meta)]
      [elems nil])))

(defn- evt-path-attr [evt]
  (crud-event-attr-accessor evt li/path-attr))

(defn- regen-contains-dataflows [relname [parent child]]
  (let [ev (partial crud-evname child)
        attr-names (cn/attribute-names (cn/fetch-schema child))
        ctx-aname (k/event-context-attribute-name)]
    (let [crevt (ev :Create)
          cr-path (evt-path-attr crevt)]
      (event-internal
       crevt
       {:Instance child
        li/path-attr {:type :Fractl.Kernel.Lang/String
                      :default li/default-path}
        li/event-context ctx-aname})
      (cn/register-dataflow
       (ev :Create)
       [{child
         (merge
          (into
           {} (mapv
               (fn [a]
                 [a (if (= a li/path-attr)
                      ;; The path-identity will be appended by the evaluator.
                      cr-path
                      (crud-event-inst-accessor crevt a))])
               attr-names))
          {li/path-attr cr-path})}]))
    (let [upevt (ev :Update)]
      (event-internal
       upevt
       {:Data :Fractl.Kernel.Lang/Map
        li/path-attr :Fractl.Kernel.Lang/String
        li/event-context ctx-aname})
      (cn/register-dataflow
       upevt
       [{child
         {li/path-attr? (evt-path-attr upevt)}
         :from (crud-event-attr-accessor upevt :Data)}]))
    (let [lookupevt (ev :Lookup)
          lookupallevt (ev :LookupAll)
          evattrs {li/path-attr :Fractl.Kernel.Lang/String
                   li/event-context ctx-aname}
          child-q (li/name-as-query-pattern child)]
      (event-internal lookupevt evattrs)
      (event-internal lookupallevt evattrs)
      (cn/register-dataflow lookupevt [{child {li/path-attr? (evt-path-attr lookupevt)}}])
      (cn/register-dataflow lookupallevt [{child {li/path-attr? [:like (evt-path-attr lookupallevt)]}}]))
    (let [delevt (ev :Delete)]
      (event-internal delevt {li/path-attr :Fractl.Kernel.Lang/String
                              li/event-context ctx-aname})
      (cn/register-dataflow
       delevt [[:delete child {li/path-attr (evt-path-attr delevt)}]]))
    relname))

(declare relationship)

(defn- regen-between-relationships [contains-child]
  (doseq [[rel _ _] (seq (cn/between-relationships contains-child))]
    (let [old-def (raw/find-relationship rel)]
      (cn/remove-record rel)
      (relationship rel old-def)))
  contains-child)

(defn- validate-rbac-owner [rbac nodes]
  (when-let [own (li/owner rbac)]
    (when-not (some #{own} nodes)
      (u/throw-ex (str "invalid rbac owner node " own)))))

(defn- user-defined-identity-attribute-name [entity-name]
  (let [ident (cn/identity-attribute-name entity-name)]
    (when (= ident cn/id-attr)
      (u/throw-ex (str "User-defined identity attribute required for " entity-name)))
    ident))

(defn- regen-contains-child-attributes [child meta]
  (if-not (cn/path-identity-attribute-name child)
    (let [cident (user-defined-identity-attribute-name child)
          child-attrs (preproc-for-built-in-attrs
                       (raw/entity-attributes-include-inherits child))
          cident-raw-spec (cident child-attrs)
          cident-spec (if (map? cident-raw-spec)
                        cident-raw-spec
                        (cn/find-attribute-schema cident-raw-spec))]
      (when-let [a (some li/reserved-attrs (map #(keyword (s/upper-case (name %))) (keys child-attrs)))]
        (u/throw-ex (str child "." a " - attribute name is reserved")))
      (assoc
       child-attrs
       cident (merge
               (if (:globally-unique meta)
                 cident-spec
                 (dissoc cident-spec :identity))
               {:type (or (:type cident-spec) :UUID)
                :path-identity true
                :indexed true}
               (when-not cident-spec ;; __Id__
                 {:default u/uuid-string}))
       li/path-attr li/path-attr-spec))
    (cn/fetch-entity-schema child)))

(defn- cleanup-rel-attrs [attrs]
  (dissoc attrs :meta :rbac :ui))

(defn- contains-relationship [relname attrs relmeta [_ child :as elems]]
  (when (seq (keys (cleanup-rel-attrs attrs)))
    (u/throw-ex (str "attributes not allowed for a contains relationship - " relname)))
  (let [raw-attrs attrs
        meta (:meta attrs)
        attrs (assoc attrs :meta
                     (assoc meta cn/relmeta-key
                            relmeta :relationship :contains))
        child-attrs (regen-contains-child-attributes child meta)]
    (if-let [r (record relname raw-attrs)]
      (if (entity child child-attrs false)
        (if (cn/register-relationship elems relname)
          (and (regen-contains-dataflows relname elems)
               (regen-between-relationships (second elems))
               (raw/relationship relname raw-attrs) r)
          (u/throw-ex (str "failed to register relationship - " relname)))
        (u/throw-ex (str "failed to regenerate schema for " child)))
      (u/throw-ex (str "failed to define schema for " relname)))))

(defn- between-node-types [node1 node2]
  (let [id1 (cn/identity-attribute-name node1)
        t1 (cn/attribute-type node1 id1)]
    (if (= node1 node2)
      [t1 t1]
      [t1 (cn/attribute-type node2 (cn/identity-attribute-name node2))])))

(defn- assoc-relnode-attributes [attrs [node1 node2] relmeta]
  (let [[a1 a2] (li/between-nodenames node1 node2 relmeta)
        [t1 t2] (between-node-types node1 node2)]
    (when-not (and a1 a2)
      (u/throw-ex (str "failed to resolve both node-attributes for between-relationship - " [a1 a2])))
    (assoc attrs a1 t1 a2 t2)))

(defn- between-unique-meta [meta relmeta [node1 node2]]
  (let [[_ n1] (li/split-path node1)
        [_ n2] (li/split-path node2)]
    (cond
      (:one-one relmeta)
      (assoc meta :unique [n1 n2])

      (:one-n relmeta)
      (assoc meta :unique [n1])

      :else meta)))

(defn- between-relationship [relname attrs relmeta elems]
  (let [new-attrs (assoc-relnode-attributes attrs elems relmeta)
        meta (assoc (between-unique-meta (:meta attrs) relmeta elems)
                    :relationship :between cn/relmeta-key relmeta)
        r (serializable-entity relname (assoc new-attrs :meta meta) attrs)]
    (when (cn/register-relationship elems relname)
      (and (raw/relationship relname attrs) r))))

(defn relationship
  ([relation-name attrs]
   (let [meta (let [m (:meta attrs)]
                (if (some #{:cascade-on-delete} (keys m))
                  m
                  (assoc m :cascade-on-delete true)))
         contains (mt/contains meta)
         between (when-not contains (mt/between meta))
         [elems relmeta] (parse-relationship-member-spec (or contains between))]
     (when-not elems
       (u/throw-ex
        (str "type (contains, between) of relationship is not defined in meta - "
             relation-name)))
     ((if contains contains-relationship between-relationship)
      relation-name (assoc attrs :meta meta) relmeta elems)))
  ([schema]
   (let [r (parse-and-define relationship schema)
         n (li/record-name schema)]
     (and (raw/relationship n (li/record-attributes schema)) r))))

(defn- resolver-for-entity [component ename spec]
  (if (cn/find-entity-schema ename)
    (cn/install-resolver component ename spec)
    (u/throw-ex (str "cannot install resolver, schema not found for " ename))))

(defn- resolver-for-component [component spec]
  (if (cn/component-exists? component)
    (cn/install-resolver component spec)
    (u/throw-ex (str "cannot install resolver, component not found - " component))))

(def ^:private resolver-keys #{:type :compose? :config})

(defn- validate-resolver-spec [spec]
  (if (and (map? spec)
           (= resolver-keys
              (set/union resolver-keys
                         (keys spec))))
    spec
    (u/throw-ex (str "invalid key(s) in resolver spec - "
                     (set/difference (set (keys spec))
                                     resolver-keys)))))

(defn resolver
  "Add a resolver for a component or entity.
  Target should be fully-qualified name of a component or entity."
  [target spec]
  (let [spec (validate-resolver-spec spec)
        [a b] (li/split-path target)]
    (if (and a b)
      (resolver-for-entity a b spec)
      (resolver-for-component target spec))))
