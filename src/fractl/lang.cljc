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
            [fractl.component :as cn]
            [fractl.compiler :as c]
            [fractl.compiler.rule :as rl]
            [fractl.evaluator.state :as es]
            [fractl.compiler.context :as ctx]
            [fractl.resolver.registry :as r]
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

(declare init)

(defn component
  "Create and activate a new component with the given name."
  ([n spec]
   (init)
   (let [ns-name (li/validate-name n)]
     (cn/create-component
      ns-name
      (when spec
        (validate-component-spec spec)))))
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

(defn- finalize-raw-attribute-schema [scm]
  (doseq [[k v] scm]
    (case k
      :check (li/validate fn? ":check is not a predicate" v)
      :unique (li/validate-bool :unique v)
      :immutable (li/validate-bool :immutable v)
      :optional (li/validate-bool :optional v)
      :default (when-not (fn? v)
                 (when-let [predic (:check scm)]
                   (li/validate predic "invalid value for :default" v)))
      :type (li/validate attribute-type? "invalid :type" v)
      :identity (li/validate-bool :identity v)
      :expr (li/validate fn-or-name? ":expr has invalid value" v)
      :eval (li/validate eval-block? ":eval has invalid value" v)
      :query (li/validate fn? ":query must be a compiled pattern" v)
      :format (li/validate string? ":format must be a textual pattern" v)
      :listof (li/validate listof-spec? ":listof has invalid type" v)
      :setof (li/validate attribute-type? ":setof has invalid type" v)
      :indexed (li/validate-bool :indexed v)
      :write-only (li/validate-bool :write-only v)
      :type-in-store (li/validate string? ":type-in-store must be specified as a string" v)
      :ref (li/validate reference-exists? ":ref is invalid" v)
      :cascade-on-delete (li/validate-bool :cascade-on-delete v)
      :var (li/validate-bool :var v)
      :writer (li/validate fn? ":writer must be a function" v)
      :secure-hash (li/validate-bool :secure-hash v)
      :oneof v
      (u/throw-ex (str "invalid constraint in attribute definition - " k))))
  (merge
   {:unique false :immutable false}
   (if-let [fmt (:format scm)]
     (assoc scm :format (partial re-matches (re-pattern fmt)) :format-str fmt)
     scm)))

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
   (let [canon (cn/canonical-type-name n)
         [_ n] (li/split-path canon)]
     (when (k/plain-kernel-type? n)
       (log/warn (str "redefinition of kernel type "
                      n " will always require the fully-qualified name - "
                      canon)))
     (validate-name canon)))
  ([n] (validated-canonical-type-name li/validate-name n)))

(defn- intern-attribute
  "Add a new attribute definition to the component."
  ([validate-name n scm]
   (cn/intern-attribute
    (validate-name n)
    (normalize-attribute-schema
     (validate-attribute-schema n scm))))
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

(def ^:private default-expression-compiler [c/compile-attribute-expression nil])

(defn- fetch-expression-compiler [expr]
  (if (vector? expr)
    (c/expression-compiler (first expr))
    default-expression-compiler))

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
        (let [[c tag] (fetch-expression-compiler expr)]
          (when tag
            (cn/register-custom-compiled-record tag recname))
          (attribute
           nm
           (merge (if-let [t (:type v)]
                    {:type t}
                    (u/throw-ex (str ":type is required for attribute " k " with compound expression")))
                  {:expr (c recname attrs k expr)})))))))

(defn- normalize-attr [recname attrs fqn [k v]]
  (let [newv
        (cond
          (map? v)
          (let [nm (fqn (li/unq-name))]
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
       (filter (fn [[_ v]]
                 (if (map? v)
                   (not (or (:optional v) (:default v)))
                   true))
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
                 (or (cn/find-entity-schema type-name)
                     (cn/find-record-schema type-name))
                 :event (or (cn/find-event-schema type-name)
                            (cn/find-record-schema type-name)))]
    (:schema scm)
    (u/throw-ex (str "parent type not found - " type-name))))

(defn- normalized-attributes [rectype recname orig-attrs]
  (let [f (partial cn/canonical-type-name (cn/get-current-component))
        orig-attrs (normalize-kernel-types orig-attrs)
        meta (:meta orig-attrs)
        inherits (:inherits meta)
        inherited-scm (when inherits (fetch-inherited-schema inherits rectype))
        req-inherited-attrs (or (:required-attributes inherited-scm)
                                (required-attribute-names inherited-scm))
        base-attrs (dissoc orig-attrs :meta)
        attrs (if inherited-scm
                (merge inherited-scm base-attrs)
                base-attrs)
        req-orig-attrs (or (:required-attributes meta)
                           (required-attribute-names attrs))
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
     (let [cn (validated-canonical-type-name n)]
       (cn/intern-record
         cn (normalized-attributes :record cn attrs)))
     (u/throw-ex (str "Syntax error in record. Check record: " n))))
  ([schema]
   (parse-and-define record schema)))

(defn- event-internal
  ([n attrs verify-name?]
   (let [cn (if verify-name?
              (validated-canonical-type-name n)
              (cn/canonical-type-name n))]
     (cn/intern-event cn (if (cn/inferred-event-schema? attrs)
                              attrs
                              (normalized-attributes :event cn attrs)))))
  ([n attrs]
   (event-internal n attrs false)))

(defn- ensure-no-reserved-event-attrs! [attrs]
  (when (some #(= li/event-context (first %)) attrs)
    (u/throw-ex "li/event-context is a reserved attribute name")))

(defn event
  "An event record with timestamp and other auto-generated meta fields."
  ([n attrs]
   (ensure-no-reserved-event-attrs! attrs)
   (event-internal
    n (assoc attrs li/event-context (k/event-context-attribute-name))
    true))
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
  (if (not (seq patterns))
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
                  (cn/register-dataflow event hd patterns)))))))))

(defn- crud-evname [entity-name evtname]
  (cn/canonical-type-name
   (keyword (str (name evtname) "_" (name entity-name)))))

(defn- crud-event-attr-accessor
  ([evtname use-name? attr-name]
   (keyword (str (if use-name? (name evtname) (subs (str evtname) 1)) "." attr-name)))
  ([evtname attr-name]
   (crud-event-attr-accessor evtname false attr-name)))

(defn- crud-event-inst-accessor
  ([evtname canonical? inst-attr]
   (let [r (keyword (str (name evtname) ".Instance"
                         (when inst-attr
                           (str "." (name inst-attr)))))]
     (if canonical? (cn/canonical-type-name r) r)))
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

(def ^:private intern-rec-fns
  {:entity cn/intern-entity
   :relationship cn/intern-relationship})

(defn- serializable-record [rectype n attrs]
  (if-let [intern-rec (rectype intern-rec-fns)]
    (if (map? attrs)
      (let [rec-name (validated-canonical-type-name n)
            [attrs dfexps] (lift-implicit-entity-events rec-name attrs)
            result (intern-rec
                    rec-name
                    (maybe-assoc-id
                     rec-name
                     (normalized-attributes
                      rectype rec-name attrs)))
            ev (partial crud-evname n)
            ctx-aname (k/event-context-attribute-name)
            inst-evattrs {:Instance n li/event-context ctx-aname}
            id-attr (identity-attribute-name rec-name)
            id-attr-type (or (identity-attribute-type id-attr attrs)
                             :Kernel.Lang/Any)
            id-evattrs {id-attr id-attr-type
                        li/event-context ctx-aname}]
        ;; Define CRUD events and dataflows:
        (let [upevt (ev :Upsert)
              delevt (ev :Delete)
              lookupevt (ev :Lookup)
              lookupevt-internal (ev cn/lookup-internal-event-prefix)]
          (cn/for-each-entity-event-name
           rec-name (partial entity-event rec-name))
          (event-internal upevt inst-evattrs)
          (let [ref-pats (mapv (fn [[k v]]
                                 (load-ref-pattern
                                  upevt :Instance rec-name k
                                  (cn/find-attribute-schema v)))
                               (cn/ref-attribute-schemas
                                (cn/fetch-schema rec-name)))]
            (cn/register-dataflow upevt `[~@ref-pats ~(crud-event-inst-accessor upevt)]))
          (event-internal delevt id-evattrs)
          (cn/register-dataflow delevt [(crud-event-delete-pattern delevt rec-name)])
          (event-internal lookupevt-internal id-evattrs)
          (cn/register-dataflow lookupevt-internal [(crud-event-lookup-pattern lookupevt-internal rec-name)])
          (event-internal lookupevt id-evattrs)
          (cn/register-dataflow lookupevt [(crud-event-lookup-pattern lookupevt rec-name)]))
        ;; Install dataflows for implicit events.
        (when dfexps (doall (map eval dfexps)))
        result)
      (u/throw-ex (str "Syntax error. Check " (name rectype) ": " n)))
    (u/throw-ex (str "Not a serializable record type: " (name rectype)))))

(def serializable-entity (partial serializable-record :entity))

(defn- meta-entity [entity-name]
  (let [[c n :as en] (li/split-path entity-name)
        n (cn/meta-entity-name en)]
    (serializable-entity
     n
     (cn/meta-entity-attributes c))))

(defn entity
  "A record that can be persisted with a unique id."
  ([n attrs]
   (when-let [r (serializable-entity n attrs)]
     (and (meta-entity n) r)))
  ([schema]
   (parse-and-define entity schema)))

(defn- normalize-relation-attribute
  ([attr-spec cardinality]
   (let [new-spec
         (if (keyword? attr-spec)
           {:type attr-spec
            :indexed true}
           (assoc attr-spec :indexed true))]
     (if (:exclusive cardinality)
       (assoc new-spec :unique true)
       new-spec)))
  ([attr-spec]
   (normalize-relation-attribute attr-spec nil)))

(defn- identity-attributes-for-relationship [rec-a rec-b]
  (let [scm-a (cn/ensure-entity-schema rec-a)
        scm-b (when-not (= rec-a rec-b) (cn/ensure-entity-schema rec-b))
        ida (cn/ensure-identity-attribute-name scm-a)
        idb (if scm-b (cn/ensure-identity-attribute-name scm-b) ida)]
    (when (and ida idb)
      [ida idb scm-a (or scm-b scm-a)])))

(defn- generate-relationship-attributes [each-unique cascade-on-delete
                                         is-id rec recattr idattr recscm attr]
  (if is-id
    {attr {:ref (li/make-ref rec recattr)
           :cascade-on-delete cascade-on-delete
           :unique each-unique}}
    {attr {:type (cn/attribute-type recscm recattr)
           :unique each-unique}
     (cn/relationship-member-identity attr)
     {:ref (li/make-ref rec idattr)
      :cascade-on-delete cascade-on-delete}}))

(defn- assoc-relationship-attributes [attrs is-contains [rec-a rec-b :as recs]
                                      on-attrs each-unique cascade-on-delete]
  (when-not (or (li/name? rec-a) (li/name? rec-b))
    (u/throw-ex (str "invalid relationship elements - " recs)))
  (let [[ida idb scma scmb] (identity-attributes-for-relationship rec-a rec-b)]
    (when-not ida
      (u/throw-ex (str "no identity attribute found for " rec-a ", cannot form relationship")))
    (when-not idb
      (u/throw-ex (str "no identity attribute found for " rec-b ", cannot form relationship")))
    (let [[ra rb] (or on-attrs [ida idb])
          [a b :as ab] (cn/relationship-attribute-names rec-a rec-b)
          genattr (partial generate-relationship-attributes each-unique cascade-on-delete)
          a-attrs (genattr
                   (cn/unique-or-identity? scma ra)
                   rec-a ra ida scma a)
          b-attrs (genattr
                   (cn/unique-or-identity? scmb rb)
                   rec-b rb idb scmb b)
          id-attrs (merge a-attrs b-attrs)]
      [(merge attrs id-attrs) ab])))

(defn- parse-relationship-member-spec [spec]
  (let [elems [(first spec) (second spec)]
        meta (seq (rest (rest spec)))]
    (if meta
      [elems (us/wrap-to-map meta)]
      [elems nil])))

(defn- ensure-unique-contains [[_ e2 :as elems]]
  (when elems
    (when-let [r (cn/find-contained-relationship e2)]
      (u/throw-ex (str e2 " is already in a contains relationship - " r)))
    elems))

(defn- parent-query-pattern [attr-accessor relname parent]
  (let [cps (seq (cn/containing-parents parent))
        parent-pat {parent {(li/name-as-query-pattern
                             (cn/identity-attribute-name parent))
                            (attr-accessor (name parent))}}]
    [{relname {}} ; TODO: Instead of {}, `:from` a map attribute in the event?
     (if cps
       (let [[r _ p] (first cps)]
         (assoc parent-pat li/rel-tag (parent-query-pattern attr-accessor r p)))
       parent-pat)]))

(defn- parent-query-path [attr-accessor relname parent child]
  (let [f attr-accessor, np (name parent) nc (name child)
        path (str "/" np "/" (f np) "/" (name relname) "/" nc "/" (f nc))]
    (loop [parent parent, path path]
      (if-let [cps (seq (cn/containing-parents parent))]
        (let [[r _ p] (first cps), np (name p)]
          (recur p (str np "/" (f np) "/" (name r) "/" path)))
        (str "path:" (if (s/starts-with? path "/") "/" "//") path)))))

(defn- parent-names-as-attributes [parent]
  (loop [p parent, result {(keyword (name parent)) :Kernel.Lang/Any}]
    (if-let [cps (seq (cn/containing-parents p))]
      (let [[_ _ p0] (first cps)]
        (recur p0 (assoc result (keyword (name p0)) :Kernel.Lang/Any)))
      result)))

(defn- regen-default-dataflows-for-contains [relname [parent child]]
  (let [ev (partial crud-evname child)
        upevt (ev :Upsert)
        attr-names (cn/attribute-names (cn/fetch-schema child))
        f1 (partial crud-event-inst-accessor upevt true)
        f2 (partial crud-event-attr-accessor upevt)
        ups-inst-pat (into {} (mapv (fn [a] [a (f1 a)]) attr-names))
        lookupevt (ev :Lookup)
        f3 (partial crud-event-attr-accessor lookupevt true)
        c (name child)
        ctx-aname (k/event-context-attribute-name)]
    (event-internal
     upevt
     (merge
      {:Instance child
       li/event-context ctx-aname}
      (parent-names-as-attributes parent)))
    (cn/register-dataflow
     upevt
     [{child ups-inst-pat
       li/rel-tag
       (parent-query-pattern f2 relname parent)}])
    (event-internal
     lookupevt
     (merge
      {(keyword c) :Kernel.Lang/Any
       li/event-context ctx-aname}
      (parent-names-as-attributes parent)))
    (cn/register-dataflow
     lookupevt
     [{(li/name-as-query-pattern child)
       (parent-query-path f3 relname parent child)}])))

(defn- find-between-ref [attrs node-rec-name]
  (let [cn (li/split-path node-rec-name)]
    (us/first-truth
     #(when-let [r (:ref (second %))]
        (let [p (li/path-parts r)]
          (when (= cn [(:component p) (:record p)])
            [(first %) (first (:refs p))])))
     attrs)))

(defn- make-between-upsert-attributes [ups-event-name attrs]
  (if (seq attrs)
    (into
     {}
     (mapv (fn [[k _]]
             [k (li/make-ref ups-event-name [:Instance k])])
           attrs))
    {}))

(defn- regen-default-dataflows-for-between [relname [from to] attrs]
  (let [[aname-from from-qattr] (find-between-ref attrs from)
        attrs (dissoc attrs aname-from)
        [aname-to to-qattr] (find-between-ref attrs to)
        ev (partial crud-evname relname)
        upevt (ev :Upsert)
        ups-attrs (make-between-upsert-attributes upevt (dissoc attrs aname-to))
        ctx-aname (k/event-context-attribute-name)
        f (second (li/split-path from))
        t (second (li/split-path to))
        [fname tname] (if (= from to)
                        [(keyword (str (name f) "1"))
                         (keyword (str (name t) "2"))]
                        [f t])]
    (event-internal
     upevt
     (merge
      {:Instance {:type relname :optional true}
       li/event-context ctx-aname}
      {fname :Kernel.Lang/Any
       tname :Kernel.Lang/Any}))
    (cn/register-dataflow
     upevt
     [{from {(li/name-as-query-pattern from-qattr)
             (li/make-ref upevt fname)}
       li/rel-tag [{relname ups-attrs}
                   {to {(li/name-as-query-pattern to-qattr)
                        (li/make-ref upevt tname)}}]}])))

(defn relationship
  ([relation-name attrs]
   (let [meta (:meta attrs)
         contains (ensure-unique-contains (mt/contains meta))
         between (when-not contains (mt/between meta))
         [elems relmeta] (parse-relationship-member-spec
                          (or contains between))
         each-uq (if (:one-one relmeta) true false)
         combined-uqs (and (not each-uq)
                           (or (and contains (not (:n-n relmeta)))
                               (:one-n relmeta)))
         on-attrs (:on relmeta)
         cascade-on-delete (:cascade-on-delete relmeta)]
     (when-not elems
       (u/throw-ex
        (str "type (contains, between) of relationship is not defined in meta - " relation-name)))
     (let [[attrs uqs] (assoc-relationship-attributes
                        attrs contains elems
                        on-attrs each-uq
                        (if cascade-on-delete true false))
           meta0 (assoc meta cn/relmeta-key relmeta)
           meta (assoc meta0 (if contains mt/contains mt/between) elems)
           r (serializable-entity
              relation-name
              (assoc
               attrs
               :meta (assoc
                      (if combined-uqs (assoc meta :unique uqs) meta)
                      :relationship true)))]
       (when (cn/register-relationship elems relation-name)
         (when-let [r (and (meta-entity relation-name) r)]
           (if contains
             (regen-default-dataflows-for-contains relation-name contains)
             (regen-default-dataflows-for-between relation-name between (dissoc attrs :meta)))
           r)))))
  ([schema]
   (let [r (parse-and-define serializable-entity schema)]
     (and (meta-entity (first (keys schema))) r))))

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

(defn- do-init-kernel []
  (cn/create-component :Kernel.Lang {})
  (doseq [[type-name type-def] k/types]
    (intern-attribute type-name {:check type-def
                                 :type type-name}))

  (attribute (k/event-context-attribute-name)
             (k/event-context-attribute-schema))

  (intern-attribute :Kernel.Lang/Password
                    {:type :Kernel.Lang/String
                     :secure-hash true})

  (record :Kernel.Lang/Future
          {:Result :Kernel.Lang/Any
           :TimeoutMillis {:type :Kernel.Lang/Int
                           :default 2000}})

  (entity {:Kernel.Lang/Resolver
           {:Type :Kernel.Lang/String
            :Configuration :Kernel.Lang/Map
            :Identifier {:check keyword? :unique true}}})

  (entity
   :Kernel.Lang/Role
   {:Name {:type :Kernel.Lang/String
           :unique true}})

  (event
   :Kernel.Lang/RoleAssignment
   {:Role :Kernel.Lang/Role
    :Assignee :Kernel.Lang/Entity})

  (entity
   :Kernel.Lang/Policy
   {:Intercept {:type :Kernel.Lang/Keyword
                :indexed true}
    :Resource {:type :Kernel.Lang/Path
               :indexed true}
    :Spec :Kernel.Lang/Edn
    :InterceptStage
    {:oneof [:PreEval :PostEval :Default]
     :default :Default}})

  (entity {:Kernel.Lang/Timer
           {:Expiry :Kernel.Lang/Int
            :ExpiryUnit {:oneof [:Seconds :Minutes :Hours :Days]
                         :default :Seconds}
            :ExpiryEvent :Kernel.Lang/Map
            ;; :TaskHandle is set by the runtime, represents the
            ;; thread that execute the event after timer expiry.
            :TaskHandle {:type :Kernel.Lang/Any :optional true}}})

  (dataflow
   :Kernel.Lang/LoadPolicies
   {:Kernel.Lang/Policy
    {:Intercept? :Kernel.Lang/LoadPolicies.Intercept
     :Resource? :Kernel.Lang/LoadPolicies.Resource}})

  (event
   :Kernel.Lang/AppInit
   {:Data :Kernel.Lang/Map})

  (event
   :Kernel.Lang/InitConfig
   {})

  (record
   :Kernel.Lang/InitConfigResult
   {:Data {:listof :Kernel.Lang/Map}})

  #?(:clj
     (do
       (record :Kernel.Lang/DataSource
               {:Uri {:type :Kernel.Lang/String
                      :optional true} ;; defaults to currently active store
                :Entity :Kernel.Lang/String ;; name of an entity
                :AttributeMapping {:type :Kernel.Lang/Map
                                   :optional true}})

       (event :Kernel.Lang/DataSync
              {:Source :Kernel.Lang/DataSource
               :DestinationUri {:type :Kernel.Lang/String
                                :optional true}})

       (r/register-resolvers
        [{:name :meta
          :type :meta
          :compose? false
          :config {:fractl-api
                   {:component component
                    :entity entity
                    :event event
                    :record record
                    :dataflow dataflow}}
          :paths [:Kernel.Lang/LoadModelFromMeta]}
         {:name :timer
          :type :timer
          :compose? false
          :paths [:Kernel.Lang/Timer]}
         {:name :data-sync
          :type :data-sync
          :compose? false
          :paths [:Kernel.Lang/DataSync]}]))))

(defn init []
  (when-not (cn/kernel-inited?)
    (do-init-kernel)))
