(ns fractl.lang
  "The core constructs of the modeling language."
  (:require [clojure.set :as set]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.lang.kernel :as k]
            [fractl.component :as cn]
            [fractl.compiler :as c]))

(defn- normalize-imports [imports]
  (let [imps (rest imports)]
    (if (keyword? (first imps))
      [imps]
      imps)))

(defn- component-spec-for [k spec]
  (seq (filter #(= k (first %)) spec)))

(declare init)

(defn component
  "Create and activate a new component with the given name."
  [n & spec]
  (init)
  (let [ns-name (li/validate-name n)
        imports (component-spec-for :import spec)
        clj-imports (component-spec-for :clj spec)
        java-imports (component-spec-for :java-imports spec)
        v8-imports (component-spec-for :v8-imports spec)
        resolver (component-spec-for :resolver spec)]
    (cn/create-component
     ns-name
     {:imports (normalize-imports
                (li/validate-imports (first imports)))
      :clj-imports (li/validate-clj-imports (first clj-imports))
      :java-imports (li/validate-java-imports (first java-imports))
      :v8-imports (li/validate-clj-imports (first v8-imports))
      :resolver resolver})))

(defn- attribute-type? [nm]
  (or (cn/find-attribute-schema nm)
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
      (assoc (dissoc (assoc-oneof-default attr-schema) :oneof)
             :check (fn [x] (some #{x} values))))
    attr-schema))

(defn- reference-exists? [path]
  (if-let [scm (cn/find-attribute-schema path)]
    true
    false))

(defn- query-pattern? [a-map]
  (let [ks (keys a-map)
        k (first ks)]
    (and (= 1 (count ks))
         (li/name? k)
         (map? (get a-map k)))))

(defn- fn-or-name? [x]
  (or (fn? x) (li/name? x)))

(defn- encryption? [x]
  ;; true/:default means use the default encryption algorithm.
  ;; In future, specific algorithms may be supported
  ;; as an enum of keywords, e.g - :bcrypt, :sha512 etc.
  (or (= true x)
      (= :default x)))

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
      :expr (li/validate fn-or-name? ":expr has invalid value" v)
      :query (li/validate fn? ":query must be a compiled pattern" v)
      :format (li/validate string? ":format must be a textual pattern" v)
      :listof (li/validate attribute-type? ":listof has invalid type" v)
      :setof (li/validate attribute-type? ":setof has invalid type" v)
      :indexed (li/validate-bool :indexed v)
      :write-only (li/validate-bool :write-only v)
      :encryption (li/validate encryption? "invalid encryption type" v)
      :type-in-store (li/validate string? ":type-in-store must be specified as a string" v)
      :ref (li/validate reference-exists? ":ref is invalid" v)
      :var (li/validate-bool :var v)
      :writer (li/validate fn? ":writer must be a function" v)
      (u/throw-ex (str "invalid constraint in attribute definition - " k))))
  (merge {:unique false :immutable false} scm))

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

(defn- validate-attribute-schema-map-keys [scm]
  (let [newscm (maybe-assoc-ref-type
                (finalize-raw-attribute-schema (oneof-as-check scm)))]
    (if (:unique newscm)
      (assoc newscm :indexed true)
      newscm)))

(defn- validate-attribute-schema [scm]
  (if (fn? scm)
    scm
    (validate-attribute-schema-map-keys
     (li/validate map? "attribute specification should be a map" scm))))

(defn attribute
  "Add a new attribute definition to the component."
  [n scm]
  (cn/intern-attribute
   (li/validate-name n)
   (normalize-attribute-schema
    (validate-attribute-schema scm))))

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
  )

(defn- attref? [n]
  (let [[[_ _] a] (li/ref-as-names n)]
    (if a true false)))

(defn- normalize-attr [recname attrs fqn [k v]]
  (let [newv (cond
               (map? v)
               (let [nm (fqn (li/unq-name))]
                 (if (query-pattern? v)
                   (attribute nm {:query (query-eval-fn recname attrs k v)})
                   (if-let [expr (:expr v)]
                     (attribute nm {:expr (c/compile-attribute-expression
                                           recname attrs k expr)})
                     (attribute nm v))))
               (list? v)
               (attribute (fqn (li/unq-name))
                          {:expr (c/compile-attribute-expression
                                  recname attrs k v)})
               :else
               (let [fulln (fqn v)]
                 (if (attref? fulln)
                   (attribute (fqn (li/unq-name))
                              {:ref fulln})
                   fulln)))]
    [k newv]))

(defn- validated-canonical-type-name [n]
  (li/validate-name (cn/canonical-type-name n)))

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

(defn- normalized-attributes [recname orig-attrs]
  (let [f (partial cn/canonical-type-name (cn/get-current-component))
        meta (:meta orig-attrs)
        inherits (:inherits orig-attrs)
        [_ scm] (cn/find-schema inherits)
        inherited-scm (when scm (:schema scm))
        base-attrs (dissoc orig-attrs :meta :inherits)
        attrs (if inherited-scm
                (merge inherited-scm base-attrs)
                base-attrs)
        req-attrs (or (:required-attributes meta)
                      (required-attribute-names attrs))
        attrs-with-defaults (into {} (map (partial assoc-defaults req-attrs) attrs))
        newattrs (map (partial normalize-attr recname attrs f) attrs-with-defaults)
        final-attrs (into {} (validate-attributes newattrs))]
    (assoc final-attrs :meta (assoc meta :required-attributes req-attrs))))

(defn- parse-and-define [f schema]
  (let [n (first (keys schema))]
    (f n (get schema n))))

(defn record
  "Add a new record definition to the component."
  ([n attrs]
   (let [cn (validated-canonical-type-name n)]
     (cn/intern-record
      cn (normalized-attributes cn attrs))))
  ([schema]
   (parse-and-define record schema)))

(defn- event-internal
  ([n attrs verify-name?]
   (let [cn (if verify-name?
              (validated-canonical-type-name n)
              (cn/canonical-type-name n))]
     (cn/intern-event cn (if (cn/inferred-event-schema? attrs)
                              attrs
                              (normalized-attributes cn attrs)))))
  ([n attrs]
   (event-internal n attrs false)))

(defn event
  "An event record with timestamp and other auto-generated meta fields."
  ([n attrs]
   (event-internal n attrs true))
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
    :else (u/throw-ex (str "invalid dataflow pattern - " x))))

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

(defn dataflow
  "A declarative data transformation pipeline."
  [match-pat & patterns]
  (ensure-dataflow-patterns! patterns)
  (let [hd (:head match-pat)]
    (if-let [mt (and hd (:on-entity-event hd))]
      (cn/register-entity-dataflow mt hd patterns)
      (let [event (normalize-event-pattern (if hd (:on-event hd) match-pat))]
        (do (ensure-event! event)
            (cn/register-dataflow event hd patterns))))))

(defn- crud-evname [entity-name evtname]
  (cn/canonical-type-name
   (keyword (str (name evtname) "_" (name entity-name)))))

(defn- crud-event-inst-accessor [evtname]
  (cn/canonical-type-name
   (keyword (str (name evtname) ".Instance"))))

(defn- id-accessor [evtname]
  (cn/canonical-type-name
   (keyword (str (name evtname) ".Instance.Id"))))

(defn- direct-id-accessor [evtname]
  (cn/canonical-type-name
   (keyword (str (name evtname) ".Id"))))

(defn- crud-event-delete-pattern [evtname entity-name]
  [:delete entity-name (direct-id-accessor evtname)])

(defn- crud-event-lookup-pattern [evtname entity-name]
  {entity-name {:Id? (direct-id-accessor evtname)}})

(defn- implicit-entity-event-dfexp
  "Construct a dataflow expressions for an implicit dataflow
  lifted from the :on-entity-event property of an entity
  definition."
  [ename event-spec]
  `(dataflow {:head {:on-entity-event {~ename {:Id 'aaaaid}}
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

(defn entity
  "A record that can be persisted with a unique id."
  ([n attrs]
   (let [entity-name (validated-canonical-type-name n)
         [attrs dfexps] (lift-implicit-entity-events entity-name attrs)
         result (cn/intern-entity
                 entity-name
                 (normalized-attributes
                  entity-name
                  ;; TODO: Check for user-define identity attributes first.
                  (assoc attrs :Id (cn/canonical-type-name :Id))))
         ev (partial crud-evname n)
         inst-evattrs {:Instance n}
         id-evattrs {:Id :Kernel/UUID}]
     ;; Define CRUD events and dataflows:
     (let [upevt (ev :Upsert)
           delevt (ev :Delete)
           lookupevt (ev :Lookup)]
       (cn/for-each-entity-event-name
        entity-name
        (partial entity-event entity-name))
       (event-internal upevt inst-evattrs)
       (cn/register-dataflow upevt [(crud-event-inst-accessor upevt)])
       (event-internal delevt id-evattrs)
       (cn/register-dataflow delevt [(crud-event-delete-pattern delevt entity-name)])
       (event-internal lookupevt id-evattrs)
       (cn/register-dataflow lookupevt [(crud-event-lookup-pattern lookupevt entity-name)]))
     ;; Install dataflows for implicit events.
     (when dfexps (doall (map eval dfexps)))
     result))
  ([schema] (parse-and-define entity schema)))

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

(def ^:private kernel-inited
  #?(:clj
     (ref false)
     :cljs
     (atom false)))

(defn- do-init-kernel []
  (cn/create-component :Kernel {})
  (doseq [[type-name type-def] k/types]
    (attribute type-name type-def))

  (record :Kernel/DataflowResult
          {:Pattern :Kernel/Any
           :Event :Kernel/Event
           :Result :Kernel/Any})

  (record :Kernel/Future
          {:Result :Kernel/Any
           :TimeoutMillis {:type :Kernel/Int
                           :default 2000}})

  (entity {:Kernel/Resolver
           {:Type {:check keyword?}
            :Configuration :Kernel/Map
            :Identifier {:check keyword? :unique true}}})

  (event :Kernel/AppInit
         {:AppName :Kernel/Keyword}))

(defn- initf []
  (when-not @kernel-inited
    (do-init-kernel)
    true))

(defn init []
  (u/safe-set-truth kernel-inited initf))
