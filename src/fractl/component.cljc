(ns fractl.component
  "Components of a model."
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [fractl.util :as u]
            [fractl.util.hash :as sh]
            [fractl.util.seq :as su]
            [fractl.util.logger :as log]
            [fractl.lang.internal :as li]))

(def ^:private components
  "Table that maps component names to their definitions."
  #?(:clj  (ref {})
     :cljs (atom {})))

(def ^:private current-component
  "The name of the active component for the current thread."
  #?(:clj
     (proxy [ThreadLocal] []
       (initialValue [] :Kernel))
     :cljs
     (atom nil)))

(def ^:private components-inited
  "All components inited, but init-events not yet fired."
  #?(:clj (ref [])
     :cljs (atom [])))

(defn- set-current-component [n]
  #?(:clj (dosync (.set current-component n)
                  (ref-set components-inited (conj @components-inited n)))
     :cljs (do (reset! current-component n)
               (reset! components-inited (conj @components-inited n))))
  n)

(defn get-current-component []
  #?(:clj (.get current-component)
     :cljs @current-component))

(defn switch-component [n]
  (let [oldn (get-current-component)]
    (set-current-component n)
    oldn))

(defn fetch-components-inited! []
  #?(:clj
     (dosync
      (let [ms @components-inited]
        (dosync (ref-set components-inited []))
        ms))
     :cljs (let [ms @components-inited]
             (reset! components-inited [])
             ms)))

(def full-name li/make-path)

(defn canonical-type-name
  "Return the fully-qualified type-name."
  ([component typname]
   (if (pos? (.indexOf (str typname) "/"))
     typname
     (full-name component typname)))
  ([typname] (canonical-type-name (get-current-component) typname)))

(defn normalize-type-name [^String n]
  (last (li/split-path n)))

(defn component-init-event-name [component]
  (keyword (str (name component) "_Init")))

(declare intern-attribute intern-event)

(defn create-component
  "Create a new component with the given name and references to
  the components in the imports list. If a component already exists with
  the same name, it will be overwritten. Returns the name of the new component."
  [component spec]
  (u/call-and-set
   components
   #(let [imports (when-let [imports (:imports spec)]
                    {:import [imports (li/mappify-alias-imports imports)]
                     ;; Special alias key for imports
                     :alias (li/mappify-alias-imports imports)})
          clj-imports (when-let [clj-imports (:clj-imports spec)]
                        {:clj [(first (rest clj-imports)) (li/mappify-alias-imports clj-imports)]})
          java-imports (when-let [java-imports (:java-imports spec)]
                         {:java (first (rest java-imports))})
          v8-imports (when-let [v8-imports (:v8-imports spec)]
                       {:v8 [(first (rest v8-imports)) (li/mappify-alias-imports v8-imports)]})]
      ;; The interned component has the following structure:
      ;; {component {:resolver <resolver-config-map>
      ;;              :import [imports aliases]
      ;;              :clj [clj-imports aliases]
      ;;              :java java-imports
      ;;              :v8 [v8-imports aliases]}}
      (assoc @components component
             (merge {:resolver (:resolver spec)}
                    imports clj-imports java-imports v8-imports))))
  (intern-attribute [component :Id]
                    {:type :Kernel/UUID
                     :unique true
                     :immutable true
                     :default u/uuid-string})
  (intern-event [component (component-init-event-name component)]
                {:ComponentName :Kernel/Keyword})
  (set-current-component component)
  component)

(defn remove-component [component]
  (u/call-and-set components #(dissoc @components component)))

(defn component-exists? [component]
  (if (find @components component)
    true
    false))

(defn component-definition [component]
  (find @components component))

(defn extract-alias-of-component [component alias-entry]
  (if (component-exists? component)
    (get-in @components [component :alias alias-entry])
    (log/error (str "Component " component " is not present!"))))

(defn- meta-key [path]
  (conj path :-*-meta-*-))

(defn fetch-meta [path]
  (get-in @components (meta-key (li/split-path path))))

(defn- component-intern
  "Add or replace a component entry.
  `typname` must be in the format - :ComponentName/TypName
  Returns the name of the entry. If the component is non-existing, raise an exception."
  ([typname typdef typtag meta]
   (let [[component n :as k] (li/split-path typname)
         intern-k [component typtag n]]
     (when-not (component-exists? component)
       (u/throw-ex-info
        (str "component not found - " component)
        {:name typname
         :tag typtag}))
     (u/call-and-set
      components
      #(assoc-in (if meta
                   (assoc-in @components (meta-key k) meta)
                   @components)
                 intern-k typdef))
     typname))
  ([typname typdef typtag]
   (component-intern typname typdef typtag nil)))

(defn- component-find [path]
  (get-in @components path))

(defn component-resolvers [component]
  (component-find [component :resolvers :component-level]))

(defn entity-resolvers [component entity-name]
  (component-find [component :resolvers entity-name]))

(defn install-resolver
  "Add a resolver for a component or an entity."
  ([component entity-name spec]
   (let [resolvers (or (if (= :component-level entity-name)
                         (component-resolvers component)
                         (entity-resolvers component entity-name))
                       [])]
     (component-intern [component entity-name] (conj resolvers spec) :resolvers)))
  ([component spec]
   (install-resolver component :component-level spec)))

(defn intern-attribute
  "Add or replace an attribute in a component.
  The attribute name must be fully-qualified, as in - `:ComponentName/AttrName`.
  Returns the name of the attribute. If the component is non-existing, raise an exception."
  [attrname attrdef]
  (component-intern attrname attrdef :attributes))

(def ^:private type-tag-key :type-*-tag-*-)

(defn- normalize-recdef [recdef typetag]
  (let [recdef (if (:schema recdef)
                 recdef
                 {:schema recdef})]
    (if (type-tag-key recdef)
      recdef
      (assoc recdef type-tag-key typetag))))

(defn intern-record
  "Add or replace an record in a component.
  The record name must be fully-qualified, as in - `:ComponentName/RecName`
  Returns the name of the record. If the component is non-existing, raise an exception."
  ([typetag recname recdef]
   (let [meta (:meta recdef)
         recdef (dissoc recdef :meta)]
     (component-intern recname (normalize-recdef recdef typetag) :records meta)))
  ([recname recdef]
   (intern-record :record recname recdef)))

(def intern-entity (partial intern-record :entity))
(def intern-event (partial intern-record :event))

(defn find-attribute-schema
  "Find and return an attribute schema by the given path.
  Path should be in one of the following forms:
   - :ComponentName/AttributeName
   - :ComponentName/RecordName.AttributeName
  If the lookup succeeds, return the attribute schema as a map.
  Return `nil` on lookup failure."
  ([component aref]
   (let [[recname attrname] (li/split-ref aref)]
     (if attrname
       (when-let [rec (component-find [component :records recname])]
         (find-attribute-schema (get-in rec [:schema attrname])))
       (component-find [component :attributes aref]))))
  ([path]
   (let [[component aref] (li/split-path path)]
     (find-attribute-schema component aref))))

(defn all-attributes [component]
  (component-find [component :attributes]))

(defn find-record-schema
  "Find and return an record schema by the given path.
   Path should be of the form - :ComponentName/RecordName.
   If the lookup succeeds, return the record schema as a map.
   Return `nil` on lookup failure."
  [path]
  (let [[component recname] (li/split-path path)]
    (component-find [component :records recname])))

(defn- find-record-schema-by-type [typ path]
  (when-let [scm (find-record-schema path)]
    (when (= (type-tag-key scm) typ)
      scm)))

(def find-entity-schema (partial find-record-schema-by-type :entity))
(def find-event-schema (partial find-record-schema-by-type :event))

(defn find-schema [path]
  (u/first-applied [find-attribute-schema :attribute
                    find-entity-schema :entity
                    find-event-schema :event
                    find-record-schema :record]
                   [path]))

(defn make-record-instance [type-tag full-name attributes]
  (into {} (concat {type-tag-key type-tag
                    :name full-name} attributes)))

(def instance->map identity)

(defn instance-type-tag [rec]
  (type-tag-key rec))

(defn instance-name [rec]
  (:name rec))

(defn parsed-instance-name [rec]
  (li/split-path (:name rec)))

(defn record-instance? [rec]
  (= :record (instance-type-tag rec)))

(defn entity-instance? [rec]
  (= :entity (instance-type-tag rec)))

(defn event-instance? [rec]
  (= :event (instance-type-tag rec)))

(defn an-instance? [x]
  (and (map? x) (instance-type-tag x)))

(defn an-entity-instance? [x]
  (and (an-instance? x)
       (entity-instance? x)))

(defn inherits? [base-type child-type]
  (if-let [t (:inherits (fetch-meta child-type))]
    (if (= t base-type)
      true
      (inherits? base-type t))
    false))

(defn instance-of?
  "Return true if the fully-qualified name is the same as that of the instance."
  [nm inst]
  (or (= (li/split-path nm)
         (parsed-instance-name inst))
      (inherits? nm (instance-name inst))))

(defn instance-attributes [x]
  (when (an-instance? x)
    (dissoc x type-tag-key :name :dirty)))

(defn instance-all-attributes [x]
  (when (an-instance? x)
    x))

(defn instance-user-attributes
  "Returns only the user assigned attributes.
   Excludes :Id in its return"
  [inst]
  (when (an-instance? inst)
    (dissoc inst type-tag-key :Id :name :dirty)))

(def set-attribute-value assoc)

(def error? (partial instance-of? :error))
(def async-future? (partial instance-of? :Kernel/Future))

(defn same-record-type?
  "Return true if both instances have the same name and type."
  [inst-a inst-b]
  (and (= (instance-name inst-a)
          (instance-name inst-b))
       (= (instance-type-tag inst-a)
          (instance-type-tag inst-b))))

(defn make-with-attributes
  "Make a copy of the given instance and set the new attributes."
  [inst newattrs]
  (make-record-instance (instance-type-tag inst)
                        (instance-name inst)
                        newattrs))

(defn attribute-names
  "Return names of attributes from schema as a set."
  [schema]
  (set (keys (:schema schema))))

(def attributes :schema)

(defn has-attribute? [schema attr-name]
  (if (attr-name (attribute-names schema))
    true
    false))

(defn- filter-attribute-schemas [predic entity-schema]
  (filter #(let [ascm (find-attribute-schema (second %))]
             (predic ascm))
          entity-schema))

(defn- filter-attributes
  "Filter attribute names based on the attribute schema check using the predicate."
  [predic entity-schema]
  (map first (filter-attribute-schemas predic entity-schema)))

(defn- make-attributes-filter [predic]
  (partial filter-attributes predic))

(def indexed-attributes
  "Return the names of all attributes marked :indexed."
  (make-attributes-filter #(:indexed %)))

(def encrypted-attriutes (make-attributes-filter #(:encryption %)))

(def write-only-attributes (make-attributes-filter #(:write-only %)))

(def ref-attribute-schemas
  "Return the names and schemas of all attributes which has a :ref."
  (partial filter-attribute-schemas #(:ref %)))

(def unique-attributes
  "Return the names of all unique attributes."
  (make-attributes-filter #(:unique %)))

(def identity-attributes
  "Return the names of all identity attributes in the schema."
  (make-attributes-filter #(and (:unique %) (:immutable %))))

(defn- identity-attribute-name
  "Return the name of any one of the identity attributes of the given entity."
  [type-name]
  (let [scm (find-entity-schema type-name)]
    (first (identity-attributes (:schema scm)))))

(defn instance-eq?
  "Return true if both entity instances have the same identity."
  [a b]
  (or (identical? a b)
      (if (every? entity-instance? [a b])
        (let [instname (instance-name a)]
          (and (instance-of? instname b)
               (when-let [idattr (identity-attribute-name instname)]
                 (= (idattr (instance-attributes a))
                    (idattr (instance-attributes b))))))
        (= a b))))

(defn attributes-eq?
  "Return true if both instances have the same attributes."
  [a b]
  (= (instance-attributes a)
     (instance-attributes b)))

(defn same-type? [inst1 inst2]
  (and (= (instance-type-tag inst1)
          (instance-type-tag inst2))
       (= (parsed-instance-name inst1)
          (parsed-instance-name inst2))))

(defn same-instance? [a b]
  (and (instance-eq? a b) (attributes-eq? a b)))

(defn make-error
  "Return an instance of the error record with the given message
   and additional attributes."
  ([msg attributes]
   (make-record-instance :record :error (assoc attributes :message msg)))
  ([msg]
   (make-error msg nil)))

(defn exception->error [ex]
  #?(:clj
     (make-error (or (.getMessage ex) (str ex)) {})
     :cljs
     (make-error ex)))

(defn throw-error
  "Call make-error to create a new instance of error, wrap it in an
  ex-info and raise it as an exception."
  ([msg attributes]
   (u/throw-ex-info (str "component/error: " msg)
                       {:error (make-error msg attributes)}))
  ([msg] (throw-error msg nil)))

(defn- check-attribute-names [schema attributes]
  (let [sks (set (keys schema))
        aks (set (keys attributes))]
    (if-let [ks (seq (set/difference aks sks))]
      (throw-error "invalid attribute(s) found" {:irritant ks})
      true)))

(defn decimal-value? [x]
  #?(:clj
     (or (decimal? x)
         (decimal? (bigdec x)))
     :cljs
     (float? x)))

(defn decimal [x]
  #?(:clj
     (bigdec x)
     :cljs
     (float x)))

(declare apply-attribute-validation)

(defn- element-type-check [tpname [tptag tpscm] x]
  (case tptag
    :attribute (apply-attribute-validation tpname tpscm {tpname x})
    (vec :record :entity) (instance-of? tpname x)
    nil))

(defn- merge-attr-schema [parent-scm child-scm]
  (let [scm (merge parent-scm child-scm)]
    (dissoc scm :type)))

(defn- valid-attrval [v]
  (when-not (nil? v) [true v]))

(defn- get-attr-val [attr-schema attributes attr-name]
  (when-let [[f v :as r] (valid-attrval (get attributes attr-name))]
    (if (:var attr-schema)
      [f (deref v)]
      r)))

(defn- check-format [ascm aname aval]
  (when-let [p (:check ascm)]
    (when-not (p aval)
      (throw-error (str "check failed, invalid value " aval " for " aname))))
  (when-let [fmt (:format ascm)]
    (when-not (fmt aval)
      (throw-error (str "format mismatch - " aname))))
  aval)

(defn- instantiable-map? [x]
  (and (map? x)
       (= 1 (count (keys x)))
       (map? (first (vals x)))))

(defn- instantiable-map-of? [type-name x]
  (or (instance-of? type-name x)
      (and (instantiable-map? x)
           (= type-name (keyword (first (keys x)))))))

(defn- assert-literal-instance [attr-name type-name obj]
  (if (instantiable-map-of? type-name obj)
    obj
    (throw-error (str "expected type for " attr-name " is " type-name))))

(declare valid-attribute-value)

(defn- type-check [attr-name obj ascm]
  (let [tp (:type ascm)]
    (if (find-record-schema tp)
      (assert-literal-instance attr-name tp obj)
      (if-let [attr-scm (find-attribute-schema tp)]
        (valid-attribute-value
         attr-name (check-format ascm attr-name obj)
         (merge-attr-schema attr-scm ascm))
      (throw-error (str "no schema defined for " tp))))))

(defn valid-attribute-value
  "Check against the attribute schema, if the provided value (v)
  is a valid value for the attribute. If valid, return v. If v is nil,
  try to return the default value for the attribute. Otherwise, raise an
  exception."
  [aname aval ascm]
  (if-not (nil? aval)
    (cond
      (:type ascm)
      (type-check aname aval ascm)

      (:listof ascm)
      (let [tp (:listof ascm)
            p (partial element-type-check tp (find-schema tp))]
        (if (every? identity (map p aval))
          aval
          (throw-error (str "invalid list for " aname))))

      (:setof ascm)
      (do (when-not (set? aval)
            (throw-error (str "not a set - " aname)))
          (let [tp (:setof ascm)
                p (partial element-type-check tp (find-schema tp))]
            (if (every? identity (map p aval))
              aval
              (throw-error (str "invalid list for " aname)))))

      :else (check-format ascm aname aval))
    (let [dval (:default ascm)]
      (when-not (nil? dval)
        (if (fn? dval) (dval) dval)))))

(defn- apply-attribute-validation [aname ascm attributes]
  (if (or (:expr ascm) (:query ascm))
    attributes
    (if-let [[_ aval] (get-attr-val ascm attributes aname)]
      (do (valid-attribute-value aname aval ascm)
          attributes)
      (let [dval (valid-attribute-value aname nil ascm)]
        (if-not (nil? dval)
          (assoc attributes aname dval)
          (if (:optional ascm)
            attributes
            (throw-error (str "no default value defined for " aname))))))))

(defn- ensure-attribute-is-instance-of [recname attrname attributes]
  (if-let [aval (get attributes attrname)]
    (if (instance-of? recname aval)
      attributes
      (throw-error (str "attribute " attrname " is not of type " recname)))
    (throw-error (str "no record set for attribute " attrname))))

(defn- preproc-attribute-value [attributes attrname attr-type]
  (if-let [p (case attr-type
               :Kernel/Float float
               :Kernel/Double double
               :Kernel/Decimal decimal
               false)]
    (assoc attributes attrname (p (get attributes attrname)))
    attributes))

(defn- validated-attribute-values [schema attributes]
  (let [r (check-attribute-names schema attributes)]
    (or (error? r)
        (loop [schema schema, attributes attributes]
          (if-let [[aname atype] (first schema)]
            (if-not (li/name? aname)
              (recur (rest schema) attributes)
              (let [typname (li/extract-attribute-name atype)]
                (recur
                 (rest schema)
                 (if-let [ascm (find-attribute-schema typname)]
                   (apply-attribute-validation
                    aname ascm (preproc-attribute-value attributes aname typname))
                   (ensure-attribute-is-instance-of typname aname attributes)))))
            attributes)))))

(defn validate-attribute-value [attr-name attr-val schema]
  (if-let [typname (li/extract-attribute-name (get (:schema schema) attr-name))]
    (if-let [ascm (find-attribute-schema typname)]
      (valid-attribute-value attr-name attr-val ascm)
      (throw-error (str "no schema found for attribute - " attr-name)))
    (throw-error (str "attribute not in schema - " attr-name))))

(def inferred-event-schema {:inferred true})

(defn inferred-event-schema? [schema]
  (if-let [scm (:schema schema)]
    (inferred-event-schema? scm)
    (:inferred schema)))

(defn ensure-schema [recname]
  (if-let [rec (find-record-schema recname)]
    (:schema rec)
    (throw-error (str "schema not found for " recname))))

(defn- validate-record-attributes
  ([recname recattrs schema]
   ;; The :inferred key will be added
   ;; only for inferred events. Do no validate
   ;; the schema of inferred events.
   (if (:inferred schema)
     recattrs
     (validated-attribute-values schema recattrs)))
  ([recname recattrs]
   (validate-record-attributes recname recattrs (ensure-schema recname))))

(defn- type-tag-of [recname]
  (type-tag-key (find-record-schema recname)))

(defn- serialized-instance? [x]
  (and (type-tag-key x) (:name x)))

(defn- deserialize-name [x]
  (let [n (:name x)]
    (cond
      (keyword? n) n
      (string? n) (keyword n)
      (seqable? n) (vec (map #(if (keyword? %) % (keyword %)) n))
      :else (u/throw-ex (str "not a valid name - " n)))))

(defn- deserialize-instance [x]
  (let [tp (keyword (type-tag-key x))
        nm (deserialize-name x)]
    (assoc x type-tag-key tp :name nm)))

(declare make-instance)

(defn- maybe-instance [x validate?]
  (cond
    (and (instantiable-map? x) (not (instance-type-tag x)))
    (if (find-record-schema (first (keys x)))
      (make-instance (first (keys x))
                     (first (vals x))
                     validate?)
      x)

    (map? x)
    (if (serialized-instance? x)
      (deserialize-instance x)
      x)

    (string? x) x

    (set? x)
    (set (map #(maybe-instance % validate?) x))

    (vector? x)
    (vec (map #(maybe-instance % validate?) x))

    :else x))

(defn- maps-to-insts
  "If any of the values in the attributes map itself is the
   map-encoded representation of a record or entity, convert
   that map into a proper instance."
  [attrs validate?]
  (into {} (map (fn [[k v]]
                  [k (maybe-instance v validate?)])
                attrs)))

(defn- post-process-attributes
  "Apply any additional processing to attribute values, like encryption"
  [recname attrs schema]
  (loop [encrypted (seq (encrypted-attriutes schema)), result attrs]
    (if-let [k (first encrypted)]
      (recur (rest encrypted) (assoc result k (sh/salted-hash (k result))))
      result)))

(defn make-instance
  "Initialize an instance of a record from the given map of attributes.
   All attribute values will be validated using the associated value predicates.
   full-record-name must be in the form - :ComponentName/RecordName.
   Return the new record on success, return an :error record on failure."
  ([record-name attributes validate?]
   (let [attrs-with-insts (maps-to-insts attributes validate?)
         schema (ensure-schema record-name)
         validated-attrs (if validate?
                           (validate-record-attributes record-name attrs-with-insts schema)
                           attrs-with-insts)
         attrs (post-process-attributes record-name validated-attrs schema)]
     (if (error? attrs)
       attrs
       (make-record-instance (type-tag-of record-name) record-name attrs))))
  ([record-name attributes]
   (make-instance record-name attributes true))
  ([m]
   (make-instance (first (keys m)) (first (vals m)))))

(defn- make-X-instance
  "Make a new instance of the record, entity or event with the name `xname`.
   The attributes for the new instance are encoded as a plain map."
  [x? xname attributes]
  (let [inst (make-instance xname attributes true)]
    (and (x? inst) inst)))

(def ^:private entity-event-sep "_")

(defn make-entity-event-name [entity-name event-type trig-type]
  (keyword (str (subs (str entity-name) 1) entity-event-sep
                (name event-type) entity-event-sep
                (name trig-type))))

(defn for-each-entity-event-name
  "For the given entity, generate the corresponding entity-event names.
   Call `f` with each generated name, its event tag and trigger tag as arguments.
   The generated event name will have the format entityName_eventTag_triggerTag.
   The event tag will stand for an upsert or a delete. The trigger tag denotes when the
   event will be fired - i.e either before or after the upsert/delete."
  [entity-name f]
  (let [p (partial make-entity-event-name entity-name)]
    (doseq [et [:OnUpsert :OnDelete]]
      (doseq [tt [:Before :After]]
        (let [event-name (p et tt)]
          (f event-name et tt))))))

(def ^:private entity-event-name-suffixes
  [(str entity-event-sep "After")
   (str entity-event-sep "Before")])

(defn entity-event? [x]
  (let [n (if (keyword? x)
            (str x)
            (str (instance-name x)))]
    (some (partial s/ends-with? n)
          entity-event-name-suffixes)))

(def make-event-instance (partial make-X-instance event-instance?))
(def make-entity-instance (partial make-X-instance entity-instance?))

(defn kernel-crud-event [event-type trig-type inst oldinst]
  (let [evtname (make-entity-event-name
                 (instance-name inst)
                 event-type trig-type)]
    (make-event-instance
     evtname
     (if (= event-type :OnDelete)
       {:Instance inst}
       {:Instance inst
        :OldInstance oldinst}))))

(def kernel-create-event (partial kernel-crud-event :OnUpsert :After))
(def kernel-update-event kernel-create-event)
(def kernel-delete-event (partial kernel-crud-event :OnDelete :After))

(def kernel-create-pending-event (partial kernel-crud-event :OnUpsert :Before))
(def kernel-update-pending-event kernel-create-pending-event)
(def kernel-delete-pending-event (partial kernel-crud-event :OnDelete :Before))

(defn merge-attributes
  "Both and `a` and `b` must be instances of the same record, entity or event.
   A new instance of this type is returned with the attributes of `b` merged into `a`."
  [a b]
  (let [newattrs (validate-record-attributes
                  (instance-name a)
                  (merge (instance-attributes a) (instance-attributes b)))]
    (make-with-attributes a newattrs)))

(defn- event-name [e]
  (cond
    (keyword? e) e
    (event-instance? e) (instance-name e)
    (map? e) (let [n (first (keys e))]
               (if (keyword? n)
                 n
                 (u/throw-ex (str "not a valid event name - " n))))
    :else (u/throw-ex (str "invalid event pattern - " e))))

(defn register-dataflow
  "Attach a dataflow to the event."
  ([event head patterns component]
   (u/call-and-set
    components
    #(let [ms @components
           ename (normalize-type-name (event-name event))
           path [component :events ename]
           currpats (get-in ms path [])
           newpats (conj currpats [event {:head head
                                          :event-pattern event
                                         :patterns patterns
                                          :opcode (u/make-cell nil)}])]
       (assoc-in ms path newpats)))
   event)
  ([event head patterns]
   (let [[component _] (li/split-path (event-name event))]
     (register-dataflow event head patterns component)))
  ([event patterns] (register-dataflow event nil patterns)))

(defn- register-entity-event-df [head patterns event-name _ _]
  (let [instpat (keyword (str (subs (str event-name) 1) ".Instance"))
        pats `[~instpat ~@patterns]]
    (register-dataflow event-name head pats)))

(defn register-entity-dataflow
  "Register an :EntityEvent dataflow with a condition, based on an :on-entity-event
  spec associated with an entity."
  [head-pat head patterns]
  (let [nm (cond
             (map? head-pat) (first (keys head-pat))
             (li/name? head-pat) head-pat
             :else (throw-error (str "invalid head pattern - " head-pat)))]
    (if (find-entity-schema nm)
      (for-each-entity-event-name
       nm (partial register-entity-event-df head patterns))
      (throw-error (str "cannot regsiter dataflow, schema not found for " nm)))))

(declare event-cond-expr->fncall-expr normalize-event-cond-predic)

(defn- event-attrval
  "Translate an entry in a event condition based on the following rules:
     - an attribute name: replace with the corresponding attribute value from the event instance.
     - a vector: must be a nested condition expression, translate recursively.
     - a list: must be a predicate call, apply translation rules for predicates.
     - any other value, return as is."
  [event-inst k]
  (cond
    (li/name? k)
    (get-in event-inst (li/split-by-delim #"\." (name k)))

    (vector? k)
    (event-cond-expr->fncall-expr event-inst k)

    (list? k)
    (normalize-event-cond-predic event-inst k)

    :else k))

(defn- normalize-event-cond-predic
  "Translate event attribute references in a predicate call to corresponding
  attribute values from the event instance."
  [event-inst predic-call-expr]
  (let [args (map (partial event-attrval event-inst) (rest predic-call-expr))]
    `(~(first predic-call-expr) ~@args)))

(defn- event-cond-expr->fncall-expr
  "Translate an event condition expression to a predicate function call.
  An example event condition is [:= :X 100], when :X is an attribute of the
  incoming event."
  [event-inst cond-expr]
  (let [fpos-expr
        (let [f (first cond-expr)]
          (if (some #{f} #{:= :< :> :<= :>= :and :or})
            (symbol (name f))
            (throw-error (str "invalid condition in event pattern - " cond-expr))))]
    `(~fpos-expr ~@(map (partial event-attrval event-inst) (rest cond-expr)))))

(defn- satisfies-event-condition?
  "Return true if the event instance satisfies the condition expression."
  [event-inst cond-expr]
  (eval (event-cond-expr->fncall-expr event-inst cond-expr)))

(defn- satisfies-event-predicate?
  "Return true if the event instance satisfies the predicate."
  [event-inst fn-call-expr]
  (eval (normalize-event-cond-predic event-inst fn-call-expr)))

(defn- cond-pat->map [p]
  (try
    (and (seqable? p) (into {} (first p)))
    (catch #?(:clj Exception :cljs :default) e
      nil)))

(def dataflow-event-pattern first)
(def ^:private dataflow-spec second)

(defn dataflow-patterns [df]
  (:patterns (dataflow-spec df)))

(defn dataflow-opcode [df]
  @(:opcode (dataflow-spec df)))

(defn set-dataflow-opcode! [df opc]
  (u/safe-set
   (:opcode (dataflow-spec df))
   opc))

(defn dataflow-on-entity [df]
  (get-in (dataflow-spec df) [:head :on-entity-event]))

(defn dataflow-with-condition [df]
  (get-in (dataflow-spec df) [:head :when]))

(defn dataflow-match-entity-name [df]
  (and (entity-event? (dataflow-event-pattern df))
       (when-let [e (dataflow-on-entity df)]
         (if (li/name? e)
           e
           (first (keys e))))))

(defn- satisfies-conditions?
  "Return true if conditions/predicates in the event pattern attached to the dataflow
  all return true"
  [event-inst df]
  (if-let [condition (dataflow-with-condition df)]
    (let [inst (if (dataflow-match-entity-name df)
                 (:Instance event-inst)
                 event-inst)]
      (cond
        (vector? condition) (satisfies-event-condition? inst condition)
        (list? condition) (satisfies-event-predicate? inst condition)
        :else true))
    true))

(defn- filter-by-conditional-events
  "Return only those dataflows whose event conditions and predicates are all true."
  [event-inst dfs]
  (filter (partial satisfies-conditions? event-inst) dfs))

(defn dataflows-for-event
  "Return all dataflows attached to the event."
  [event]
  (let [[component ename] (li/split-path (event-name event))
        path [component :events ename]]
    (filter-by-conditional-events event (component-find path))))

(defn evalable-dataflow [[k dfspec :as df]]
  [k (dataflow-patterns df)])

;; Component querying, useful for the edges.

(defn record-names-by-type
  "Return a list of record-names, of the given type, interned in this component.
  The type argument `tp` could be one of - :record, :event or :entity."
  [tp component]
  (let [evts (filter (fn [[_ v]] (= tp (type-tag-key v)))
                     (:records (get @components component)))]
    (set (map (partial full-name component) (keys evts)))))

(def record-names (partial record-names-by-type :record))
(def entity-names (partial record-names-by-type :entity))
(def event-names (partial record-names-by-type :event))

(defn get-schema [getter recname]
  (:schema (getter recname)))

(def event-schema (partial get-schema find-event-schema))
(def record-schema (partial get-schema find-record-schema))
(def entity-schema (partial get-schema find-entity-schema))

(defn fetch-schema [some-type]
  (:schema (second (find-schema some-type))))

(defn computed-attribute-fns
  "Return the expression or query functions attached to computed attributes
  as a mapping of [[attrname fn], ...]"
  [prop schema]
  (let [schema (dissoc (or (:schema schema) schema) :meta)
        exps (map (fn [[k v]]
                    (when-let [f (prop (find-attribute-schema v))]
                      [k f]))
                  schema)]
    (seq (su/nonils exps))))

(def expr-fns (partial computed-attribute-fns :expr))
(def query-fns (partial computed-attribute-fns :query))

(defn all-computed-attribute-fns [entity-name]
  (when-let [scm (find-entity-schema entity-name)]
    [(expr-fns scm) (query-fns scm)]))

(defn mark-dirty [inst]
  (assoc inst :dirty true))

(defn dirty? [x]
  (:dirty x))

(defn unmark-dirty [inst]
  (dissoc inst :dirty))

(defn filter-dirty [insts-map]
  (let [res (map (fn [[nm insts]]
                   [nm (filter #(:dirty %) insts)])
                 insts-map)]
    (into {} res)))

(defn- computed? [attr-schema]
  (or (:expr attr-schema) (:query attr-schema)))

(defn- pickled [attr-schema attr-val]
  (if-let [p (:writer attr-schema)]
    (p attr-val)
    attr-val))

(defn serializable-attributes [inst]
  (let [attrs (instance-attributes inst)
        schema (entity-schema (:name inst))
        new-attrs (map (fn [[k v]]
                         (let [ascm (find-attribute-schema v)]
                           (when-not (computed? ascm)
                             [k (pickled ascm (get attrs k))])))
                       schema)]
    (into {} (su/nonils new-attrs))))

(defn kernel-resolver-name? [n]
  (= :Kernel/Resolver n))

(defn as-df-result
  "Return the result of a dataflow evaluation as an instance
  of :DataflowResult. `pat` is the event pattern attached to the
  dataflow. `evt` is the instance of the event that triggered the evaluation.
  `result` is the value of evaluation."
  [result pat evt]
  (make-instance
   :Kernel/DataflowResult
   {:Pattern pat
    :Event evt
    :Result result} false))

(defn tag? [k]
  (or (= k :name)
      (= k type-tag-key)))

(defn attribute-unique-reference-path [[attr-name attr-spec]]
  (when-let [r (:ref attr-spec)]
    (when (:unique (find-attribute-schema r))
      [attr-name r])))

(defn all-reference-paths [attrs]
  (seq (filter attribute-unique-reference-path attrs)))

(defn unique-attribute? [entity-schema attr]
  (:unique (find-attribute-schema (get entity-schema attr))))

(defn attribute-type [entity-schema attr]
  (let [ascm (get entity-schema attr)]
    (or (:type (find-attribute-schema ascm))
        ascm)))

(defn type-any? [entity-schema attr]
  (= :Kernel/Any (attribute-type entity-schema attr)))

(defn find-ref-path [attr-schema-name]
  (:ref (find-attribute-schema attr-schema-name)))

(defn dissoc-write-only [instance]
  (let [schema (ensure-schema (instance-name instance))]
    (if-let [wo-attrs (seq (write-only-attributes schema))]
      (into {} (filter (fn [[k _]]
                         (not (some #{k} wo-attrs)))
                       instance))
      instance)))

(defn make-future [future-obj timeout-ms]
  (make-instance :Kernel/Future {:Result future-obj
                                 :TimeoutMillis timeout-ms}))

(def future-object? (partial instance-of? :Kernel/Future))

(defn deref-future-object [obj]
  #?(:clj
     (deref (:Result obj) (:TimeoutMillis obj) nil)
     :cljs
     ;; Concurrency not yet implemented in cljs.
     (:Result obj)))

(defn maybe-deref [obj]
  (if (future-object? obj)
    (or (deref-future-object obj)
        (make-error "Async timeout" obj))
    obj))

(defn validate-instance [inst]
  (let [n (instance-name inst)
        schema (ensure-schema n)
        attrs (validate-record-attributes
               n (instance-attributes inst) schema)]
    (if (error? attrs)
      (u/throw-ex attrs)
      (make-record-instance (type-tag-key inst) n attrs))))

(defn tag-record [recname attrs]
  (assoc attrs :name recname type-tag-key :record))

(def ^:private trigger-store
  #?(:clj  (ref {})
     :cljs (atom {})))

(defn install-triggers!
  "Install the predicate for the given records.
  On upsert, the event is triggered if the predicate
  return true for the record instance"
  [record-names event-name predicate where-clause records-to-load]
  (doseq [rn (if (keyword? record-names)
               [record-names]
               record-names)]
    (let [rn (li/split-path rn)
          ts @trigger-store]
      (u/safe-set
       trigger-store
       (let [trigs (get ts rn)
             rs (set (map li/split-path records-to-load))]
           (assoc
            ts rn
            (conj
             trigs
             [predicate event-name [where-clause rs]]))))))
  (u/call-and-set
   trigger-store
   #(assoc
     @trigger-store
     (li/split-path event-name) :conditional-event)))

(defn conditional-event? [n]
  (= :conditional-event (get @trigger-store (li/split-path n))))

(defn conditional-events
  "Return conditional events to fire for the given instance"
  [instance]
  (let [recname (li/split-path (instance-name instance))]
    (seq (get @trigger-store recname))))

(defn fire-event? [event-info instances]
  (let [args (map (fn [inst] [(li/split-path (instance-name inst)) inst]) instances)]
    (when ((first event-info) (into {} args))
      true)))

(defn- replace-referenced-value [loaded-instances [[c n] r :as term]]
  (if-let [inst (first (filter #(= (li/split-path (instance-name %)) [c n]) loaded-instances))]
    (if-let [v (get inst r)]
      v
      (u/throw-ex (str "failed to load reference - " term)))
    term))

(defn- rewrite-term [loaded-instances term]
  (if (li/parsed-path? term)
    (replace-referenced-value loaded-instances term)
    term))

(defn- extract-query-target [rewritten-clause]
  (loop [rcs rewritten-clause, target nil]
    (if-let [r (first rcs)]
      (if (li/parsed-path? r)
        (if target
          (u/throw-ex
           (str "cannot have two targets in the same clause - "
                rewritten-clause))
          (recur (rest rcs) (first r)))
        (recur (rest rcs) target))
      target)))

(defn- normalize-rewritten [rewritten-clause]
  (loop [rcs rewritten-clause, literals [], result []]
    (if-let [r (first rcs)]
      (if (li/parsed-path? r)
        (recur (rest rcs) literals (conj result (second r)))
        (recur (rest rcs) (conj literals r) result))
      (concat result literals))))

(defn parse-where-clause [clause loaded-instances]
  (let [opr (first clause)
        rewritten (map (partial rewrite-term loaded-instances) (rest clause))]
    {:from (extract-query-target rewritten)
     :where (concat [opr] (normalize-rewritten rewritten))}))
