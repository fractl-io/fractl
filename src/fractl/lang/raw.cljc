(ns fractl.lang.raw
  "Raw (edn) representation of all interned component-elements"
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]
            #?(:clj [fractl.lang.pub-schema :as pubs])))

(def ^:private raw-store (u/make-cell {}))

(defn- maybe-publish-add-definition [tag record-name attrs]
  #?(:clj
     (when (pubs/publish-schema?)
       (pubs/publish-event {:operation :add :tag tag :type record-name :schema attrs}))))

(defn- maybe-publish-remove-definition [tag record-name]
  #?(:clj
     (when (pubs/publish-schema?)
       (pubs/publish-event {:operation :delete :tag tag :type record-name}))))

(defn- process-component-spec [spec]
  (if-let [clj-imps (:clj-import spec)]
    (if-not (= 'quote (first clj-imps))
      (assoc spec :clj-import `(~'quote ~clj-imps))
      spec)
    spec))

(defn component [component-name spec]
  (let [cdef (get @raw-store component-name '())
        cspec (concat `(~'component ~component-name) (when spec [(process-component-spec spec)]))
        new-cdef (conj (rest cdef) cspec)]
    (u/safe-set raw-store (assoc @raw-store component-name new-cdef))
    (maybe-publish-add-definition 'component component-name spec)
    component-name))

(defn intern-component [component-name defs]
  (u/safe-set raw-store (assoc @raw-store component-name (seq defs)))
  component-name)

(defn update-component-defs [component-name f]
  (u/call-and-set
   raw-store
   #(let [rs @raw-store]
      (if-let [cdef (get rs component-name)]
        (assoc rs component-name (f cdef))
        rs)))
  component-name)

(defn append-to-component [component-name definition]
  (update-component-defs component-name #(concat % [definition])))

(defn remove-from-component [component-name predic]
  (update-component-defs component-name #(filter (complement predic) %)))

(defn find-in-component [component-name predic]
  (when-let [cdef (get @raw-store component-name)]
    (filter predic cdef)))

(defn create-function [component-name function-name params-vector body]
  (when-not (symbol? function-name)
    (u/throw-ex (str "invalid function name: " function-name)))
  (when-not (vector? params-vector)
    (u/throw-ex (str "not a vector: " params-vector)))
  (append-to-component component-name `(~'defn ~function-name ~params-vector ~body)))

(defn- clj-def? [tag n x]
  (and (seqable? x) (= (first x) tag) (= (second x) n)))

(defn delete-function [component-name function-name]
  (remove-from-component component-name (partial clj-def? 'defn function-name)))

(defn- find-defn [component-name function-name]
  (first (find-in-component component-name (partial clj-def? 'defn function-name))))

(defn get-function-params [component-name function-name]
  (nth (find-defn component-name function-name) 2))

(defn get-function-body [component-name function-name]
  (nth (find-defn component-name function-name) 3))

(defn update-component-spec! [component-name spec-key spec]
  (when-let [cdef (get @raw-store component-name)]
    (let [cn (first cdef)
          [_ _ cspec] cn
          new-cspec (process-component-spec (assoc cspec spec-key spec))
          new-cdef (conj (rest cdef) `(~'component ~component-name ~new-cspec))]
      (u/safe-set raw-store (assoc @raw-store component-name new-cdef))
      component-name)))

(defn- infer-component-name [defs]
  (when (seqable? defs)
    (when-not (or (map? defs) (string? defs))
      (when-let [d (first (filter #(= 'component (first %)) defs))]
        (second d)))))

(defn maybe-intern-component [defs]
  (when-let [component-name (infer-component-name defs)]
    (intern-component component-name defs)))

(defn prepost-header? [n]
  (some #{(first n)} #{:after :before}))

(defn- extract-def-name [obj]
  (cond
    (keyword? obj) obj
    (vector? obj) (if (prepost-header? obj)
                    obj
                    (first obj))
    :else (li/record-name obj)))

(defn- def? [tag full-name rec-name obj]
  (and (= (first obj) (symbol tag))
       (let [n (extract-def-name (second obj))]
         (or (= n full-name) (= n rec-name)))))

(defn- extract-record-name [n]
  (cond
    (keyword? n) n
    (vector? n) (if (prepost-header? n)
                  (nth n 2)
                  (first n))
    :else (u/throw-ex (str "cannot extract record name from " n))))

(defn- get-all-defs [record-name]
  (let [s @raw-store
        recname (extract-record-name record-name)
        [c n] (li/split-path recname)]
    (when-let [defs (get s c)]
      [defs c n])))

(defn- remove-def [tag full-recname]
  (when-let [[defs c rec-name] (get-all-defs full-recname)]
    [c (filter (complement (partial def? tag full-recname rec-name)) defs)]))

(defn- find-def [tag full-recname]
  (when-let [[defs _ rec-name] (get-all-defs full-recname)]
    (first (filter (partial def? tag full-recname rec-name) defs))))

(defn- quote-fn-calls [spec]
  (cond
    (map? spec)
    (into {} (mapv (fn [[k v]]
                     [k (cond
                          (and (list? v) (not= 'quote (first v)))
                          `(quote ~v)

                          (map? v) (quote-fn-calls v)

                          :else v)])
                   spec))
    (and (vector? spec) (= :eval (first spec)) (not= 'quote (first (second spec))))
    (assoc spec 1 `(quote ~(second spec)))

    :else spec))

(defn- replace-def [tag full-recname new-spec]
  (when-let [[defs c rec-name] (get-all-defs full-recname)]
    (let [tag (symbol tag)
          p? (partial def? tag full-recname rec-name)
          ndef (if (= 'dataflow tag)
                 `(~tag ~full-recname ~@(map quote-fn-calls new-spec))
                 `(~tag ~full-recname ~(quote-fn-calls new-spec)))]
      (loop [defs defs, found false, new-defs []]
        (if-let [d (first defs)]
          (if (p? d)
            (recur (rest defs) true (conj new-defs ndef))
            (recur (rest defs) found (conj new-defs d)))
          [c (seq (if found new-defs (conj new-defs ndef)))])))))

(defn- find-defspec [tag full-recname]
  (when-let [d (find-def tag full-recname)]
    (let [s (second d)]
      (if (keyword? s)
        (nth d 2)
        (li/record-attributes d)))))

(def find-entity (partial find-defspec 'entity))
(def find-relationship (partial find-defspec 'relationship))
(def find-record (partial find-defspec 'record))
(def find-event (partial find-defspec 'event))
(def find-rule (partial find-defspec 'rule))
(def find-inference (partial find-defspec 'inference))
(def find-resolver (partial find-defspec 'resolver))

(defn find-attribute [n]
  (when-not (li/internal-attribute-name? n)
    (find-defspec 'attribute n)))

(defn- change-defs [f]
  (when-let [[c defs] (f)]
    (u/safe-set raw-store (assoc @raw-store c defs))
    defs))

(defn add-definition [tag record-name attrs]
  (when (change-defs #(replace-def tag record-name attrs))
    (maybe-publish-add-definition tag record-name attrs)
    record-name))

(defn remove-definition [tag record-name]
  (case (symbol tag)
    event (remove-definition 'dataflow record-name)
    entity (doseq [evt (li/prepost-event-heads record-name)]
             (remove-definition 'dataflow evt))
    true)
  (when (change-defs #(remove-def tag record-name))
    (maybe-publish-remove-definition tag record-name)
    record-name))

(defn attribute [n spec]
  (if (li/internal-attribute-name? n)
    n
    (add-definition 'attribute n spec)))

(def record (partial add-definition 'record))
(def entity (partial add-definition 'entity))
(def relationship (partial add-definition 'relationship))
(def dataflow (partial add-definition 'dataflow))
(def rule (partial add-definition 'rule))
(def inference (partial add-definition 'inference))
(def resolver (partial add-definition 'resolver))

(defn remove-component [cname]
  (u/safe-set raw-store (dissoc @raw-store cname))
  (maybe-publish-remove-definition 'component cname)
  cname)

(defn event [record-name attrs]
  (if (:inferred attrs)
    record-name
    (add-definition 'event record-name attrs)))

(def remove-attribute (partial remove-definition 'attribute))
(def remove-record (partial remove-definition 'record))
(def remove-entity (partial remove-definition 'entity))
(def remove-relationship (partial remove-definition 'relationship))
(def remove-dataflow (partial remove-definition 'dataflow))
(def remove-rule (partial remove-definition 'rule))
(def remove-inference (partial remove-definition 'inference))
(def remove-resolver (partial remove-definition 'resolver))

(defn remove-event [event-name]
  (if (vector? event-name) ; pre-post event - e.g: [:after :create :AnEntity]
    (remove-definition 'event event-name)
    (when (remove-dataflow event-name)
      (remove-inference event-name)
      (remove-definition 'event event-name))))

(defn remove-definition-by-tag [kw-tag record-name]
  (case kw-tag
    :entity (do (remove-entity record-name)
                (remove-relationship record-name)
                record-name)
    :event (remove-event record-name)
    :record (remove-record record-name)
    :rule (remove-rule record-name)
    :inference (remove-inference record-name)
    :resolver (remove-resolver record-name)
    :attribute (remove-attribute record-name)))

(defn fetch-attributes [tag record-name]
  (when-let [d (find-def tag record-name)]
    (last d)))

(def entity-attributes (partial fetch-attributes 'entity))
(def record-attributes (partial fetch-attributes 'record))
(def relationship-attributes entity-attributes)

(defn fetch-all-defs [tag component-name]
  (filter #(= tag (first %)) (get @raw-store component-name)))

(def fetch-all-dataflows (partial fetch-all-defs 'dataflow))
(def fetch-all-rules (partial fetch-all-defs 'rule))
(def fetch-all-inferences (partial fetch-all-defs 'inference))
(def fetch-all-resolvers (partial fetch-all-defs 'resolver))

(defn record-attributes-include-inherits [entity-name]
  (let [raw-attrs (or (entity-attributes entity-name)
                      (record-attributes entity-name))
        attrs (apply dissoc raw-attrs li/property-names)]
    (if-let [p (:inherits (:meta raw-attrs))]
      (merge attrs (record-attributes-include-inherits p))
      attrs)))

(defn entity-meta [entity-name]
  (:meta (entity-attributes entity-name)))

(defn- fix-rules-syntax [defs]
  (mapv (fn [d]
          (if (and (seqable? d)
                   (= 'rule (first d)))
            `(~(symbol "rule") ~(second d) ~@(first (nthrest d 2)))
            d))
        defs))

(defn as-edn [component-name]
  (when-let [defs (seq (get @raw-store component-name))]
    `(do ~@(fix-rules-syntax defs))))

(defn raw-store-reset! []
  (u/safe-set raw-store {}))
