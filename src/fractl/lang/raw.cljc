(ns fractl.lang.raw
  "Raw (edn) representation of all interned component-elements"
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]))

(def raw-store (u/make-cell {}))

(defn component [component-name spec]
  (let [s @raw-store, cdef (get s component-name '())
        cspec (concat `(~'component ~component-name) (when spec [spec]))
        new-cdef (conj (rest cdef) cspec)]
    (u/safe-set raw-store (assoc s component-name new-cdef))
    component-name))

(defn intern-component [component-name defs]
  (u/safe-set raw-store (assoc @raw-store component-name (seq defs)))
  component-name)

(defn- infer-component-name [defs]
  (when (seqable? defs)
    (when-not (or (map? defs) (string? defs))
      (when-let [d (first (filter #(= 'component (first %)) defs))]
        (second d)))))

(defn maybe-intern-component [defs]
  (when-let [component-name (infer-component-name defs)]
    (intern-component component-name defs)))

(defn- extract-def-name [obj]
  (if (keyword? obj)
    obj
    (li/record-name obj)))

(defn- def? [tag full-name rec-name obj]
  (and (= (first obj) (symbol tag))
       (let [n (extract-def-name (second obj))]
         (or (= n rec-name) (= n full-name)))))

(defn- get-all-defs [record-name]
  (let [s @raw-store
        [c n] (li/split-path record-name)]
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

(defn find-attribute [n]
  (when-not (li/internal-attribute-name? n)
    (find-defspec 'attribute n)))

(defn- change-defs [f]
  (when-let [[c defs] (f)]
    (u/safe-set raw-store (assoc @raw-store c defs))
    defs))

(defn add-definition [tag record-name attrs]
  (when (change-defs #(replace-def tag record-name attrs))
    record-name))

(defn remove-definition [tag record-name]
  (when (= 'event (symbol tag))
    (remove-definition 'dataflow record-name))
  (when (change-defs #(remove-def tag record-name))
    record-name))

(defn attribute [n spec]
  (if (li/internal-attribute-name? n)
    n
    (add-definition 'attribute n spec)))

(def record (partial add-definition 'record))
(def entity (partial add-definition 'entity))
(def relationship (partial add-definition 'relationship))
(def dataflow (partial add-definition 'dataflow))

(defn remove-component [cname]
  (u/safe-set raw-store (dissoc @raw-store cname))
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
(def remove-event (partial remove-definition 'event))

(defn remove-event [event-name]
  (when (remove-dataflow event-name)
    (remove-definition 'event event-name)))

(defn remove-definition-by-tag [kw-tag record-name]
  (case kw-tag
    :entity (do (remove-entity record-name)
                (remove-relationship record-name)
                record-name)
    :event (remove-event record-name)
    :record (remove-record record-name)
    :attribute (remove-attribute record-name)))

(defn fetch-attributes [tag record-name]
  (when-let [d (find-def tag record-name)]
    (last d)))

(def entity-attributes (partial fetch-attributes 'entity))
(def relationship-attributes entity-attributes)

(defn entity-attributes-include-inherits [entity-name]
  (let [raw-attrs (entity-attributes entity-name)
        attrs (apply dissoc raw-attrs li/property-names)]
    (if-let [p (:inherits (:meta raw-attrs))]
      (merge attrs (entity-attributes-include-inherits p))
      attrs)))

(defn as-edn [component-name]
  (when-let [defs (seq (get @raw-store component-name))]
    `(do ~@defs)))

(defn raw-store-reset! []
  (u/safe-set raw-store {}))
