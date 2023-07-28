(ns fractl.lang.raw
  "Raw (edn) representation of all interned component-elements"
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]))

(def raw-store (u/make-cell {}))

(defn component [component-name spec]
  (let [s @raw-store
        cdef (assoc (get s component-name) :spec spec)]
    (u/safe-set raw-store (assoc s component-name cdef))
    component-name))

(defn- remrec [tag record-name elems]
  (filter (fn [[t r _]] (not (and (= record-name r) (= tag t)))) elems))

(defn element [tag record-name attrs]
  (let [s @raw-store
        [c n] (li/split-path record-name)
        cdef (get s c), elems (remrec tag record-name (get cdef :elems))
        new-cdef (assoc cdef :elems (conj elems [tag record-name attrs]))]
    (u/safe-set raw-store (assoc s c new-cdef))
    record-name))

(def record (partial element 'record))
(def entity (partial element 'entity))
(def relationship (partial element 'relationship))
(def dataflow (partial element 'dataflow))

(defn event [record-name attrs]
  (if (:inferred attrs)
    record-name
    (element 'event record-name attrs)))

(defn fetch-attributes [tag record-name]
  (let [s @raw-store
        [c n] (li/split-path record-name)
        cdef (get s c)]
    (last
     (first
      (filter
       (fn [[t r _]]
         (and (= t tag) (= r record-name)))
       (get cdef :elems))))))

(def entity-attributes (partial fetch-attributes 'entity))

(defn entity-attributes-include-inherits [entity-name]
  (let [raw-attrs (entity-attributes entity-name)
        attrs (apply dissoc raw-attrs li/property-names)]
    (if-let [p (:inherits (:meta raw-attrs))]
      (merge attrs (entity-attributes-include-inherits p))
      attrs)))

(defn- elem-as-edn [elem]
  (let [[tag n spec] elem]
    (if (= 'dataflow tag)
      `(~tag ~n ~@spec)
      (seq elem))))

(defn- elems-of-tag [tag elems]
  (seq (filter #(= (first %) tag) elems)))

(defn- order-defs [cdef]
  (let [elems (reverse (:elems cdef))
        recs (elems-of-tag 'record elems)
        ents (elems-of-tag 'entity elems)
        evts (elems-of-tag 'event elems)
        rels (elems-of-tag 'relationship elems)
        dfs (elems-of-tag 'dataflow elems)]
    (assoc cdef :elems (concat recs ents rels evts dfs))))

(defn as-edn
  ([component-name ordered]
   (when-let [cdef (get @raw-store component-name)]
     (let [cdef (if ordered (order-defs cdef) cdef)]
       `(do
          ~(concat [(symbol "component") component-name]
                   (when-let [spec (:spec cdef)]
                     [spec]))
          ~@(mapv
             elem-as-edn
             ((if ordered identity reverse)
              (:elems cdef)))))))
  ([component-name] (as-edn component-name true)))
