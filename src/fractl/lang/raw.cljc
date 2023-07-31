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

(defn element
  ([tag delete-only record-name attrs]
   (let [s @raw-store
         [c n] (li/split-path record-name)
         cdef (get s c), elems (remrec tag record-name (get cdef :elems))
         new-cdef (assoc cdef :elems (if delete-only elems (conj elems [tag record-name attrs])))]
     (u/safe-set raw-store (assoc s c new-cdef))
     record-name))
  ([tag record-name attrs]
   (element tag false record-name attrs))
  ([tag record-name]
   (element tag true record-name nil)))

(defn attribute [n spec]
  (if (li/internal-attribute-name? n)
    n
    (element 'attribute n spec)))

(def record (partial element 'record))
(def entity (partial element 'entity))
(def relationship (partial element 'relationship))
(def dataflow (partial element 'dataflow))

(defn remove-component [cname]
  (u/safe-set raw-store (dissoc @raw-store cname))
  cname)

(defn event [record-name attrs]
  (if (:inferred attrs)
    record-name
    (element 'event record-name attrs)))

(defn remove-attribute [n]
  (element 'attribute n))

(def remove-record record)
(def remove-entity entity)
(def remove-relationship relationship)
(def remove-dataflow dataflow)
(defn remove-event [event-name]
  (when (remove-dataflow event-name)
    (element 'event event-name)))

(defn remove-definition [kw-tag record-name]
  (case kw-tag
    :entity (do (remove-entity record-name)
                (remove-relationship record-name)
                record-name)
    :event (remove-event record-name)
    :record (remove-record record-name)
    :attribute (remove-attribute record-name)))

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

(defn- elem-as-edn [elem]
  (let [[tag n spec] elem]
    (if (= 'dataflow tag)
      `(~tag ~n ~@spec)
      (seq elem))))

(defn- elems-of-tag [tag elems]
  (seq (filter #(= (first %) tag) elems)))

(defn- order-defs [cdef]
  (let [elems (reverse (:elems cdef))
        attrs (elems-of-tag 'attribute elems)
        recs (elems-of-tag 'record elems)
        ents (elems-of-tag 'entity elems)
        evts (elems-of-tag 'event elems)
        rels (elems-of-tag 'relationship elems)
        dfs (elems-of-tag 'dataflow elems)]
    (assoc cdef :elems (concat attrs recs ents rels evts dfs))))

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
