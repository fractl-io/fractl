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

(defn element [tag record-name attrs]
  (let [s @raw-store
        [c n] (li/split-path record-name)
        cdef (get s c), elems (get cdef :elems)
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

(defn- elem-as-edn [elem]
  (let [[tag n spec] elem]
    (if (= 'dataflow tag)
      `(~tag ~n ~@spec)
      (seq elem))))

(defn as-edn [component-name]
  (when-let [cdef (get @raw-store component-name)]
    `(do
       ~(concat [(symbol "component") component-name]
                (when-let [spec (:spec cdef)]
                  [spec]))               
       ~@(mapv elem-as-edn (reverse (:elems cdef))))))
