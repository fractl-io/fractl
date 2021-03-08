(ns fractl.resolver.wsdl.typesgen
  (:require [clojure.set :as set]
            [fractl.resolver.wsdl.wsdl-parser :as wsdl]
            [loom.graph :as g]))

(def base-types #{"xsd:anyType"
                  "xsd:base64Binary"
                  "xsd:double"
                  "xsd:dateTime"
                  "xsd:int"
                  "xsd:time"
                  "xsd:boolean"
                  "xsd:string"
                  "xsd:date"})

(defn sort-knowns
  ([entities known-types]
   (loop [unknown entities known known-types]
     (let [start-count (count known)
           k-types (atom known)
           left-over
           (reduce-kv
            (fn [m k v]
              (case (:type v)
                :enum
                (do
                  (reset! k-types (conj @k-types k))
                  m)
                
                :record
                (if (every? known (map :type (vals (:attributes v))))
                  (do
                    (reset! k-types (conj @k-types k))
                    m)
                  (assoc m k v))))
            {}
            unknown)]
       (if (= (count @k-types) start-count)
         {:known known :unknown left-over}
         (recur left-over @k-types)))))
  ([entities]
   (sort-knowns entities base-types)))

(defn flatten-extends
  [entities ename]
  (let [entity-def (get entities ename)]
    (if-let [base-name (get-in entity-def [:meta :extends])]
      (let [flat-base (flatten-extends entities base-name)]
        (assoc entity-def :attributes (merge (:attributes entity-def)
                                             (:attributes flat-base))))
      entity-def)))

(defn wsdl-type->abstract-types
  [types]
  (let [flattened (into {} (map (fn [e]
                                  [e (flatten-extends types e)])
                                (keys types)))
        known (sort-knowns flattened base-types)
        circular-deps
        (set
         (keys (let [ks (into #{} (:known known))]
                 (reduce-kv
                  (fn [m k v]
                    (let [deps (into #{} (map :type (vals (:attributes v))))
                          delta (set/difference deps ks)]
                      (assoc m k delta)))
                  {}
                  (:unknown known)))))
        dagnodes (set/difference (:known known) circular-deps)
        dag (select-keys flattened dagnodes)]
    dag))

(defn enums
  [definitions]
  (into {} (filter #(= (:type (second %)) :enum) definitions)))

(defn records
  [definitions]
  (into {} (filter #(= (:type (second %)) :record) definitions)))

(defn metadata-records
  [records]
  (into {} (filter #(= (get-in (second %) [:meta :extends]) "Metadata")) records))

(defn root-defs
  [definitions]
  (let [g (g/digraph definitions)]
    (set/difference
     (:nodeset g)
     (set (keys (:in g))))))

(comment

  (def processed-deps
    (reduce-kv
     (fn [m k v]
       (case (:type v)
         :record
         (assoc m k (into #{} (map :type (vals (:attributes v)))))
         
         :enum
         (assoc m k #{})))
     {}
     entity-defs))
  
  (def dependencies
    (g/digraph processed-deps))

  (reverse (ga/topsort dependencies))


  (def all-roots (roots dependencies))

  (def records (clojure.set/difference all-roots (set (keys enums))))

  (def record-dependencies
    (into {} (map (fn [r] [r (filter #(not= r %) (ga/post-traverse dependencies r))]) records)))

  (def inverted-deps
    (reduce-kv
     (fn [m k v]
       (loop [m m v v]
         (if-let [vk (first v)]
           (recur
            (assoc m vk (conj (get m vk #{}) k))
            (rest v))
           m)))
     {}
     record-dependencies))
  
  (def non-base-inverted-deps
    (filter (fn [[k v]] (not (some base-types #{k}))) inverted-deps))

  (count (filter (fn [[k v]] (> (count v) 1)) non-base-inverted-deps)))

(defn test-run [wsdl-file-name]
  (let [wsdl (slurp (clojure.java.io/file wsdl-file-name))
        parsed (wsdl/parse-xml wsdl)
        schema (wsdl/get-schema parsed)
        wsdl-types (into {} (map wsdl/parse-schema schema))
        abstract-types (wsdl-type->abstract-types wsdl-types)
        records (records abstract-types)
        mrecords (metadata-records records)
        rdefs (root-defs abstract-types)
        enums (enums abstract-types)]
    {:schema schema
     :wsdl-types wsdl-types
     :abstract-types abstract-types
     :records records
     :metadata-records mrecords
     :root-defs rdefs
     :enums enums}))
