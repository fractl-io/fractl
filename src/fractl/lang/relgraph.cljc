(ns fractl.lang.relgraph
  "Traversal of the schema/instance graph as inferred from `contains` relationships"
  (:require [clojure.set :as set]
            [fractl.component :as cn]
            [fractl.meta :as mt]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.lang.internal :as li]
            [fractl.evaluator :as ev]))

(defn- component-name? [obj]
  (and (li/name? obj)
       (cn/component-exists? obj)))

(defn- path? [obj]
  (and (map? obj)
       (let [ks (keys obj)]
         (and (= 1 (count ks))
              (li/name? (first ks))))))

(def ^:private roots-tag :-*-roots-*-)
(def ^:private paths-tag :-*-paths-*-)
(def ^:private back-link-tag :-*-back-link-*-)

(defn- attach-roots [graph]
  (assoc
   graph
   roots-tag
   (loop [g graph, result (set (keys graph))]
     (if-let [[k vs] (first g)]
       (if (seq vs)
         (recur
          (rest g)
          (set/difference
           result (set (filter (partial not= k) (mapv :to vs)))))
         (recur (rest g) result))
       result))))

(defn- as-node [[rel-name rel-type child-entity]]
  {:type rel-type
   :relationship rel-name
   :to child-entity})

(defn- do-build-graph [entity-names]
  (attach-roots
   (reduce
    (fn [graph entity-name]
      (let [children (mapv as-node (cn/contained-children entity-name))
            between-rels (mapv as-node (cn/between-relationships entity-name))
            existing-children (entity-name graph)]
        (assoc graph entity-name (vec (concat existing-children children between-rels)))))
    {} entity-names)))

(def roots identity)

(defn paths [graph root-node]
  (when-let [ps (root-node graph)]
    {paths-tag ps back-link-tag [root-node graph]}))

(defn- paths-source-graph [paths]
  (second (back-link-tag paths)))

(defn- paths-rep [obj]
  (when-let [ps (paths-tag obj)]
    (set (mapv :relationship ps))))

(defn descend [paths rel-name]
  (when-let [path (first (filter #(= rel-name (:relationship %))
                                 (paths-tag paths)))]
    (assoc (paths-source-graph paths)
           roots-tag (set [(:to path)]))))

(defn node-object [paths rel-name]
  (when-let [ps (paths-tag paths)]
    (first (filter #(= rel-name (:relationship %)) ps))))

(defn rep [obj]
  (if-let [rts (roots-tag obj)]
    rts
    (paths-rep obj)))

(defn build-graph [root]
  (when (component-name? root)
    (let [enames (cn/entity-names root)]
      (do-build-graph
       (seq
        (filter
         #(and (not (cn/relationship? %))
               (not (cn/meta-entity-for-any? enames %)))
         enames))))))

(defn- find-instance-contains-rels [contains-lookup instance]
  (let [[_ e :as inst-type] (li/split-path (cn/instance-type instance))
        entity-name (li/make-path inst-type)]
    (when-let [parent-rels (seq (contains-lookup entity-name))]
      (let [inst-id ((cn/identity-attribute-name entity-name) instance)
            qattr (keyword (str (name e) "?"))]
        (mapv
         (fn [p]
           [p
            (when-let [relinst (first (ev/safe-eval-pattern {(cn/relinfo-name p) {qattr inst-id}}))]
              (let [[c pe :as parent-entity] (li/split-path (cn/relinfo-to p))
                    lookupevt-name (keyword (str (name c) "/Lookup_" (name pe)))
                    pidattr (cn/identity-attribute-name parent-entity)
                    pidval (pe relinst)]
                (first (ev/safe-eval {lookupevt-name {pidattr pidval}}))))])
         parent-rels)))))

(def find-parents (partial find-instance-contains-rels cn/containing-parents))
(def find-children (partial find-instance-contains-rels cn/contained-children))
