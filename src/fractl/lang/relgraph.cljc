(ns fractl.lang.relgraph
  "Traversal of the schema/instance graph as inferred from `contains` relationships"
  (:require [clojure.set :as set]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.lang.internal :as li]))

(defn- component-name? [obj]
  (and (li/name? obj)
       (cn/component-exists? obj)))

(defn- path? [obj]
  (and (map? obj)
       (let [ks (keys obj)]
         (and (= 1 (count ks))
              (li/name? (first ks))))))

(def ^:private roots-tag :-*-roots-*-)

(defn- attach-roots [graph]
  (assoc
   graph
   roots-tag
   (loop [g graph, result (set (keys graph))]
     (if-let [[k vs] (first g)]
       (if (seq vs)
         (recur (rest g) (set/difference result (set (mapv :to vs))))
         (recur (rest g) result))
       result))))

(defn- as-contains-node [[rel-name child-entity]]
  {:type :contains
   :relationship rel-name
   :to child-entity})

(defn- do-build-graph [entity-names]
  (attach-roots
   (reduce
    (fn [graph entity-name]
      (let [children (mapv as-contains-node (cn/contained-children entity-name))
            existing-children (entity-name graph)]
        (assoc graph entity-name (vec (concat existing-children children)))))
    {} entity-names)))

(defn- nodes-in-path [path-info]
  ;; TODO: implement
  )

(def graph? roots-tag)

(defn roots [graph]
  (select-keys graph [roots-tag]))

(defn rep [obj]
  (if-let [rts (roots-tag obj)]
    rts
    nil))

(defn build-graph [root]
  (cond
    (component-name? root)
    (let [enames (cn/entity-names root)]
      (do-build-graph
       (seq
        (filter
         #(and (not (cn/relationship? %))
               (not (cn/meta-entity-for-any? enames %)))
         enames))))

    (path? root)
    (nodes-in-path root)

    :else
    (u/throw-ex (str "not a valid graph-root - " root))))
