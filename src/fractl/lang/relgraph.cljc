(ns fractl.lang.relgraph
  "Traversal of the schema/instance graph as inferred from `contains` relationships"
  (:require [fractl.component :as cn]
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

(defn- merge-as-child [graph entity-name]
  (loop [g graph]
    (when-let [[k vs] (first g)]
      (if-let [idx (su/index-of entity-name vs)]
        (assoc
         (dissoc graph entity-name)
         k (assoc vs idx (entity-name graph)))
        (recur (rest g))))))

(defn- merge-child-nodes [entity-names graph]
  (reduce
   (fn [graph entity-name]
     (or (merge-as-child graph entity-name)
         graph))
   graph entity-names))

(defn- build-graph [entity-names]
  (merge-child-nodes
   entity-names
   (reduce
    (fn [graph entity-name]
      (let [children (cn/contained-children entity-name)
            existing-children (entity-name graph)]
        (assoc graph entity-name (concat existing-children children))))
    {} entity-names)))

(defn- nodes-in-path [path-info]
  ;; TODO: implement
  )

(defn find-nodes [root]
  (cond
    (component-name? root)
    (let [enames (cn/entity-names root)]
      (build-graph
       (seq
        (filter
         #(and (not (cn/relationship? %))
               (not (cn/meta-entity-for-any? enames %)))
         enames))))

    (path? root)
    (nodes-in-path root)

    :else
    (u/throw-ex (str "not a valid graph-root - " root))))
