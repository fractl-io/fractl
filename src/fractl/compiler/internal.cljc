(ns fractl.compiler.internal
  (:require [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.util.graph :as g]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.compiler.context :as ctx]
            [fractl.compiler.validation :as cv]))

(defn literal? [x]
  (or (number? x) (string? x) (boolean? x)))

(defn- var-in-context [ctx s]
  (if-let [[_ v] (ctx/fetch-variable ctx s)]
    v
    (u/throw-ex (str "variable not in context - " s))))

(defn- valid-attr-value [ctx k v schema]
  (cond
    (literal? v)
    (cv/validate-attribute-value k v schema)

    (symbol? v)
    (if-let [x (var-in-context ctx v)]
      (valid-attr-value ctx k x schema)
      v)

    :else v))

(defn classify-attributes [ctx pat-attrs schema]
  (loop [ps pat-attrs, result {}]
    (if-let [[ak av :as a] (first ps)]
      (recur
       (rest ps)
       (let [k (li/normalize-attr-name ak)
             v (valid-attr-value ctx k av schema)
             tag (cond
                   (li/query-pattern? ak) :query
                   (or (literal? v) (vector? v)) :computed
                   (li/name? v) :refs
                   (seqable? v) :compound
                   :else (u/throw-ex (str "not a valid attribute pattern - " a)))]
         (su/aconj result tag [k v])))
      result)))

(defn- name-in-context [ctx component rec refs]
  (if-let [inst (ctx/fetch-record ctx [component rec])]
    (do (when (seq refs)
          (let [p (li/make-path component rec)
                [_ scm] (cn/find-schema p)]
            ;; TODO: validate multi-level references.
            (when-not (some #{(first refs)} (cn/attribute-names scm))
              (u/throw-ex (str "invalid reference - " [p refs])))))
        true)
    (u/throw-ex (str "reference not in context - " [component rec refs]))))

(declare reach-name)

(defn- aliased-name-in-context [ctx schema n]
  (when-let [an (ctx/aliased-name ctx n)]
    (reach-name ctx schema an)))

(defn- reach-name [ctx schema n]
  (let [{component :component rec :record refs :refs
         path :path}
        (li/path-parts n)]
    (if path
      (if (or (cn/has-attribute? schema path)
              (aliased-name-in-context ctx schema n))
        true
        (u/throw-ex (str "reference not in schema - " path)))
      (name-in-context ctx component rec refs))))

(defn- valid-dependency [ctx schema v]
  (cond
    (and (li/name? v) (reach-name ctx schema v))
    [v false]

    (symbol? v)
    (do (var-in-context ctx v)
        nil)

    (seqable? v)
    [(seq (su/nonils (map first (map #(valid-dependency ctx schema %) (rest v)))))
     true]))

(defn add-edges-with-cycle-check [graph k vs]
  (let [g (g/add-edges graph k vs)
        cycinfo (g/detect-cycle g k)]
    (when (:cycle cycinfo)
      (u/throw-ex (str "attribute has a cyclic-dependency - " k " " (:path cycinfo))))
    g))

(defn build-dependency-graph [pat-attrs ctx schema graph]
  (let [p (partial valid-dependency ctx schema)
        g (loop [attrs pat-attrs, g graph]
            (if-let [[k v] (first attrs)]
              (let [g2 (if-let [[d mult?] (p v)]
                         (add-edges-with-cycle-check
                          g k (if mult? d [d]))
                         g)]
                (recur (rest attrs) g2))
              g))]
    g))

(defn- attr-entry [attrs n]
  (loop [attrs attrs]
    (when-let [[k vs] (first attrs)]
      (if-not (= k :computed)
        (if-let [r (first (filter #(= n (first %)) vs))]
          [k r]
          (recur (rest attrs)))
        (recur (rest attrs))))))

(defn- process-dep-entry [attrs deps result]
  (loop [deps deps, r result]
    (if-let [d (first deps)]
      (recur (rest deps)
             (if-not (some #{d} (first r))
               [(conj (first r) d)
                (if-let [e (attr-entry attrs d)]
                  (conj (second r) e)
                  (second r))]
               r))
      r)))

(defn- process-dependency [attrs sorted-dep result]
  (loop [sorted-dep sorted-dep, result result]
    (if-let [deps (seq (first sorted-dep))]
      (recur (rest sorted-dep)
             (process-dep-entry attrs deps result))
      result)))

(defn- as-sorted-attrs [attrs graph]
  (let [m (sort-by first (comp - compare)
                   (group-by count (g/all-edges graph)))
        pd (partial process-dependency attrs)]
    (loop [sorted-deps (vals (into {} m))
           result [#{} []]]
      (if-let [sd (seq (first sorted-deps))]
        (recur (rest sorted-deps)
               (pd sd result))
        (second result)))))

(defn sort-attributes-by-dependency [attrs graph]
  (as-sorted-attrs attrs (g/topological-all graph)))

(defn- process-where-clause [clause]
  (cv/ensure-where-clause
   (if (= 2 (count clause))
     (su/vec-add-first := clause)
     clause)))

(defn expand-query [entity-name query-pattern]
  (let [wildcard? (not query-pattern)
        qp (when-not wildcard? (map process-where-clause query-pattern))
        where-clause (if wildcard?
                       :*
                       (if (> (count qp) 1)
                         (su/vec-add-first :and qp)
                         (first qp)))]
    {:from entity-name
     :where where-clause}))
