(ns fractl.compiler.internal
  (:require [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.util.graph :as g]
            [fractl.util.logger :as log]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.compiler.context :as ctx]
            [fractl.compiler.validation :as cv]))

(defn const-value? [x]
  (or (number? x) (string? x) (boolean? x)))

(defn- var-in-context [ctx s]
  (if-let [[_ v] (ctx/fetch-variable ctx s)]
    v
    (u/throw-ex (str "variable not in context - " s))))

(defn- valid-attr-value [ctx k v schema]
  (cond
    (const-value? v)
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
                   (or (const-value? v) (vector? v)) :computed
                   (li/name? v) :refs
                   (seqable? v) :compound
                   :else (u/throw-ex (str "not a valid attribute pattern - " a)))]
         (su/aconj result tag [k v])))
      result)))

(def conditional-dataflow-tag :conditional-dataflow)

(defn- log-warn [s]
  (log/warn s))

(defn- name-in-context [ctx component rec refs]
  (if (ctx/fetch-record ctx [component rec])
    (when (seq refs)
      (let [p (li/make-path component rec)
            [_ scm] (cn/find-schema p)]
        ;; TODO: validate multi-level references.
        (when-not (cn/inferred-event-schema? scm)
          (when-not (some #{(first refs)} (cn/attribute-names scm))
            (if (= (get scm :type-*-tag-*-) :event)
              (u/throw-ex (str "Error in: Event " p " no such attribute - " (first refs)))
              (u/throw-ex (str "Invalid reference - " [p refs])))))))
    ((if (ctx/fetch-variable ctx conditional-dataflow-tag)
       log-warn
       u/throw-ex)
     (str "Reference cannot be found for "
          rec
          " Did you mean one of: "
          (first (keys (dissoc @ctx :compile-query-fn :zero-trust-rbac))))))
  true)

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

(defn- map->seqable [m]
  (map second (if (li/instance-pattern? m)
                (li/instance-pattern-attrs m)
                m)))

(defn- valid-dependency
  ([ctx schema v vals-of-map?]
   (cond
     (and (li/name? v) (reach-name ctx schema v))
     [v false]

     (symbol? v)
     (do (var-in-context ctx v)
         nil)

     (map? v)
     (valid-dependency ctx schema (map->seqable v) true)

     (seqable? v)
     [(seq (su/nonils (map first (map #(valid-dependency ctx schema %)
                                      (if vals-of-map? v (rest v))))))
      true]))
  ([ctx schema v]
   (valid-dependency ctx schema v false)))

(defn add-edges-with-cycle-check [graph k vs]
  (let [g (g/add-edges graph k vs)
        cycinfo (g/detect-cycle g k)]
    (when (:cycle cycinfo)
      (if (every? #(= k %) (:path cycinfo))
        g
        (u/throw-ex (str "attribute has a cyclic-dependency - " k " " (:path cycinfo)))))
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

(defn left-out-from-sorted [tag attrs sorted]
  (let [all-in-tag (apply
                    concat
                    (map second (filter (fn [[k _]] (= k tag)) sorted)))
        ks (filter li/name? all-in-tag)
        left-out (filter
                  (fn [[k _]]
                    (not (some #{k} ks)))
                  (tag attrs))]
    left-out))

(defn sort-attributes-by-dependency [attrs graph]
  (let [sorted (as-sorted-attrs attrs (g/topological-all graph))]
    sorted))

(defn- process-where-clause [clause]
  (cv/ensure-where-clause
   (if (and (= 2 (count clause))
            (not (vector? (first clause))))
     (su/vec-add-first := clause)
     clause)))

(defn expand-query [entity-name query-pattern]
  (let [wildcard? (not query-pattern)
        qp (when-not wildcard? (map process-where-clause query-pattern))
        where-clause (if wildcard?
                       :*
                       (if (vector? (ffirst qp))
                         (su/vec-add-first :and qp)
                         (first qp)))]
    {:from entity-name
     :where where-clause}))
