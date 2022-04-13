(ns fractl.relationship
  (:require [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.lang.internal :as li]))

(defn- resolve-ref-for-relationship [schema attrname]
  (if-let [rf (:ref (cn/find-attribute-schema (attrname schema)))]
    (let [p (if (map? rf) rf (li/path-parts rf))]
      [(:component p) (:record p)])
    (u/throw-ex (str "Unable to resolver relationship reference - " attrname))))

(defn relationships [component]
  (loop [ns (cn/relationship-names component)
         to-counts {}, result {}]
    (if-let [n (first ns)]
      (let [find-ref (partial
                      resolve-ref-for-relationship
                      (cn/fetch-schema n))
            meta (cn/fetch-meta n)
            from (:from meta)
            to (:to meta)
            fref (find-ref from)
            tref (find-ref to)
            rels (get result n [])
            cf (get to-counts fref 0)
            ct (get to-counts tref 0)]
        (recur (rest ns) (assoc to-counts fref cf tref (inc ct))
               (assoc result n (conj rels {:from [fref :via from]
                                           :to [tref :via to]}))))
      (let [roots (sort #(< (second %1) (second %2)) to-counts)]
        {:graph result
         :roots roots}))))

(def relationship-name first)
(def relationship-spec second)

(def root-record first)
(def root-rank second)

(defn participation [relspec recname]
  (let [s (first relspec)
        n (li/split-path recname)
        f (:from s)
        t (:to s)
        from (when (= n (first f))
               (nth f 2))
        to (when (= n (first t))
             (nth t 2))]
    (when (or from to)
      {:from from :to to})))
