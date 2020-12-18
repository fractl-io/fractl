(ns fractl.compiler.context
  "Context information attached to a compilation session."
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]))

(defn make []
  (u/make-cell {}))

(defn put-record!
  "A record/entity/event encounterd during the compilation
  process. This can be used to validate references downstream."  
  [ctx rec-name schema]
  (u/safe-set ctx (assoc @ctx rec-name schema)))

(defn fetch-record [ctx rec-name]
  (get @ctx rec-name))

(defn bind-variable! [ctx k v]
  (u/safe-set ctx (assoc @ctx k v)))

(defn fetch-variable [ctx k]
  (find @ctx k))

(defn lookup-record [ctx instance path]
  (if-let [r (fetch-record ctx path)]
    r
    (u/throw-ex (str "instance not found - " path))))

(defn lookup-variable [ctx k]
  (if-let [r (fetch-variable ctx k)]
    r
    (u/throw-ex (str "unbound variable - " k))))

(defn bind-compile-query-fn! [ctx r]
  (bind-variable! ctx :compile-query-fn r))

(defn fetch-compile-query-fn [ctx]
  (second (fetch-variable ctx :compile-query-fn)))

(defn add-alias! [ctx nm alias]
  (let [aliases (or (second (fetch-variable ctx :aliases)) {})
        v (if (li/parsed-path? nm) (li/make-path nm) nm)]
    (bind-variable! ctx :aliases (assoc aliases alias v))))

(defn aliased-name [ctx k]
  (let [[n r] (li/split-ref k)
        aliases (second (fetch-variable ctx :aliases))]
    (when-let [a (get aliases n)]
      (if r
        (keyword (subs (str a "." (name r)) 1))
        a))))
