(ns agentlang.compiler.context
  "Context information attached to a compilation session."
  (:require [agentlang.util :as u]
            [agentlang.lang.internal :as li]))

(def ^:dynamic dynamic-context nil)

(declare add-alias!)

(defn make
  ([with-types]
   (let [ctx
         (u/make-cell
          (if (map? with-types)
            {:with-types with-types}
            {}))]
     (add-alias! ctx :Error)
     ctx))
  ([] (make nil)))

(defn put-record!
  "A record/entity/event encounterd during the compilation
  process. This can be used to validate references downstream."  
  [ctx rec-name schema]
  (u/call-and-set ctx #(assoc @ctx rec-name schema)))

(defn fetch-record [ctx rec-name]
  (get @ctx rec-name))

(defn put-fresh-record!
  "Call put-record! if rec-name does not exist in context."
  [ctx rec-name schema]
  (if-not (fetch-record ctx rec-name)
    (u/call-and-set ctx #(assoc @ctx rec-name schema))
    ctx))

(defn bind-variable! [ctx k v]
  (u/call-and-set ctx #(assoc @ctx k v)))

(defn fetch-variable [ctx k]
  (find @ctx k))

(defn unbind-variable! [ctx k]
  (u/call-and-set ctx #(dissoc @ctx k)))

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

(defn add-sub-alias! [ctx alias target]
  (cond
    (vector? alias)
    (doseq [a alias]
      (add-sub-alias! ctx a target))

    (keyword? alias)
    (let [aliases (or (second (fetch-variable ctx :aliases)) {})]
      (bind-variable! ctx :aliases (assoc aliases alias [:alias target])))

    :else
    (u/throw-ex (str "invalid alias identifier - " alias))))

(defn alias-name [alias]
  (if (vector? alias)
    (keyword (str "A" (hash alias)))
    alias))

(defn add-alias
  ([ctx nm alias]
   (let [alias-name (alias-name alias)
         aliases (or (second (fetch-variable ctx :aliases)) {})
         v (if (li/parsed-path? nm) (li/make-path nm) nm)]
     (bind-variable! ctx :aliases (assoc aliases alias-name v))
     (when (vector? alias)
       (doseq [a alias]
         (add-sub-alias! ctx a alias-name))))
   ctx)
  ([ctx alias]
   (add-alias ctx (alias-name alias) alias)))

(def add-alias! add-alias)

(defn redirect? [a]
  (and (vector? a) (= :alias (first a))))

(def ^:private redirect-tag second)

(defn aliased-name
  ([ctx k follow-redirect]
   (let [aliases (second (fetch-variable ctx :aliases))]
     (when-let [a (get aliases k)]
       (if (and (redirect? a) follow-redirect)
         (aliased-name ctx (redirect-tag a))
         a))))
  ([ctx k] (aliased-name ctx k true)))

(def with-types-tag li/with-types-tag)

(defn bind-with-types! [ctx types]
  (bind-variable! ctx with-types-tag types))

(defn fetch-with-types [ctx]
  (second (fetch-variable ctx with-types-tag)))

(defn dynamic-type [ctx rec-name]
  (if-let [wt (with-types-tag @ctx)]
    (let [k (if (keyword? rec-name)
              rec-name
              (li/make-path rec-name))]
      (get wt k rec-name))
    rec-name))

(defn build-partial-instance! [ctx]
  (bind-variable! ctx :partial-instance? true))

(defn build-partial-instance? [ctx]
  (fetch-variable ctx :partial-instance?))

(defn clear-build-partial-instance! [ctx]
  (unbind-variable! ctx :partial-instance?))

(defn with-build-partial-instance [ctx f]
  (try
    (do (build-partial-instance! ctx)
        (f))
    (finally
      (clear-build-partial-instance! ctx))))

(defn with-ignore-relationship-query-constraint [ctx f]
  (try
    (do (bind-variable! ctx :ignore-rel-query-constraint? true)
        (f))
    (finally
      (unbind-variable! ctx :ignore-rel-query-constraint?))))

(defn ignore-relationship-query-constraint? [ctx]
  (fetch-variable ctx :ignore-rel-query-constraint?))

(defn from-bindings [env]
  (let [ctx (make)]
    (loop [env env]
      (when-let [[k v] (first env)]
        (cond
          (vector? k) (put-record! ctx (li/make-path k) v)
          (keyword? k) (if (= 2 (count (li/split-path k)))
                         (put-record! ctx k v)
                         (add-alias! ctx k)))
        (recur (rest env))))
    ctx))
