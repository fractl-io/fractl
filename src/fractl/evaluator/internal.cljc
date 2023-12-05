(ns fractl.evaluator.internal
  (:require [clojure.string :as s]
            [fractl.lang.opcode :as opc]
            [fractl.lang.internal :as li]
            [fractl.store :as store]
            [fractl.util :as u]
            [fractl.env :as env]
            [fractl.component :as cn]))

(def dispatch-table opc/dispatch-table)

;; Result builders.
(defn make-result
  ([status result env message]
   {opc/result-status-tag status :result result
    :env env :message message})
  ([status result env] (make-result status result env nil))
  ([status] {opc/result-status-tag status}))

;; result builders
(def ok (partial make-result opc/ok-tag))

(def not-found (partial make-result opc/not-found-tag))
(def declined (partial make-result opc/declined-tag))

(defn error [message]
  (make-result opc/error-tag nil nil message))

(defn- tag-eq? [tag r]
  (= tag (opc/result-status r)))

(def ok? (partial tag-eq? opc/ok-tag))
(def error? (partial tag-eq? opc/error-tag))

(defn dummy-result [env]
  {opc/result-status-tag opc/ok-tag :env env})

(defn- name-from-path-component [component n]
  (let [k (li/fully-qualified-path-type component n)
        parts (li/split-path k)]
    (if (= 2 (count parts))
      k
      (li/make-path component k))))

(defn- parent-info-from-path [component-name path]
  (let [parts (filter seq (s/split path #"/"))
        at-root (= (count parts) 3)
        ps (if at-root parts (take-last 3 parts))
        nc (partial name-from-path-component component-name)]
    [(nc (first ps)) (second ps) (nc (last ps)) at-root]))

(defn lookup-ref-inst
  ([cast-val env recname id-attr id-val]
   (or (first (env/lookup-instances-by-attributes
               env (li/split-path recname) {id-attr id-val} true))
       (store/query-by-unique-keys
        (env/get-store env) recname [id-attr]
        {id-attr (if cast-val (cn/parse-attribute-value recname id-attr id-val) id-val)})))
  ([env recname id-attr id-val] (lookup-ref-inst true env recname id-attr id-val)))

(defn find-parent-by-path [env record-name path]
  (let [[c n] (li/split-path record-name)
        [parent pid-val relname at-root] (parent-info-from-path c path)
        pid-attr (cn/identity-attribute-name parent)]
    (when-not (cn/parent-via? relname record-name parent)
      (u/throw-ex (str "not in relationship - " [relname record-name parent])))
    (when-let [result (if at-root
                        (lookup-ref-inst env parent pid-attr pid-val)
                        (let [fq (partial li/as-fully-qualified-path c)
                              path-val (fq (str li/path-query-prefix (subs path 0 (s/last-index-of path "/"))))]
                          (or (first (env/lookup-instances-by-attributes
                                      env (li/split-path parent) {li/path-attr path-val}))
                              (store/query-by-unique-keys
                               (env/get-store env) parent [li/path-attr] {li/path-attr path-val}))))]
      (if (map? result) result (when (seq result) (first result))))))

(defn find-parent-by-full-path [env child-type child-inst]
  (when-let [path (li/path-attr child-inst)]
    (when-not (li/null-path? path)
      (find-parent-by-path
       env child-type
       (let [p (li/as-partial-path path)]
         (subs p 0 (s/last-index-of p "/")))))))
