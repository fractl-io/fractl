(ns fractl.lang.name-util
  "Namespace for fully-qualified name utilities"
  (:require [clojure.string :as string]
            [fractl.util.seq :as su]
            [fractl.component :as cn]
            [fractl.lang.kernel :as k]
            [fractl.lang.internal :as li]))

(defn- infer-component-with-ns [n]
  (if (k/kernel-binding? n)
    :Kernel
    (cn/get-current-component)))

(defn- fq-name
  "Return the fully-qualified (component-name/n) name of `n`."
  [n]
  (if (li/name? n)
    (let [[prefix suffix] (li/split-path n)]
      (if suffix
        ;; Check whether the prefix matches the current component-name
        ;; else lookup aliases from refs of component and return the value of
        ;; alias.
        (if (not= prefix (cn/get-current-component))
          (if-let [alias (cn/extract-alias-of-component (cn/get-current-component) prefix)]
            (li/make-path alias suffix)
            n)
          (li/make-path (infer-component-with-ns prefix) suffix))
        (let [mname (infer-component-with-ns prefix)]
          (if-not (= mname prefix)
            (li/make-path mname prefix)
            mname))))
    n))

(defn- query-key? [k]
  (string/ends-with? (str k) "?"))

(defn- looks-like-inst? [x]
  (and (= 1 (count (keys x)))
       (map? (first (vals x)))))

(declare map-with-fq-names fq-generic)

(defn vals-sanitizer [attrs]
  (let [sanattrs (for [[k v] attrs]
                   (cond
                     (keyword? v) {k v}
                     (map? v) {k v}
                     :else (if (re-matches #"\(quote .*" (str v))
                             {k v}
                             (let [qv (str "(quote " v ")")]
                               {k (read-string qv)}))))]
    (into {} sanattrs)))

(defn- fq-inst-pat
  "Update the keys and values in an instance pattern with
   component-qualified names."
  [x is-recdef]
  (let [n (first (keys x))
        attrs (first (vals x))
        mvs (vals-sanitizer attrs)
        mergvs (merge attrs mvs)]
    {(fq-name n) (map-with-fq-names mergvs is-recdef)}))

(defn- fq-map
  "Update the keys and values in a map literal with
   component-qualified names."
  [x is-recdef]
  (let [y (map (fn [[k v]]
                 (if (query-key? k)
                   [k v]
                   [(fq-name k) (fq-generic v is-recdef)]))
               x)]
    (into {} y)))

(defn- fq-generic
  "Update a data-literal in a component with fully-qualified names."
  [v is-recdef]
  (cond
    (li/name? v) (fq-name v)
    (map? v) (if (looks-like-inst? v)
               (fq-inst-pat v is-recdef)
               (fq-map v is-recdef))
    (list? v) (if-not is-recdef
                (reverse (into '() (map #(fq-generic % is-recdef) v)))
                v)
    (vector? v) (vec (map #(fq-generic % is-recdef) v))
    :else v))

(defn- fq-map-entry [[k v] is-recdef]
  (if (or (query-key? k) (= :meta k))
    [k v]
    [k (fq-generic v is-recdef)]))

(defn- map-with-fq-names [m is-recdef]
  (into {} (map #(fq-map-entry % is-recdef) m)))

(defn- fq-preproc-attribute-def
  "Preprocess an attribute definition to add fully-qualified names."
  [exp]
  `(~(symbol "attribute") ~(fq-name (second exp))
    ~(let [scm (su/third exp)]
       (if (map? scm)
         (map-with-fq-names scm false)
         (fq-name scm)))))

(defn- fq-preproc-record-def
  "Preprocess a record, entity or event definition to add fully-qualified names."
  [exp]
  (let [scm (second exp)]
    (if (map? scm)
      `(~(symbol (name (first exp)))
        ~(fq-inst-pat scm true))
      `(~(symbol (name (first exp)))
        ~(fq-name scm)
        ~(map-with-fq-names (su/third exp) true)))))

(defn- fq-named-df-pat [pat]
  (let [k (fq-name (first (keys pat)))
        vs (first (vals pat))
        mvs (vals-sanitizer vs)
        mergvs (merge vs mvs)]
    {k mergvs}))

(defn- fq-preproc-dataflow-def
  "Preprocess a dataflow to add fully-qualified names."
  [exp]
  (let [pat (second exp)
        body (nthrest exp 2)
        proc-pat (if (map? pat)
                   (fq-named-df-pat pat)
                   (fq-name pat))
        proc-body (map #(fq-generic % false) body)]
    `(~(symbol "dataflow") ~proc-pat ~@proc-body)))

;; Preprocssors to add fully-qualified names to each type
;; of expression.
(def ^:private fq-preproc-defs {'attribute fq-preproc-attribute-def
                                'record fq-preproc-record-def
                                'entity fq-preproc-record-def
                                'event fq-preproc-record-def
                                'dataflow fq-preproc-dataflow-def})

(defn fully-qualified-names [exp]
  (let [exp
        (if (seqable? exp)
          ((get fq-preproc-defs (first exp) identity) exp)
          exp)]
    exp))
