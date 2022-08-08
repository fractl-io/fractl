(ns fractl.lang.name-util
  "Namespace for fully-qualified name utilities"
  (:require [clojure.string :as string]
            [clojure.walk :as w]
            [fractl.util.logger :as log]
            [fractl.util.seq :as su]
            [fractl.component :as cn]
            [fractl.lang.kernel :as k]
            [fractl.lang.internal :as li]))

(defn- normalize-path [path]
  (let [s (str path)]
    (if (string/ends-with? s "?")
      (keyword (subs s 1 (dec (count s))))
      path)))

(def ^:dynamic fq-name nil)

(defn- make-fq-name
  "Return a function that returns the fully-qualified (component-name/n) name of `n`."
  [declared-names]
  (let [component-name (:component declared-names)
        recs (:records declared-names)]
    (fn [n]
      (if (li/name? n)
        (let [path (:path (li/path-parts n))]
          (if (and path (some #{(normalize-path path)} recs))
            (li/make-path component-name n)
            n))
        n))))

(defn- looks-like-inst? [x]
  (and (= 1 (count (keys x)))
       (map? (first (vals x)))))

(declare map-with-fq-names fq-generic)

(defn- sanitized? [v]
  (or (keyword? v)
      (map? v)
      (string? v)
      (number? v)
      (boolean? v)))

(defn- vals-sanitizer [attrs]
  (into
   {}
   (for [[k v] attrs]
     (if (sanitized? v)
       {k v}
       (if (= 'quote (first v))
         {k v}
         (let [qv (str "(quote " v ")")]
           {k (eval (read-string qv))}))))))

(defn- fq-inst-pat
  "Update the keys and values in an instance pattern with
   component-qualified names."
  [x is-recdef]
  (let [n (first (keys x))
        attrs (first (vals x))
        mvs (vals-sanitizer attrs)]
    {(fq-name n) (map-with-fq-names mvs is-recdef)}))

(defn- fq-map
  "Update the keys and values in a map literal with
   component-qualified names."
  [x is-recdef]
  (let [y (mapv (fn [[k v]]
                  [(fq-name k) (fq-generic v is-recdef)])
                x)]
    (into {} y)))

(defn- fq-generic
  "Update a data-literal in a component with fully-qualified names."
  [v is-recdef]
  (if (and (keyword? v) (string/starts-with? (name v) ":%"))
    v
    (if (li/quoted? v)
      (w/prewalk
        #(if (li/unquoted? %)
           (fq-generic % is-recdef)
           %)
        v)
      (cond
        (li/name? v) (fq-name v)
        (map? v) (if (looks-like-inst? v)
                   (fq-inst-pat v is-recdef)
                   (fq-map v is-recdef))
        (su/list-or-cons? v) (if-not is-recdef
                               (doall (reverse (into '() (mapv #(fq-generic % is-recdef) v))))
                               v)
        (vector? v) (mapv #(fq-generic % is-recdef) v)
        :else v))))

(defn- fq-map-entry [[k v] is-recdef]
  (if (= :meta k)
    [k v]
    [k (fq-generic v is-recdef)]))

(defn- map-with-fq-names [m is-recdef]
  (into {} (doall (map #(fq-map-entry % is-recdef) m))))

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
        proc-body (mapv #(fq-generic % false) body)]
    `(~(symbol "dataflow") ~proc-pat ~@proc-body)))

;; Preprocssors to add fully-qualified names to each type
;; of expression.
(def ^:private fq-preproc-defs
  {'attribute fq-preproc-attribute-def
   'record fq-preproc-record-def
   'entity fq-preproc-record-def
   'event fq-preproc-record-def
   'dataflow fq-preproc-dataflow-def})

(defn fully-qualified-names [declared-names exp]
  (binding [fq-name (make-fq-name declared-names)]
    (if (seqable? exp)
      ((get fq-preproc-defs (first exp) identity) exp)
      exp)))
