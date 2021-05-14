(ns fractl.lang.internal
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [fractl.util :as util]
            #?(:cljs
               [cljs.js :refer [eval empty-state js-eval]])))

(defn evaluate [form]
  #?(:clj (eval form)
     :cljs (eval (empty-state)
                 form
                 {:eval js-eval
                  :context :expr}
                 :value)))

(defn- not-reserved? [x]
  (not-any? #{x} #{:Error :Future :DataflowResult}))

(defn- capitalized? [s]
  (let [s1 (first s)]
    (= (str s1) (string/capitalize s1))))

(defn- no-special-chars? [s]
  (not-any? #{\_ \- \$ \@ \# \! \& \^ \% \~} s))

(defn- no-invalid-chars? [s]
  (not-any? #{\+ \* \< \> \=} s))

(defn- no-restricted-chars? [s]
  (and (no-special-chars? s)
       (no-invalid-chars? s)))

(defn name?
  ([char-predic x]
   (and (keyword? x)
        (not-reserved? x)
        (let [s (subs (str x) 1)]
          (and (char-predic s)
               (capitalized? s)))))
  ([x] (name? no-invalid-chars? x)))

(defn wildcard? [x]
  (and (symbol? x)
       (= x (symbol "_"))))

(def varname? symbol?)
(def pathname? name?)
(def parsed-path? coll?)

(def quote-tag :q#)
(def unquote-tag :uq#)

(def ^:private special-form-names
  #{:match :for-each :delete
    quote-tag unquote-tag
    :and :or := :< :<= :> :>=
    :between :async :future-get
    :resolver :eval-on :pull :push :entity})

(defn special-form? [x]
  (and (vector? x)
       (some #{(first x)} special-form-names)))

(defn- user-defined-macro? [k]
  ;; TODO: implemenet lookup into registered user-macro names.
  false)

(defn registered-macro? [k]
  (or (some #{k} special-form-names)
      (user-defined-macro? k)))

(defn logical-opr? [x]
  (or (= x :and)
      (= x :or)))

(defn single-spec?
  "Check whether format contains 3 keywords with :as
  for eg: [:Directory :as :Dir]"
  [x]
  (and (= 3 (count x))
       (name? (first x))
       (= :as (second x))
       (name? (nth x 2))))

(defn specs?
  "Extension to single-spec? to check multiple entries."
  [x]
  (if (keyword? (first x))
    (single-spec? x)
    (every? single-spec? x)))

(defn import-list? [x]
  (or (nil? x)
      (and (seq x)
           (= :import (first x))
           (specs? (rest x)))))

(defn clj-list? [x]
  ;; Insert into v8compile as `(apply list (first (rest <name>)))`
  ;; Example: `[:clj [:require '[clojure.java.jdbc :as jdbc]]]`
  (or (nil? x)
      (and (seq x)
           (or (= :clj (first x))
               (= :v8 (first x)))
           (cond
             (= :require (ffirst (rest x))) true
             (= :use (ffirst (rest x))) true
             (= :refer (ffirst (rest x))) true
             (= :refer-macros (ffirst (rest x))) true
             :else false)
           (cond
             (= :as (second (first (rest (first (rest x)))))) true
             (= :only (second (first (rest (first (rest x)))))) true
             :else false))))

(defn java-list? [x]
  ;; Insert into v8compile similar to clj-list
  ;; Example: `[:java [:import '(java.util.Date)]]
  (or (nil? x)
      (and (seq x)
           (= :java (first x))
           (= :import (first (second x))))))

(defn attribute-entry-format? [x]
  (if (vector? x)
    (and (name? (second x))
         (let [tag (first x)]
           (some #{tag} #{:unique :listof})))
    (name? x)))

(defn extract-attribute-name
  "Return the attribute name from the attribute schema,
   which must be of either one of the following forms:
     - :AttributeName
     - [tag :AttributeName]"
  [attr-scm]
  (if (vector? attr-scm)
    (second attr-scm)
    attr-scm))

(defn extract-attribute-tag [attr-scm]
  (if (vector? attr-scm)
    (first attr-scm)
    attr-scm))

(defn record-name [pat]
  (if (name? pat)
    pat
    (when (map? pat)
      (let [n (first (keys pat))]
        (when (name? n) n)))))

(defn record-attributes [pat]
  (when (map? pat)
    (first (vals pat))))

(defn destruct-instance-pattern [pat]
  [(first (keys pat)) (first (vals pat))])

(defn split-by-delim
  "Split a delimiter-separated string into individual components.
  The delimiter must be a regex pattern."
  [delim string]
  (vec (map #(keyword (.substring
                       % (if (= \: (first %)) 1 0)))
            (string/split string delim))))

(defn split-path
  "Split a :Component/Member path into the individual
  components - [:Component, :Member]. If the path is already
  parsed, return it as it is."
  [path]
  (if (parsed-path? path)
    path
    (split-by-delim #"/" (str path))))

(defn split-ref
  "Split a :Component.SubNs1.SubNsN or a :Record.Attribute path into the individual
  components. If the path is already parsed, return it as it is."
  [path]
  (if (parsed-path? path)
    path
    (split-by-delim #"\." (str path))))

(defn make-path
  ([component obj-name]
   (keyword (str (name component) "/" (name obj-name))))
  ([[component obj-name]]
   (make-path component obj-name)))

(defn make-ref [recname attrname]
  (keyword (str (name recname) "." (name attrname))))

(defn has-modpath? [path]
  (some #{\/} (str path)))

(defn parsedpath-as-name [[component obj-name]]
  (make-path component obj-name))

(defn ref-as-names
  "Split a path like :Component/RecordName.AttributeName into
  [[Component RecordName] AttributeName]."
  [path]
  (let [[m rp] (split-path path)]
    (when (and m rp)
      (let [[rn an] (split-ref rp)]
        [[m rn] an]))))

(defn root-component [refpath]
  (first (split-ref refpath)))

(def ^:private uppercase-component (partial util/capitalize #"[\s-_]" ""))

(def ^:private lowercase-component (partial util/lowercase #"[\s-_]" ""))

(defn v8ns-as-cljns
  [v8ns]
  (let [parts (string/split (name v8ns) #"\.")
        names (string/join "." (map lowercase-component parts))]
    names))

(def file-separator
  #?(:clj
     java.io.File/separator
     :cljs
     "/"))

(def pwd-prefix (str "." file-separator))

(defn- trim-path-prefix
  "Return file-name after removing any of these path prefixes: path-prefix, ./, ., /"
  ([^String file-name ^String path-prefix]
   (cond
     (and path-prefix (string/starts-with? file-name path-prefix))
     (subs file-name (.length path-prefix))

     (string/starts-with? file-name pwd-prefix)
     (subs file-name 2)

     (or (string/starts-with? file-name file-separator)
         (string/starts-with? file-name "."))
     (subs file-name 1)

     :else file-name))
  ([^String file-name] (trim-path-prefix file-name nil)))

(defn component-from-filename [^String component-root-path ^String file-name]
  (let [parts (.split (trim-path-prefix
                       (trim-path-prefix file-name)
                       component-root-path)
                      file-separator)
        ns-parts (concat (take (dec (count parts)) parts)
                         [(util/remove-extension (last parts))])
        names (string/join "." (map uppercase-component ns-parts))]
    (keyword names)))

(defn components [^String component-ns]
  (let [parts (string/split component-ns #"\.")
        names (string/join "." (map uppercase-component parts))]
    (keyword names)))

(defn literal? [x]
  (not (or (name? x)
           (symbol? x)
           (special-form? x))))

;; Generic comparison
(defn- cmpr [opr x1 x2 & xs]
  (loop [xs (concat [x1 x2] xs)]
    (if-let [x (first xs)]
      (if-let [y (second xs)]
        (if (opr (compare x y) 0)
          (recur (rest xs))
          false)
        true)
      true)))

(def lt (partial cmpr <))
(def gt (partial cmpr >))
(def lteq (partial cmpr <=))
(def gteq (partial cmpr >=))
(def eq (partial cmpr =))

(def cmpr-oprs {:= =, :< lt, :<= lteq, :> gt, :>= gteq})

(defn cmpr-opr? [x]
  (contains? cmpr-oprs x))

(defn validate [predic errmsg x]
  (when-not (predic x)
    (util/throw-ex (str errmsg " - " x)))
  x)

(def validate-name (partial validate (partial name? no-restricted-chars?) "not a valid name"))
(def validate-name-relaxed (partial validate name? "not a valid name"))
(def validate-imports (partial validate import-list? "not a valid imports list"))
(def validate-clj-imports (partial validate clj-list? "not a valid clj require list"))
(def validate-java-imports (partial validate java-list? "not a valid java import list"))

(defn validate-bool [attrname v]
  (validate boolean? (str attrname " must be either true or false") v))

(defn mappify-alias-imports
  "Take a import statement with alias, break it
  and save it as an inverted-map"
  [imports]
  (->> imports
       flatten
       vec
       (remove #{:as})
       (apply hash-map)
       (set/map-invert)))

(defn unq-name []
  (keyword (gensym)))

(defn async-var-ref? [exp]
  (and (seqable? exp)
       (special-form? (first exp))
       (= :future-get (ffirst exp))))

(defn instance-pattern? [pat]
  (let [ks (keys pat)]
    (name? (first ks))))

(defn instance-pattern-name [pat]
  (first (keys pat)))

(defn instance-pattern-attrs [pat]
  (first (vals pat)))

(def kw "Convert non-nil strings to keywords"
  (partial util/map-when keyword))

(defn path-parts
  "Parse a path of the form :M/R.A1.A2 to a map.
   The returned map will have the structure:
    {:component :M :record R :refs (:A1, :A2)}
   :refs may be nil. If the path is simple name like :P,
   the returned map will be {:path :P}."
  [path]
  (let [s (subs (str path) 1)
        [a b] (string/split s #"/")
        cs (seq (string/split (or b a) #"\."))
        [m r] (kw [a b])
        refs (seq (kw cs))]
    (if (and r refs)
      {:component m
       :record (if refs (first refs) r)
       :refs (when-let [rs (seq (rest refs))]
               (vec rs))}
      (if refs ; must be a reference via an alias.
        {:path (first refs) :refs (rest refs)}
        {:path path}))))

(defn query-pattern? [a]
  (and (keyword? a) (string/ends-with? (name a) "?")))

(defn query-target-name [q]
  (keyword (let [s (subs (str q) 1)] (subs s 0 (dec (count s))))))

(defn normalize-attr-name [a]
  (let [n (name a)]
    (if (string/ends-with? n "?")
      (keyword (subs n 0 (dec (count n))))
      a)))

(defn macro-name? [x]
  (and (keyword? x)
    (let [c (first (name x))]
      (= c (string/lower-case c)))))

(defn compile-event-trigger-pattern
  "Compile the dataflow match pattern into a predicate"
  [pat]
  )

(defn referenced-record-names
  "Return record names referenced in the pattern"
  [pattern]
  (loop [ps pattern, recnames []]
    (if-let [p (first ps)]
      (recur (rest ps)
             (cond
               (vector? p)
               (concat
                recnames
                (referenced-record-names p))

               (name? p) (conj recnames p)

               :else recnames))
      (set recnames))))

(defn references-to-event-attributes
  "Return attributes for a pattern-triggered event,
  from the list of record names referenced in the pattern"
  [rec-names]
  (loop [rs rec-names, attrs {}]
    (if-let [r (first rs)]
      (let [[_ n] (split-path r)]
        (recur (rest rs) (assoc attrs n r)))
      attrs)))
