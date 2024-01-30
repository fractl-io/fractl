(ns fractl.lang.internal
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [fractl.util :as u]
            [fractl.global-state :as gs]
            #?(:cljs
               [cljs.js :refer [eval empty-state js-eval]])))

(def id-attr :__Id__)
(def with-types-tag :with-types)

(def path-attr :__path__)
(def path-attr? :__path__?)

(def path-prefix "path:/")
(def path-prefix-len (count path-prefix))

(def guid :guid)
(def path-identity :id)

(defn proper-path? [x]
  (and (string? x)
       (s/starts-with? x path-prefix)))

(defn path-string [s]
  (subs s path-prefix-len))

(defn as-partial-path [path]
  (let [s (path-string path)
        has-child (s/ends-with? s "%")
        idx (s/last-index-of s "/")
        s1 (subs s 0 idx)]
    (if has-child
      (subs s1 0 (s/last-index-of s1 "/"))
      s1)))

(defn maybe-add-path-prefix [s]
  (if (proper-path? s)
    s
    (str path-prefix (if (= "/" (first s)) s (str "/" s)))))

(def path-query-tag :?)
(defn path-query-tag? [x] (= x :?))

(def ^:private default-path-prefix (str path-prefix "/___/"))

(defn default-path []
  (str default-path-prefix (u/uuid-string)))

(defn null-path? [s]
  (s/starts-with? s default-path-prefix))

(def path-attr-spec
  {:type :Fractl.Kernel.Lang/String
   :default default-path
   :unique true
   :indexed true})

(def meta-attr :__instmeta__)
(def meta-attr-spec {:type :Fractl.Kernel.Lang/Map
                     :optional true})
(def reserved-attrs #{path-attr meta-attr})

(def globally-unique :globally-unique)

(defn evaluate [form]
  #?(:clj (eval form)
     :cljs (eval (empty-state)
                 form
                 {:eval js-eval
                  :context :expr}
                 :value)))

(def rbac-oprs #{:read :create :update :delete :eval})

(defn- privs-list? [xs]
  (and (seqable? xs)
       (= rbac-oprs (set/union (set xs) rbac-oprs))))

(defn instance-privilege-spec? [obj]
  (and (map? obj)
       (every? string? (keys obj))
       (every? privs-list? (vals obj))))

(def cmpr-oprs [:= :< :> :<= :>= :<>])
(def query-cmpr-oprs (conj cmpr-oprs :like))
(def sql-keywords [:where :from :order-by
                   :group-by :having :limit
                   :offset :join :left-join
                   :right-join :desc])
(def oprs (concat query-cmpr-oprs sql-keywords [:not :and :or :between :in]))
(def macro-names #{:match :try :rethrow-after :for-each :delete :query :await :entity :eval})
(def property-names #{:meta :ui :rbac})

(defn operator? [x]
  (some #{x} oprs))

(def ^:private special-form-names
  (set (concat oprs macro-names property-names)))

(def ^:private reserved-names
  (set (concat
        special-form-names
        #{with-types-tag :as :resolver})))

(def event-context :EventContext)

(defn only-user-attributes [attrs]
  (apply dissoc attrs property-names))

(defn- reserved? [x]
  (some #{x} reserved-names))

(defn- capitalized? [s]
  (let [s1 (first s)]
    (= (str s1) (s/capitalize s1))))

(defn- no-special-chars? [s]
  (not-any? #{\- \$ \@ \# \! \& \^ \% \~} s))

(defn- no-invalid-chars? [s]
  (not-any? #{\+ \* \< \> \=} s))

(defn- no-restricted-chars? [s]
  (and (no-special-chars? s)
       (no-invalid-chars? s)))

(defn name?
  ([char-predic x]
   (or (= id-attr x)
       (and (keyword? x)
            (not (reserved? x))
            (not (operator? x))
            (let [s (subs (str x) 1)]
              (char-predic s)))))
  ([x] (name? no-invalid-chars? x)))

(defn wildcard? [x]
  (and (symbol? x)
       (= x (symbol "_"))))

(def varname? symbol?)
(def pathname? name?)

(defn parsed-path? [x]
  (and (coll? x) (not (map? x))))

(def ^:private quote-tag :q#)
(def ^:private unquote-tag :uq#)

(defn quoted? [x]
  (and (vector? x)
       (= quote-tag (first x))))

(defn unquoted? [x]
  (and (vector? x)
       (= unquote-tag (first x))))

(defn special-form? [x]
  (and (vector? x)
       (some #{(first x)} special-form-names)))

(defn- user-defined-macro? [k]
  ;; TODO: implemenet lookup into registered user-macro names.
  false)

(defn registered-macro? [k]
  (or (some #{k} macro-names)
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

(defn clj-import-list? [x]
  (if (and (seqable? x) (= (first x) 'quote))
    (clj-import-list? (first (rest x)))
    (and (seq x)
         (every?
           (fn [entry]
             (let [k (first entry)]
               (and (some #{k} #{:require :use :import :refer :refer-macros})
                    (or (every? vector? (rest entry)) (every? list? (rest entry))))))
           x))))

(defn do-clj-import [clj-import]
  #?(:clj
     (when (gs/in-script-mode?)
       (doseq [spec clj-import]
         (let [f (case (first spec)
                   :require require
                   :use use
                   nil)]
           (when f (apply f (rest spec))))))))

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
      (let [n (ffirst (filter (fn [[_ v]] (map? v)) pat))]
        (when (or (name? n)
                  (and (string? n) (name? (keyword n))))
          n)))))

(defn record-attributes [pat]
  (when (map? pat)
    (first (filter map? (vals pat)))))

(defn destruct-instance-pattern [pat]
  [(first (keys pat)) (first (vals pat))])

(defn split-by-delim
  "Split a delimiter-separated string into individual components.
  The delimiter must be a regex pattern."
  [delim string]
  (mapv #(keyword (.substring
                   % (if (= \: (first %)) 1 0)))
        (s/split string delim)))

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
   (when (and component obj-name)
     (keyword (str (name component) "/" (name obj-name)))))
  ([n]
   (if (keyword? n)
     n
     (let [[component obj-name] n]
       (make-path component obj-name)))))

(defn make-ref
  ([recname attrname]
   (keyword (str (subs (str recname) 1) "."
                 (if (keyword? attrname)
                   (name attrname)
                   (s/join "." (mapv name attrname))))))
  ([parts]
   (keyword (s/join "." (mapv name parts)))))

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

(def ^:private uppercase-component (partial u/capitalize #"[\s-_]" ""))

(def ^:private lowercase-component (partial u/lowercase #"[\s-_]" ""))

(defn v8ns-as-cljns
  [v8ns]
  (let [parts (s/split (name v8ns) #"\.")
        names (s/join "." (map lowercase-component parts))]
    names))

(def pwd-prefix (str "." u/path-sep))

(defn- trim-path-prefix
  "Return file-name after removing any of these path prefixes: path-prefix, ./, ., /"
  ([^String file-name ^String path-prefix]
   (cond
     (and path-prefix (s/starts-with? file-name path-prefix))
     (subs file-name (.length path-prefix))

     (s/starts-with? file-name pwd-prefix)
     (subs file-name 2)

     (or (s/starts-with? file-name u/path-sep)
         (s/starts-with? file-name "."))
     (subs file-name 1)

     :else file-name))
  ([^String file-name] (trim-path-prefix file-name nil)))

(defn component-from-filename [^String component-root-path ^String file-name]
  (let [parts (.split (trim-path-prefix
                       (trim-path-prefix file-name)
                       component-root-path)
                      u/path-sep)
        ns-parts (concat (take (dec (count parts)) parts)
                         [(u/remove-extension (last parts))])
        names (s/join "." (map uppercase-component ns-parts))]
    (keyword names)))

(defn components [^String component-ns]
  (let [parts (s/split component-ns #"\.")
        names (s/join "." (map uppercase-component parts))]
    (keyword names)))

(defn literal? [x]
  (not (or (name? x)
           (symbol? x)
           (special-form? x))))

(defn validate [predic errmsg x]
  (when-not (predic x)
    (u/throw-ex (str errmsg " - " x)))
  x)

(defn auto-generated-name? [n]
  (s/starts-with? (name n) "G__"))

(defn- valid-name?
  ([char-predic n]
   (or (= path-attr n)
       (= meta-attr n)
       (auto-generated-name? n)
       (if char-predic
         (name? char-predic n)
         (name? n))))
  ([n] (valid-name? nil n)))

(def validate-name (partial validate (partial valid-name? no-restricted-chars?) "not a valid name"))
(def validate-name-relaxed (partial validate valid-name? "not a valid name"))
(def validate-clj-imports (partial validate clj-import-list? "not a valid clj-import list"))

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

(def rel-tag :->)
(def timeout-ms-tag :timeout-ms)

(def ^:private instance-meta-keys [:as :with-types timeout-ms-tag rel-tag])

(defn normalize-instance-pattern [pat]
  (apply dissoc pat instance-meta-keys))

(defn instance-pattern? [pat]
  (when (map? pat)
    (let [ks (keys (normalize-instance-pattern pat))]
      (and (= 1 (count ks))
           (let [k (first ks)]
             (and (name? k)
                  (map? (get pat k))))))))

(defn instance-pattern-name [pat]
  (first (keys (normalize-instance-pattern pat))))

(defn instance-pattern-attrs [pat]
  (first (vals (normalize-instance-pattern pat))))

(def kw "Convert non-nil strings to keywords"
  (partial u/map-when keyword))

(defn path-parts
  "Parse a path of the form :M/R.A1.A2 to a map.
   The returned map will have the structure:
    {:component :M :record R :refs (:A1, :A2)}
   :refs may be nil. If the path is simple name like :P,
   the returned map will be {:path :P}."
  [path]
  (let [s (subs (str path) 1)
        [a b] (s/split s #"/")
        cs (seq (s/split (or b a) #"\."))
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

(defn root-path [path]
  (let [{c :component r :record} (path-parts path)]
    (make-path [c r])))

(defn query-pattern? [a]
  (and (keyword? a) (s/ends-with? (name a) "?")))

(defn query-instance-pattern? [obj]
  (or (query-pattern? obj)
      (query-pattern? (record-name obj))
      (and (map? obj)
           (some query-pattern? (keys (record-attributes obj))))))

(defn name-as-query-pattern [n]
  (keyword (str (if (keyword? n) (subs (str n) 1) n) "?")))

(defn query-target-name [q]
  (keyword (let [s (subs (str q) 1)] (subs s 0 (dec (count s))))))

(defn normalize-name [a]
  (let [n (str a)]
    (if (s/ends-with? n "?")
      (keyword (subs n 1 (dec (count n))))
      a)))

(defn macro-name? [x]
  (and (keyword? x)
    (let [c (first (name x))]
      (= c (s/lower-case c)))))

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

               (name? p)
               (let [pp (path-parts p)]
                 (conj recnames (make-path [(:component pp) (:record pp)])))

               :else recnames))
      (set recnames))))

(defn upserted-instance-attribute [rec-name]
  (keyword (str "Upserted" (name rec-name))))

(defn references-to-event-attributes
  "Return attributes for a pattern-triggered event,
  from the list of record names referenced in the pattern"
  [rec-names]
  (loop [rs rec-names, attrs {}]
    (if-let [r (first rs)]
      (let [[_ n] (split-path r)
            pn (upserted-instance-attribute n)
            spec {:type r :optional true}]
        (recur (rest rs) (assoc attrs n spec pn spec)))
      attrs)))

(defn validate-on-clause [on]
  (if (or (name? on)
          (every? name? on))
    on
    (u/throw-ex (str "invalid :on clause - " on))))

(defn- valid-where-clause? [c]
  (and (some #{(first c)} query-cmpr-oprs)
       (every? #(or (name? %)
                    (literal? %))
               (rest c))
       true))

(defn- pre-parse-name [x]
  (if (name? x)
    (let [[n r] (split-ref x)]
      [(split-path n) r])
    x))

(defn validate-where-clause [wc]
  (if-not (vector? (first wc))
    (validate-where-clause [wc])
    (if (every? valid-where-clause? wc)
      (map #(map pre-parse-name %) wc)
      (u/throw-ex (str "invalid :where clause - " wc)))))

(defn keyword-type? [x]
  (or (= x :Fractl.Kernel.Lang/Keyword)
      (= x :Fractl.Kernel.Lang/Path)))

(defn normalize-upsert-pattern [pat]
  (dissoc pat :from :as))

(defn keyword-name [n]
  (if (keyword? n) n (make-path n)))

(defn encoded-uri-path-part [entity-name]
  (let [[c n] (split-path entity-name)]
    (if (and c n)
      (str (name c) "$" (name n))
      (name entity-name))))

(defn decode-uri-path-part [part]
  (keyword (s/replace part "$" "/")))

(defn uri-path-split [path]
  (vec (filter seq (s/split (path-string path) #"/"))))

(defn uri-join-parts [parts]
  (maybe-add-path-prefix (s/join "/" parts)))

(defn fully-qualified-path-type [base-component n]
  (if (s/index-of n "$")
    (keyword (s/replace n "$" "/"))
    (keyword (str (name base-component) "/" n))))

(defn fully-qualified-path-value [base-component n]
  (if (s/starts-with? n ":")
    (fully-qualified-path-type base-component (subs n 1))
    n))

(defn- fully-qualified-path-component [base-component n]
  (if (s/index-of n "$")
    n
    (str (name base-component) "$" n)))

(defn as-fully-qualified-path [base-component path]
  (let [path (if (proper-path? path) (subs path path-prefix-len) path)
        fqt (partial fully-qualified-path-component base-component)]
    (loop [parts (filter seq (s/split path #"/")), n 0, final-path []]
      (if-let [p (first parts)]
        (case n
          (0 2 3) (recur (rest parts)
                         (if (= n 3) 1 (inc n))
                         (conj final-path (fqt p)))
          (recur (rest parts) (inc n) (conj final-path p)))
        (str path-prefix "/" (s/join "/" final-path))))))

(defn- parse-fully-qualified-name [n]
  (let [[c n] (s/split n #"\$")]
    (keyword (str c "/" n))))

(defn- fully-qualified-parts-as-map [parts]
  (when (seq parts)
    (let [[e id rel] parts
          r0 {:entity [(parse-fully-qualified-name e) id]}]
      (if rel
        (assoc
         r0 rel-tag
         [(parse-fully-qualified-name rel)
          (fully-qualified-parts-as-map (nthrest parts 3))])
        r0))))

(defn parse-fully-qualified-path [path]
  (when (proper-path? path)
    (let [path (if (s/ends-with? path "%")
                 (subs path 0 (dec (count path)))
                 path)]
      (fully-qualified-parts-as-map (s/split (subs path (inc path-prefix-len)) #"/")))))

(defn flatten-fully-qualified-path [path]
  (when-let [p (parse-fully-qualified-path path)]
    (loop [p p, f []]
      (if p
        (let [e (:entity p), [r p1] (rel-tag p)]
          (if p1
            (recur p1 (concat f e (when r [r])))
            (vec (concat f e))))
        (when (seq f) (vec f))))))

(defn full-path-name? [n]
  (s/index-of (str n) "/"))

(defn ref-path-name? [n]
  (s/index-of (str n) "."))

(def owner :owner)
(def owner-exclusive-crud :crud-exclusive-for-owner)

(defn normalize-path [p]
  (s/replace p "//" "/"))

(defn internal-attribute-name? [n]
  (let [[_ n] (split-path n)]
    (auto-generated-name? n)))

(defn between-nodenames [node1 node2 relmeta]
  (or (:as relmeta)
      (let [[_ n1] (split-path node1)
            [_ n2] (split-path node2)]
        (if (= n1 n2)
          [(keyword (str (name n1) "1"))
           (keyword (str (name n2) "2"))]
          [n1 n2]))) )

(defn name-str [n]
  (subs (str n) 1))

(defn id-ref [recname]
  (make-ref recname id-attr))

(defn prepost-event-heads [entity-name]
  [[:after :create entity-name]
   [:before :create entity-name]
   [:after :update entity-name]
   [:before :update entity-name]
   [:after :delete entity-name]
   [:before :delete entity-name]])

(def event-internal-env :-*-event-internal-env-*-)
