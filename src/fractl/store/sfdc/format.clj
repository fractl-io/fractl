(ns fractl.store.sfdc.format
  "Data format translations."
  (:require [clojure.string :as s]
            [clojure.data.xml :as xml]
            [fractl.lang.internal :as li]
            [fractl.util :as u]
            [fractl.component :as cn]))

(def ^:private line-sep (System/lineSeparator))
(def ^:private xml-header "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")
(def ^:private metadata-schema-url "xmlns=\"http://soap.sforce.com/2006/04/metadata\"")

(declare attributes-as-xml)

(defn- emit-tag [tag elem]
  (let [elem-is-inst (cn/an-instance? elem)
        elem-is-map (or elem-is-inst (map? elem))
        v (if elem-is-map
            (attributes-as-xml
             (if elem-is-inst
               (cn/instance-attributes elem)
               elem))
            elem)]
    (str "<" tag ">"
         (when elem-is-inst line-sep)
         v
         (when elem-is-inst line-sep)
         "</" tag ">")))

(defn- attribute-as-xml [[k v]]
  (let [tag (u/lowercase (name k))]
    (if (vector? v)
      (s/join
       line-sep
       (map #(emit-tag tag %) v))
      (emit-tag tag v))))

(defn- attributes-as-xml [attrs]
  (s/join line-sep (map attribute-as-xml attrs)))

(defn- generate-xml-prefix [recname]
  (str xml-header line-sep
       "<" (name recname) " " metadata-schema-url ">"
       line-sep))

(defn- generate-xml-suffix [recname]
  (str line-sep "</" (name recname) ">"))

(defn instance-as-xml
  "Convert a record instance to XML"
  [record-name inst]
  (let [elems (attributes-as-xml
               (dissoc (cn/instance-attributes inst) :Id))]
    (str (generate-xml-prefix record-name)
         elems
         (generate-xml-suffix record-name))))

(defn- normalize-value [x]
  (cond
    (= x "true") true
    (= x "false") false
    :else x))

(defn- parse-attribute [elem]
  (when-not (string? elem)
    (let [[_ k] (li/split-path (get elem :tag))
          v (first (get elem :content))]
      [(keyword (u/capitalize (name k))) (normalize-value v)])))

(defn- parse-multi-attribute-value [elem]
  (let [xs (map parse-attribute elem)]
    (if (= (type (first elem)) clojure.data.xml.node.Element)
      (into {} xs)
      xs)))

(defn- last-path-component [^String s]
  (let [i (.lastIndexOf s java.io.File/separator)
        len (.length s)]
    (if (and (> i 0) (< i (dec len)))
      (.substring s (+ i 1) len)
      s)))

(defn- type-name-from-file [^String type ^String file-name]
  (let [i (.lastIndexOf file-name type)]
    (if (> i 0)
      (last-path-component (.substring file-name 0 i))
      file-name)))

(defn- fold-attributes
  "Fold duplicate keys into a single attribute of type [:listof :Kernel/Map]"
  [multi-attr-names attrs]
  (loop [attrs attrs, result {}]
    (if-let [[a b] (first attrs)]
      (if (contains? multi-attr-names a)
        (let [vs (get result a [])]
          (recur (rest attrs) (assoc result a (conj vs (into {} b)))))
        (recur (rest attrs) (assoc result a b)))
      result)))

(defn- capitalize-keys [m]
  (into {}
        (map
         (fn [[k v]]
           [(keyword (u/capitalize (name k))) v])
         m)))

(defn- normalize-maps [model-name attrs]
  (map (fn [[k v]]
         (let [v (if (and (seqable? v) (map? (first v)))
                   (first v)
                   v)]
           (if (map? v)
             [k (cn/tag-record [model-name k] v)]
             [k v])))
       attrs))

(defn- parse-generic-metadata-object [attribute-parser find-full-name
                                      fold-attributes
                                      [model-name _ :as full-recname]
                                      file-name xml]
  (let [tree (xml/parse (java.io.StringReader. xml))
        content (get tree :content)
        attrs (capitalize-keys
               ((or fold-attributes #(into {} %))
                (filter
                 identity
                 (map attribute-parser content))))
        final-attrs (normalize-maps model-name attrs)
        fn (find-full-name file-name)]
    (cn/make-instance
     full-recname
     (assoc (into {} final-attrs) :FullName fn))))

(def ^:private role-name-from-file (partial type-name-from-file ".role"))

(def parse-role (partial parse-generic-metadata-object
                         parse-attribute role-name-from-file nil))

(defn- normalize-attribute-content-seq
  "Remove newline strings from parsed xml content
   that represent an attribute value sequence."
  [c]
  (filter (complement string?) c))

(defn- parse-attribute-with-multiple-values [multi-attrs elem]
  (when-not (string? elem)
    (let [[_ k] (li/split-path (get elem :tag))
          content (get elem :content)
          v (if (contains? multi-attrs k)
              (parse-multi-attribute-value
               (normalize-attribute-content-seq content))
              (first content))]
      [k (normalize-value v)])))

(def ^:private profile-name-from-file (partial type-name-from-file ".profile"))

(def ^:private profile-multi-attrs #{:userPermissions :pageAccesses})

(def ^:private parse-profile-attribute (partial parse-attribute-with-multiple-values
                                                profile-multi-attrs))

(def parse-profile (partial parse-generic-metadata-object
                            (partial parse-attribute-with-multiple-values
                                     profile-multi-attrs)
                            profile-name-from-file
                            (partial fold-attributes profile-multi-attrs)))

(def ^:private security-settings-multi-attrs #{:passwordPolicies :sessionSettings
                                               :singleSignOnSettings})

(def parse-security-settings (partial parse-generic-metadata-object
                                      (partial parse-attribute-with-multiple-values
                                               security-settings-multi-attrs)
                                      (constantly "Security")
                                      (partial fold-attributes security-settings-multi-attrs)))
