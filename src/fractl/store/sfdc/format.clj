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
        v (if elem-is-inst
            (attributes-as-xml (cn/instance-attributes elem))
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

(defn- parse-generic-metadata-object [attribute-parser find-full-name
                                      full-recname file-name xml]
  (let [tree (xml/parse (java.io.StringReader. xml))
        content (get tree :content)
        attrs (filter
               identity
               (map attribute-parser content))
        fn (find-full-name file-name)]
    (cn/make-instance
     full-recname
     (assoc (into {} attrs) :FullName fn))))

(def ^:private role-name-from-file (partial type-name-from-file ".role"))

(def parse-role (partial parse-generic-metadata-object
                         parse-attribute role-name-from-file))

(defn- parse-profile-attribute [elem]
  ;; TODO: implement parsing of profile attributes XML elements.
  )

(def ^:private profile-name-from-file (partial type-name-from-file ".profile"))

(def parse-profile (partial parse-generic-metadata-object
                            parse-profile-attribute
                            profile-name-from-file))
