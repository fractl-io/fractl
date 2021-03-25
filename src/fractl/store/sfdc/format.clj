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

(defn- attribute-as-xml [[k v]]
  (let [tag (u/lowercase (name k))]
    (str "<" tag ">" v "</" tag ">")))

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

(defn- parse-role-attribute [elem]
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

(defn- role-name-from-file [^String file-name]
  (let [i (.lastIndexOf file-name ".role")]
    (if (> i 0)
      (last-path-component (.substring file-name 0 i))
      file-name)))

(defn parse-role [full-recname file-name xml]
  (let [tree (xml/parse (java.io.StringReader. xml))
        content (get tree :content)
        attrs (filter
               identity
               (map parse-role-attribute content))
        role-name (role-name-from-file file-name)]
    (cn/make-instance
     full-recname
     (assoc (into {} attrs) :FullName role-name))))
