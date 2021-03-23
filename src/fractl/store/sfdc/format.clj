(ns fractl.store.sfdc.format
  "Data format translations."
  (:require [clojure.string :as s]
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
