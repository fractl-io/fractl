(ns fractl.resolver.wsdl.wsdl-parser
  (:require [clojure.data.xml :as data-xml]
            [clojure.string :as str]))

(defn- norm-tns-ref
  [tns-name]
  (str/replace tns-name #"tns:" ""))

(defn- parse-attr
  [schema]
  (case (:tag schema) 
    :xsd:element
    (let [attrname (get-in schema [:attrs :name])
          attrtype (norm-tns-ref (get-in schema [:attrs :type]))
          attrinfo (get schema :attrs)
          attrmeta (dissoc attrinfo :name :type)]
      [attrname {:type attrtype :meta attrmeta}])
    
    #_(println "TBD: parse-attr - " schema)
    nil))

(defn- parse-enum
  [schema]
  (case (:tag schema)
    :xsd:enumeration
    (get-in schema [:attrs :value])
    
    #_(println "TBD: parse-enum - " schema)
    nil))

(defn- parse-record
  [element]
  (case (:tag element)
    :xsd:sequence
    {:attributes (into {} (map parse-attr (:content element)))}

    :xsd:complexContent
    (let [schema (first (:content element))]
      (case (:tag schema)
        :xsd:extension
        (let [base (get-in schema [:attrs :base])]
          (assoc (parse-record (first (:content schema)))
                 :meta {:extends (norm-tns-ref base)}))))))

(defn- parse-enums
  [schema]
  (case (:tag schema)
    :xsd:restriction
    {:values (into #{} (map parse-enum (:content schema)))}))

(defn parse-schema
  [element]
  (case (:tag element)
    :xsd:complexType
    (let [recname (get-in element [:attrs :name])]
      [recname 
       (assoc (parse-record (first (:content element)))
              :type :record)])

    :xsd:simpleType
    (let [recname (get-in element [:attrs :name])]
      [recname
       (assoc (parse-enums (first (:content element)))
              :type :enum)])

    :xsd:element
    (let [recname (get-in element [:attrs :name])
          in-element (first (:content element))]
      (when (= (:tag in-element) :xsd:complexType)
        [recname
         (assoc (parse-record (first (:content in-element)))
                :type :record)]))

    nil))

(defn get-schema
  [element]
  (case (:tag element)
    :definitions
    (get-schema (first (:content element)))

    :types
    (get-schema (first (:content element)))

    :xsd:schema
    (:content element)

    nil))

(defn- norm-content
  [content]
  (if-let [c (get content :content)]
    (assoc content :content (map norm-content 
                                 (filter #(not= % "\n") c)))
    content))

(defn parse-xml
  [content]
  (let [parsed (data-xml/parse (java.io.StringReader. content)
                               :namespace-aware false)]
    (norm-content parsed)))
