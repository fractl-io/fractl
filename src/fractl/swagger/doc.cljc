(ns fractl.swagger.doc
  (:require [fractl.component :as cn]
            [ring.swagger.swagger2 :as rs]
            [schema.core :as s]
            [cheshire.core :as json]
            [clojure.string])
  (:import [java.lang String Integer Double Float Boolean]
           [java.util Date UUID]
           [java.time LocalDate LocalTime]))

(def fractlType->schemaType
  {:Kernel/String String
   :Kernel/DateTime Date
   :Kernel/Date LocalDate
   :Kernel/Time LocalTime
   :Kernel/UUID UUID
   :Kernel/Int Integer
   :Kernel/Int64 Double
   :Kernel/Float Float
   :Kernel/Double Double
   :Kernel/Boolean Boolean
   :Kernel/Email String
   :Kernel/Any s/Any
   :Kernel.Lang/String String
   :Kernel.Lang/DateTime Date
   :Kernel.Lang/Date LocalDate
   :Kernel.Lang/Time LocalTime
   :Kernel.Lang/UUID UUID
   :Kernel.Lang/Int Integer
   :Kernel.Lang/Int64 Double
   :Kernel.Lang/Float Float
   :Kernel.Lang/Double Double
   :Kernel.Lang/Boolean Boolean
   :Kernel.Lang/Email String
   :Kernel.Lang/Any s/Any
   :String String
   :DateTime Date
   :Date LocalDate
   :Time LocalTime
   :UUID UUID
   :Int Integer
   :Int64 Double
   :Float Float
   :Double Double
   :Boolean Boolean
   :Email String
   :Any s/Any})

(defn get-clojure-type [attr]
  (let [ns (namespace attr)]
    (if (or (= ns "Kernel") (= ns "Kernel.Lang"))
      (get fractlType->schemaType attr s/Any)
      (if-let [attr (cn/find-attribute-schema attr)]
        (get fractlType->schemaType (get attr :type) s/Any)
        s/Any))))

(defn fractl-entity-to-schema
  "Fractl entity to Ring Swagger compatible schema"
  [entity-name]
  (let [entity-obj (cn/find-entity-schema entity-name)
        entity-obj (:schema entity-obj)]
    {entity-name
     (zipmap
      (keys entity-obj)
      (map get-clojure-type
           (vals entity-obj)))}))

(defn all-fractl-entities-to-schema [component]
  (let [entities (cn/entity-names component)
        schemas (map fractl-entity-to-schema entities)]
    (zipmap entities schemas)))

(all-fractl-entities-to-schema :Deeds.Core)

(defn- get-unneeded-entity-dataflows [component]
  (let [entities (cn/entity-names component)]
    (into #{}
          (apply
           concat
           (map (fn [entity]
                  (let [nm (name entity)
                        ns (namespace entity)]
                    [(keyword ns (str "Lookup_Internal_" nm))
                     (keyword ns (str "Lookup_" nm "Meta"))
                     (keyword ns (str "Delete_" nm "Meta"))
                     (keyword ns (str "Upsert_" nm "Meta"))
                     (keyword ns (str "Lookup_Internal_" nm "Meta"))
                     (keyword ns (str nm "_OnUpsert_After"))
                     (keyword ns (str nm "_OnUpsert_Before"))
                     (keyword ns (str nm "_OnDelete_After"))
                     (keyword ns (str nm "_OnDelete_Before"))]))
                entities)))))

(defn get-all-events [component]
  (let [all-events (cn/event-names component)
        comp-str (name component)
        def-events (conj (get-unneeded-entity-dataflows component)
                         (keyword comp-str (str comp-str "_Init")))]
    (filter (fn [e]
              (not (contains? def-events e))) all-events)))

(defn- process-event-schema
  "Fractl Event Schema to API body/response schema
   compatible with Ring Swagger"
  [event-schema event-type]
  (let [event-schema (dissoc event-schema :EventContext :inferred)]
    (zipmap (keys event-schema)
            (map (fn [schema-type]
                   (if (or (= (namespace schema-type) "Kernel")
                           (= (namespace schema-type) "Kernel.Lang"))
                     (if (and (= schema-type :Kernel.Lang/Any)
                              (= event-type :Upsert))
                       (get-clojure-type :Kernel.Lang/UUID)
                       (get-clojure-type schema-type))
                     (s/schema-with-name
                      (fractl-entity-to-schema schema-type)
                      (name schema-type))))
                 (vals event-schema)))))

(defn get-entity-from-event [event]
  (let [event (name event)]
    (if-let [found (clojure.string/last-index-of event "_")]
      (subs event (inc found))
      event)))

(defn get-event-type [event]
  (let [en (name event)]
    (cond
      (clojure.string/starts-with? en "Upsert_") :Upsert
      :else nil)))

(defn- api-event [event]
  (when-let [event-obj (cn/find-event-schema event)]
    (let [event-sch (process-event-schema (:schema event-obj)
                                          (get-event-type event))]
      {:parameters {:path {}
                    :body event-sch}
       :responses {200 {:description "Okay"}
                   404 {:description "Error"}}
       :tags [(get-entity-from-event event)]})))

(defn all-api-events [component]
  (let [all-events (get-all-events component)]
    (into {} (apply concat
                    (map (fn [event]
                           {(str (name component) "/" (name event))
                            {:post (api-event event)}}) all-events)))))

(defn generate-swagger-json
  "Generate swagger JSON based on the information in a Fractl 
   component (events, entities, dataflows)"
  [component]
  (let [cmpn-name (name component)]
    (json/generate-string
     (rs/swagger-json
      {:info {:version "0.0.1"
              :title cmpn-name}
       :paths (all-api-events component)}))))
