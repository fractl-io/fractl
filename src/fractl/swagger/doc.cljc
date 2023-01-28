(ns fractl.swagger.doc
  (:require [fractl.component :as cn]
            [ring.swagger.swagger2 :as rs]
            [schema.core :as s]
            [cheshire.core :as json])
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
   :Kernel/Any s/Any})

(defn get-clojure-type [attr]
  (let [ns (namespace attr)]
    (if (= ns "Kernel")
      (get fractlType->schemaType attr s/Any)
      (if-let [attr (cn/find-attribute-schema attr)]
        (get fractlType->schemaType (get attr :type) s/Any)
        s/Any))))

(defn fractl-entity-to-schema
  "Fractl entity to Ring Swagger compatible schema"
  [entity-name]
  (let [entity-obj (cn/find-entity-schema entity-name)
        entity-obj (:schema entity-obj)]
    (zipmap
     (keys entity-obj)
     (map get-clojure-type
          (vals entity-obj)))))

(defn all-fractl-entities-to-schema [component]
  (let [entities (cn/entity-names component)
        schemas (map fractl-entity-to-schema entities)]
    (zipmap entities schemas)))

(defn- get-unneeded-entity-dataflows [component]
  (let [entities (cn/entity-names component)]
    (into #{}
          (apply
           concat
           (map (fn [entity]
                  (let [nm (name entity)
                        ns (namespace entity)]
                    [(keyword ns (str nm "_OnUpsert_After"))
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
  [event-schema]
  (let [event-schema (dissoc event-schema :EventContext :inferred)]
    (zipmap (keys event-schema)
            (map (fn [schema-type]
                   (if (= (namespace schema-type) "Kernel")
                     (get-clojure-type schema-type)
                     (s/schema-with-name
                      (fractl-entity-to-schema schema-type)
                      (name schema-type))))
                 (vals event-schema)))))

(defn- api-event [event]
  (when-let [event-obj (cn/find-event-schema event)]
    (let [event-sch (process-event-schema (:schema event-obj))]
      {:parameters {:path {}
                    :body event-sch}
       :responses {200 {:description "Okay"}
                   404 {:description "Error"}}})))

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
      {:info {:version "1.0.0"
              :title cmpn-name
              :description "Sausage description"
              :termsOfService "http://helloreverb.com/terms/"
              :contact {:name "My API Team"
                        :email "foo@example.com"
                        :url "http://www.metosin.fi"}
              :license {:name "Eclipse Public License"
                        :url "http://www.eclipse.org/legal/epl-v10.html"}}
       :paths (all-api-events component)}))))
