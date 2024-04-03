(ns fractl.tool-generator.core
  (:require [clojure.set :refer [union]]
            [clojure.string :as string]
            [fractl.component :as cn]
            [fractl.swagger.doc :as doc]))

(defn fractl-entity-to-tool-type
  "Fractl entity to Tool type compatible schema"
  [entity-name]
  (let [entity-obj (:schema (cn/find-entity-schema entity-name))
        entity-obj (if entity-obj (dissoc entity-obj :path-info :__instmeta__ :path)
                                  (cn/record-schema entity-name))
        entity-obj-keys (keys entity-obj)
        entity-obj-values (vals entity-obj)
        names (map name entity-obj-keys)
        types (map doc/get-clojure-type entity-obj-values)
        inner-maps (map (fn [n t] {:name n :type t :required true}) names types)]
    inner-maps))

(defn get-key-from-fetch-string [fetch-string]
  (let [parts (clojure.string/split fetch-string #"By")]
    (if (> (count parts) 1)
      (second parts)
      "Id")))

(defn filter-maps-by-name [maps name]
  (filter #(= (:name %) name) maps))

(defn generate-description-string [entity key attrs]
  (when (not (nil? key))
    (str "Fetch " (name entity) " details for given " (name key) ". Returns attributes: " (string/join ", " attrs))))

(defn generate-df-pattern [entity key]
  (binding [*print-namespace-maps* false]
    (pr-str [(hash-map entity
                       {(keyword (str key "?")) (keyword (str "param/" key))})])))

(defn generate-fetch-maps [component]
  (let [entities (union (cn/entity-names component)
                        (cn/relationship-names component))]
    (apply merge
           (mapcat
             (fn [entity]
               (let [entity-schema (:schema (cn/find-entity-schema entity))
                     map-of-types (fractl-entity-to-tool-type entity)
                     entity-keys (keys entity-schema)
                     filtered-keys (remove #(or (= "__instmeta__" (name %))
                                                (= "__path__" (name %))
                                                (= "__Id__" (name %))) entity-keys)
                     fetch-strings (cons (str "fetch" (name entity))
                                         (map (fn [k] (str "fetch" (name entity) "By" (name k))) filtered-keys))
                     fetch-maps (map (fn [fetch-string key]
                                       (let [key-name (get-key-from-fetch-string fetch-string)
                                             filtered-maps-data-by-name (first (filter-maps-by-name map-of-types key-name))]
                                         {fetch-string {:description (generate-description-string entity key-name (map name filtered-keys))
                                                        :params      [filtered-maps-data-by-name]
                                                        :df-patterns (generate-df-pattern entity key-name)}}))
                                     fetch-strings filtered-keys)]
                 fetch-maps))
             entities))))