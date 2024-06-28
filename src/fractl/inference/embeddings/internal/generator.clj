(ns fractl.inference.embeddings.internal.generator
  (:require [clojure.string :as string]
            [fractl.util.logger :as log]
            [fractl.component :as cn]
            [fractl.lang :refer :all]
            [fractl.swagger.doc :as doc]))

;; TODO: Remove global atom.
(def uniqueness (atom false))

(defn get-clojure-type [attr]
  (let [attr (if (string? attr)
               (keyword (str "Fractl.Kernel.Lang/" attr))
               attr)]
    (if-let [type (get doc/fractlType->swaggerType attr)]
      type
      (if-let [attr (cn/find-attribute-schema attr)]
        (do
          (when (or (= true (get attr :unique))
                    (= true (get attr :guid)))
            (reset! uniqueness true))
          (get doc/fractlType->swaggerType (get attr :type) {:type "string"}))
        {:type "string"}))))

(defn generate-clojure-type [attr]
  (map (fn [[k v]]
         (when-not (or (nil? v)
                       (nil? k))
           (if (keyword? v)
             {:name (name k)
              :type {:type (name v)}
              :required true}
             {:name (name k)
              :type {:type (if-not (nil? (get v :type))
                             (name (get v :type))
                             "String")}
              :required (not (or (true? (:optional v)) (true? (:read-only v))))})))
       attr))

(defn fractl-entity-to-tool-type
  "Fractl entity to Tool type compatible schema"
  ([entity-name]
   (let [entity-obj (:schema (cn/find-entity-schema entity-name))
         entity-obj (if entity-obj (dissoc entity-obj :path-info :__instmeta__ :path :meta :rbac)
                                   (cn/entity-schema entity-name))
         entity-obj-keys (keys entity-obj)
         entity-obj-values (vals entity-obj)
         names (doall (map name entity-obj-keys))
         types (doall (map get-clojure-type entity-obj-values))
         inner-maps (map (fn [n t] (merge {:name n} t {:required true})) names types)]
     (when (and (= false @uniqueness)
                (or (empty? (some #{:Id} entity-obj-keys))
                    (nil? (some #{:Id} entity-obj-keys))))
       (reset! uniqueness true))
     {entity-name inner-maps :has-uniqueness @uniqueness}))
  ([entity-name entity-schema]
   (let [entity-obj (dissoc entity-schema :path-info :__instmeta__ :path, :meta :rbac)
         entity-obj-keys (keys entity-obj)
         entity-obj-values (vals entity-obj)
         names (doall (map name entity-obj-keys))
         types (doall (map get-clojure-type entity-obj-values))
         inner-maps (map (fn [n t]
                           (merge {:name n} {:type t} {:required true})) names types)]
     (when (and (= false @uniqueness)
                (not (contains? (set entity-obj-keys) :Id)))
       (reset! uniqueness true))
     {entity-name inner-maps :has-uniqueness @uniqueness})))

;; Note: The plan later is to use this function for tool type generation
;; as, this is a better implementation than the previous one with some modification.
(defn fractl-entity-to-tool-type-for-create-operation
  "Fractl entity to Tool type compatible schema"
  [entity-name entity-schema]
  (let [entity-obj (dissoc entity-schema :path-info :__instmeta__ :path, :meta :rbac)
        inner-maps (doall
                     (generate-clojure-type entity-obj))]
    {entity-name inner-maps :has-uniqueness true}))

(defn transform-data [data-seq]
  (reduce
    (fn [result {:keys [name type]}]
      (assoc result (keyword name) (keyword (clojure.string/capitalize type))))
    {}
    data-seq))

(defn fractl-event-to-tool-type
  "Fractl event to Tool type compatible schema"
  ([event-name]
   (let [event-obj (:schema (cn/find-event-schema event-name))
         event-obj (if event-obj (dissoc event-obj :path-info :__instmeta__ :path :EventContext :meta :rbac)
                                 (cn/event-schema event-name))
         event-obj-keys (keys event-obj)
         event-obj-values (vals event-obj)
         names (doall (map name event-obj-keys))
         types (doall (map get-clojure-type event-obj-values))
         inner-maps (map (fn [n t] (merge {:name n} t {:required true})) names types)]
     (when (and (= false @uniqueness)
                (or (empty? (some #{:Id} event-obj-keys))
                    (nil? (some #{:Id} event-obj-keys))))
       (reset! uniqueness true))
     {event-name (transform-data (into [] inner-maps)) :has-uniqueness @uniqueness}))
  ([event-name event-schema]
   (let [event-schema (if (string? event-schema)
                        (read-string event-schema)
                        event-schema)
         event-obj (dissoc event-schema :path-info :__instmeta__ :path :EventContext :meta :rbac)
         event-obj-keys (keys event-obj)
         event-obj-values (vals event-obj)
         names (doall (map name event-obj-keys))
         types (doall (map get-clojure-type event-obj-values))
         inner-maps (map (fn [n t] (merge {:name n} {:type t} {:required true})) names types)]
     {event-name inner-maps})))

(defn get-key-from-fetch-string [fetch-string]
  (let [parts (clojure.string/split fetch-string #"By" 2)]
    (if (> (count parts) 1)
      (second parts)
      "Id")))

(defn filter-maps-by-name [maps name]
  (filter #(= (:name %) name) maps))

(defn generate-fetch-strings-key [filtered-keys en coll]
  (cons (str "fetch" (name en))
        (map (fn [k]
               (if (or (= k :Id) (string/includes? k "Id"))
                 (reset! uniqueness false)
                 (reset! uniqueness true))
               (str "fetch"
                    (name en)
                    (when (and coll (not (= :Id k))) (str "s")) "By" (name k))) filtered-keys)))

(defn generate-create-strings-key [en]
  (reset! uniqueness false)
  (str "create" (name en)))

(defn generate-description-string [entity key coll]
  (when (not (nil? key))
    (str "Fetch "
         (when (and coll (not (= :Id (keyword key)))) (str "list of "))
         (name entity)
         " details for given " (name key))))

(defn generate-create-description [entity entity-map]
  (let [attributes (->> (get entity-map entity)
                        (map (fn [entry]
                               (when (:required entry)
                                 (:name entry))))
                        (remove nil?)
                        (string/join " "))]
    (str "Create " (name entity) ". Requires attributes: " attributes)))

(defn generate-returns-string [key attrs coll]
  (when (not (nil? key))
    (str ""
         (when (and coll (not (= :Id (keyword key)))) (str "List of objects with "))
         "Attributes: " (string/join ", " attrs))))

(defn generate-df-pattern-entity [entity key]
  [(hash-map entity
             {(keyword (str key "?")) (keyword (str "param/" key))})])

(defn generate-df-pattern-entity-create [entity entity-map]
  (let [required-params (->> (get entity-map entity)
                             (filter :required)
                             (map :name)
                             (map (fn [k] [(keyword k) (keyword (str "param/" k))]))
                             (into {}))]
    [(hash-map entity required-params)]))

(defn generate-df-pattern-event [keys]
  [(into {} (map (fn [key] {(keyword (name key)) (keyword "params" (name key))}) keys))])

(defn calculate-returns-many [fetch-string]
  (if (> (count (string/split fetch-string #"By")) 1)
    @uniqueness
    false))

(defn generate-tool-for-event [event-name event-schema]
  (let [event-name (keyword event-name)
        event-tool-maps (fractl-event-to-tool-type event-name event-schema)
        map-of-types (get event-tool-maps event-name)
        event-keys (keys event-schema)
        filtered-keys (remove #(or (= "__instmeta__" (name %))
                                   (= "__path__" (name %))
                                   (= "__Id__" (name %))
                                   (= "Password" (name %))
                                   (= "EventContext" (name %))
                                   (= "meta" (name %))) event-keys)
        meta-keys (get event-schema :meta)
        fetch-maps {(name event-name)
                    {:description (get meta-keys :description)
                     :returns (or (get meta-keys :returns)
                                  (get meta-keys :return))
                     :returns-many (get meta-keys :returns-many)
                     :params (into [] map-of-types)
                     :df-patterns (generate-df-pattern-event filtered-keys)}}]
    fetch-maps))

(defn generate-tool-for-entity [entity-name entity-schema]
  (let [entity-name (keyword entity-name)
        entity-tool-maps (fractl-entity-to-tool-type entity-name entity-schema)
        map-of-types (get entity-tool-maps entity-name)
        is-coll (get entity-tool-maps :has-uniqueness)
        entity-keys (keys entity-schema)
        filtered-keys (remove #(or (= "__instmeta__" (name %))
                                   (= "__path__" (name %))
                                   (= "__Id__" (name %))
                                   (= "Password" (name %))
                                   (= "meta" (name %))) entity-keys)
        fetch-strings (generate-fetch-strings-key filtered-keys entity-name is-coll)
        fetch-maps (map (fn [fetch-string]
                          (let [key-name (get-key-from-fetch-string fetch-string)
                                filtered-maps-name (filter-maps-by-name map-of-types key-name)
                                filtered-maps-data-by-name (if (and (or (nil? filtered-maps-name)
                                                                        (empty? filtered-maps-name))
                                                                    (= "Id" key-name))
                                                             {:name "Id"
                                                              :type {:type "string" :format "uuid"}
                                                              :required true}
                                                             (first filtered-maps-name))
                                returns-many (calculate-returns-many fetch-string)]
                            {fetch-string {:description (generate-description-string entity-name
                                                                                     key-name
                                                                                     is-coll)
                                           :returns (generate-returns-string key-name
                                                                             (map name filtered-keys)
                                                                             is-coll)
                                           :params [filtered-maps-data-by-name]
                                           :df-patterns (generate-df-pattern-entity entity-name key-name)
                                           :returns-many returns-many}}))
                        fetch-strings)
        fetch-maps-formatted (into {} fetch-maps)
        create-string-name (generate-create-strings-key entity-name)
        entity-tool-map (fractl-entity-to-tool-type-for-create-operation entity-name entity-schema)
        create-map {create-string-name {:description (if (nil? (get-in entity-schema [:meta :comment]))
                                                       (generate-create-description entity-name entity-tool-map)
                                                       (get-in entity-schema [:meta :comment]))
                                        :returns entity-name
                                        :params (into [] (get entity-tool-map entity-name))
                                        :df-patterns (generate-df-pattern-entity-create entity-name entity-tool-map)
                                        :returns-many false}}]
    (merge fetch-maps-formatted create-map)))

(defn generate-tool-for-data [tag type schema]
  (case tag
    entity (generate-tool-for-entity type schema)
    event (generate-tool-for-event type schema)
    (log/warn (str "Don't know how to handle " tag))))
