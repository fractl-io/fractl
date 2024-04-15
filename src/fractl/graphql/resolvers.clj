(ns fractl.graphql.resolvers
  (:require [clojure.string :as str]
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.lang.kernel :as kernel]
            [fractl.lang :as lang]
            [fractl.lang.internal :as fl]
            [fractl.lang.raw :as lr]
            [fractl.evaluator :as ev]))

(defn- find-schema [fetch-names find-schema]
  (mapv (fn [n] {n (cn/encode-expressions-in-schema (find-schema n))}) (fetch-names)))

(defn schema-info [component]
  {:records (find-schema #(cn/record-names component) lr/find-record)
   :entities (find-schema #(cn/entity-names component) lr/find-entity)
   :relationships (find-schema #(cn/relationship-names component) lr/find-relationship)})

(defn get-app-components []
  (cn/remove-internal-components (cn/component-names)))

(defn first-result [r]
  (:result (first r)))

(defn form-pattern-name [component-name pattern-str entity]
  (keyword (str (name component-name) "/" pattern-str "_" entity)))

(defn form-entity-name
  "Forms a keyword for an entity name from a component name.
   If the entity is already namespaced keyword, returns it as is."
  [component-name sep entity]
  (cond
    ;; if entity is a keyword and is namespaced, return as is
    (and (keyword? entity) (namespace entity)) entity

    ;; if entity is a keyword without a namespace, form a new keyword
    (keyword? entity) (keyword (str (name component-name) sep (name entity)))

    ;; if entity is a symbol, form a new keyword
    :else (keyword (str (name component-name) sep (name entity)))))

(defn append-question-mark [k]
  (keyword (namespace k) (str (name k) "?")))

(defn append-question-to-keys [m]
  (into {} (map (fn [[k v]] [(append-question-mark k) v]) m)))

(defn get-combined-key-value
  "Finds and returns a key-value pair from arg-map where the key is formed by combining entity-name and attribute-name.
   Example:
     Input: arg-map: {:Name \"NameVal\", :Content \"ContentVal\", :UserEmail \"UserEmailVal\"}
            entity-name: :User
            attribute-name: :Email
     Output: {:UserEmail \"UserEmailVal\"}"
  [arg-map entity-name attribute-name]
  (let [combined-key (keyword (str (name entity-name) (name attribute-name)))]
    (when (contains? arg-map combined-key)
      {combined-key (arg-map combined-key)})))

(defn extract-entity-name [kw]
  "input: :WordCount.Core/Document
  output: Document"
  (let [parts (str/split (name kw) #"/")]
    (last parts)))

(defn- temp-event-name [component]
  (fl/make-path
    component
    (fl/unq-name #_"generates unique name as a keyword")))

(defn- register-event [component pats]
  (let [event-name (temp-event-name component)]
    (and (apply lang/dataflow event-name pats)
         event-name)))

(defn as-vec [x]
  (if (vector? x)
    x
    [x]))

(defn eval-patterns [component fractl-patterns]
  (try
    (let [event (register-event component (as-vec fractl-patterns))]
      (try
        (ev/safe-eval {event {}})
        (finally
          (cn/remove-event event))))
    (catch Exception e
      (str "ERROR - " (.getMessage e)))))

(defn entity-by-attribute-resolver
  [entity-name]
  (fn [context args value]
    (let [core-component (first (get-app-components))
          args (:attributes args)
          query-params (append-question-to-keys args)
          dataflow-query {entity-name query-params}]
        (let [results (eval-patterns core-component dataflow-query)
              data (if results
                     (cond
                       (map? results) [results]
                       (coll? results) results
                       :else [])
                     [])]
          data))))

(defn parent-children-resolver
  []
  (fn [context args parent]
    ; simply return parent to be used by children resolver
    [parent]))

(defn find-guid-or-id-attribute [schema entity-name]
  (let [entities (:entities schema)
        entity-map (first (filter #(contains? % entity-name) entities))]
    (when entity-map
      (let [entity-def (entity-map entity-name)
            identity-attr (some (fn [[attr details]]
                                  (when (= details :Identity)
                                    attr))
                                entity-def)
            guid-attr (some (fn [[attr details]]
                              (when (= (get details :guid) true)
                                attr))
                            entity-def)
            id-attr (some (fn [[attr details]]
                            (when (= (get details :id) true)
                              attr))
                          entity-def)]
        (or identity-attr guid-attr id-attr)))))

(defn contains-relationship-resolver
  [relationship-name parent-name child-name]
  (fn [context args parent-instance]
    (let [args (:attributes args)
          core-component (first (get-app-components))
          schema (schema-info core-component)

          parent-guid-attribute (find-guid-or-id-attribute schema parent-name)
          query-params (append-question-to-keys args)

          dataflow-query [{parent-name
                           {(append-question-mark parent-guid-attribute) (parent-guid-attribute parent-instance)}
                           :as :Parent}
                          {child-name query-params
                           :-> [[relationship-name :Parent]]}]

          results (eval-patterns core-component dataflow-query)

          data (if results
                 (cond
                   (map? results) [results]
                   (coll? results) results
                   :else [])
                 [])]
      data)))

(defn between-relationship-resolver
  [relationship-name entity1-name entity2-name]
  (fn [context args value]
    (let [args (:attributes args)
          core-component (first (get-app-components))]
      (let [query-params (append-question-to-keys args)]
        (let [query {relationship-name query-params}]
          (let [results (eval-patterns core-component query)]
            (let [data (if results
                         (cond
                           (map? results) [results]
                           (coll? results) results
                           :else [])
                         [])]
              data)))))))

(defn generate-resolver-map [schema]
  (let [entities (mapv (fn [entity] (key (first entity))) (:entities schema))
        relationships (into {} (map (fn [rel] [(key (first rel)) (:meta (val (first rel)))]) (:relationships schema)))
        entity-resolvers (reduce (fn [acc entity-key]
                                   (assoc acc
                                          (keyword (str "Query/"
                                                        (name (last (str/split (name entity-key) #"\.")))))
                                          (fractl.graphql.resolvers/entity-by-attribute-resolver entity-key)))
                                 {} entities)
        relationship-resolvers (reduce-kv (fn [acc rel {:keys [contains between]}]
                                            (cond
                                              contains
                                              (let [[parent child] contains
                                                    parent-name (name (last (str/split (name parent) #"\.")))
                                                    child-name (name (last (str/split (name child) #"\.")))
                                                    relation-name (name (last (str/split (name rel) #"\.")))]
                                                (-> acc
                                                    (assoc (keyword (str parent-name "/" relation-name))
                                                           (fractl.graphql.resolvers/parent-children-resolver))
                                                    (assoc (keyword (str relation-name "/" child-name))
                                                           (fractl.graphql.resolvers/contains-relationship-resolver rel parent child))))
                                              between
                                              (let [[entity1 entity2] between
                                                    entity1-name (name (last (str/split (name entity1) #"\.")))
                                                    entity2-name (name (last (str/split (name entity2) #"\.")))
                                                    relation-name (name (last (str/split (name rel) #"\.")))]
                                                (assoc acc
                                                       (keyword (str "Query/" relation-name))
                                                       (fractl.graphql.resolvers/between-relationship-resolver rel entity1 entity2)))))
                                          {} relationships)]
    (merge entity-resolvers relationship-resolvers)))
