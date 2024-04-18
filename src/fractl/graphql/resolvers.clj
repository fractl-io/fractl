(ns fractl.graphql.resolvers
  (:require [clojure.string :as str]
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.graphql.generator :as gg]
            [fractl.lang.kernel :as kernel]
            [fractl.lang :as lang]
            [fractl.lang.internal :as fl]
            [fractl.lang.raw :as lr]
            [fractl.evaluator :as ev]
            [fractl.paths.internal :as pi]
            [fractl.util :as u]
            [fractl.util.logger :as log]))

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
  (let [event (register-event component (as-vec fractl-patterns))]
      (try
        (let [result (first (ev/eval-all-dataflows {event {}}))]
          (if (= (:status result) :error)
            (u/throw-ex (str "Error: " (:message result)))
            (:result result)))
        (finally
          (cn/remove-event event)))))

(defn query-entity-by-attribute-resolver
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

(defn query-parent-children-resolver
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

(defn query-contained-entity-resolver
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
          results (if results
                 (cond
                   (map? results) [results]
                   (coll? results) results
                   :else [])
                 [])]
      results)))

(defn query-between-relationship-resolver
  [relationship-name entity1-name entity2-name]
  (fn [context args value]
    (let [args (:attributes args)
          core-component (first (get-app-components))
          query-params (append-question-to-keys args)
          query {relationship-name query-params}
          results (eval-patterns core-component query)
          results (if results
                         (cond
                           (map? results) [results]
                           (coll? results) results
                           :else [])
                         [])]
      results)))

(defn create-entity-resolver
  [entity-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (first (get-app-components))
          create-pattern {entity-name args}
          results (eval-patterns core-component create-pattern)]
      (first results))))

(defn create-contained-entity-resolver
  [relationship-name parent-name child-name]
  (fn [context args value]
    (let [core-component (first (get-app-components))
          schema (schema-info core-component)
          parent-guid (find-guid-or-id-attribute schema parent-name)
          args (:input args)
          parent-guid-arg-pair (get-combined-key-value args parent-name parent-guid)
          [parent-guid-attribute parent-guid-value] (first (seq parent-guid-arg-pair))
          child-params (dissoc args parent-guid-attribute)
          fetch-parent-query {parent-name {(append-question-mark parent-guid) parent-guid-value}}
          create-child-query {child-name child-params
                              :-> [[relationship-name fetch-parent-query]]}
          results (eval-patterns core-component create-child-query)]
       (assoc results parent-guid-attribute parent-guid-value))))

(defn create-between-relationship-resolver
  [relationship-name entity1-name entity2-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (first (get-app-components))
          create-pattern {relationship-name args}
          results (first (eval-patterns core-component create-pattern))]
      results)))

(defn update-entity-resolver
  [entity-name]
  (fn [context args value]
    (let [core-component (first (get-app-components))
          schema (schema-info core-component)
          args (:input args)
          entity-guid-attr (find-guid-or-id-attribute schema entity-name)
          entity-guid-val (entity-guid-attr args)
          update-pattern {(form-pattern-name core-component "Update" (extract-entity-name entity-name))
                            {entity-guid-attr entity-guid-val
                             :Data (dissoc args entity-guid-attr)}}
          results (first (eval-patterns core-component update-pattern))]
      results)))

(defn update-contained-entity-resolver
  [relationship-name parent-name child-name]
  (fn [context args value]
    (let [core-component (first (get-app-components))
          schema (schema-info core-component)
          args (:input args)
          child-guid (find-guid-or-id-attribute schema child-name)
          parent-guid (find-guid-or-id-attribute schema parent-name)
          parent-guid-arg-pair (get-combined-key-value args parent-name parent-guid)]

      (when-not parent-guid-arg-pair
        (throw (Exception. (str "Error: " parent-guid " not provided for " parent-name))))

      (let [[parent-guid-attribute parent-guid-value] (first (seq parent-guid-arg-pair))
            child-params (dissoc args parent-guid-attribute child-guid)
            extracted-child-name (keyword (extract-entity-name child-name))
            [child-id _] (gg/find-attribute extracted-child-name (:entity-metas context) :id)
            child-id-value (child-id args)]

        (when-not child-id-value
          (throw (Exception. (str "Error: " child-id " not provided for " child-name ". It is needed to identify contained
          entity."))))

        (let [path-attr fl/path-attr
              child-path (pi/path-string
                           (pi/as-fully-qualified-path
                             core-component
                             (pi/uri-join-parts [(extract-entity-name parent-name)
                                                 parent-guid-value
                                                 (extract-entity-name relationship-name)
                                                 (extract-entity-name child-name)
                                                 child-id-value])))
              update-pattern {(form-pattern-name core-component "Update" (extract-entity-name child-name))
                              {:Data child-params
                               path-attr (str "path:/" child-path)}}
              results (eval-patterns core-component update-pattern)]
          (assoc (first results) parent-guid-attribute parent-guid-value))))))

(defn update-between-relationship-resolver
  [relationship-name entity1-name entity2-name]
  (fn [context args value]
    (let [core-component (first (get-app-components))
          schema (schema-info core-component)
          args (:input args)
          extracted-entity1-name (keyword (extract-entity-name entity1-name))
          extracted-entity2-name (keyword (extract-entity-name entity2-name))
          entity1-guid-value (extracted-entity1-name args)
          entity2-guid-value (extracted-entity2-name args)
          query-params {(append-question-mark extracted-entity1-name) entity1-guid-value
                        (append-question-mark extracted-entity2-name) entity2-guid-value}
          relationship-params (dissoc args extracted-entity1-name extracted-entity2-name)
          update-pattern {relationship-name
                            (merge query-params relationship-params)}
          results (first (eval-patterns core-component update-pattern))]
      results)))

(defn delete-entity-resolver
  [entity-name]
  (fn [context args value]
    (let [core-component (first (get-app-components))
          schema (schema-info core-component)
          entity-guid (find-guid-or-id-attribute schema entity-name)
          delete-pattern [[:delete entity-name {entity-guid (entity-guid args)}]]
          results (first (eval-patterns core-component delete-pattern))]
       results)))

(defn generate-query-resolver-map [schema]
  (let [entities (mapv (fn [entity] (key (first entity))) (:entities schema))
        relationships (into {} (map (fn [rel] [(key (first rel)) (:meta (val (first rel)))]) (:relationships schema)))
        entity-resolvers (reduce (fn [acc entity-key]
                                   (assoc acc
                                          (keyword (str "Query/"
                                                        (name (last (str/split (name entity-key) #"\.")))))
                                          (fractl.graphql.resolvers/query-entity-by-attribute-resolver entity-key)))
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
                                                           (fractl.graphql.resolvers/query-parent-children-resolver))
                                                    (assoc (keyword (str relation-name "/" child-name))
                                                           (fractl.graphql.resolvers/query-contained-entity-resolver rel parent child))))
                                              between
                                              (let [[entity1 entity2] between
                                                    entity1-name (name (last (str/split (name entity1) #"\.")))
                                                    entity2-name (name (last (str/split (name entity2) #"\.")))
                                                    relation-name (name (last (str/split (name rel) #"\.")))]
                                                (assoc acc
                                                       (keyword (str "Query/" relation-name))
                                                       (fractl.graphql.resolvers/query-between-relationship-resolver rel entity1 entity2)))))
                                          {} relationships)]
    (merge entity-resolvers relationship-resolvers)))

(defn generate-mutation-resolvers
  ([schema mutation-type]
   (generate-mutation-resolvers schema mutation-type true))
  ([schema mutation-type include-relationship-resolvers?]
  (let [mutation-type-lower-case (str/lower-case mutation-type)
        entities (mapv (fn [entity] (key (first entity))) (:entities schema))
        relationships (map (fn [rel] [(key (first rel)) (:meta (val (first rel)))]) (:relationships schema))
        entity-mutation-resolvers (reduce (fn [acc entity-key]
                                            (assoc acc
                                                   (keyword (str "Mutation/" mutation-type (name (last (str/split (name entity-key) #"\.")))))
                                                   ((resolve (symbol (str "fractl.graphql.resolvers/" mutation-type-lower-case "-entity-resolver"))) entity-key)))
                                          {} entities)
        relationship-mutation-resolvers (if include-relationship-resolvers?
                                          (reduce (fn [acc [rel-key {:keys [contains between]}]]
                                                  (cond
                                                    contains
                                                    (let [[parent child] contains
                                                          child-name (name (last (str/split (name child) #"\.")))]
                                                      (assoc acc
                                                             (keyword (str "Mutation/" mutation-type child-name))
                                                             ((resolve (symbol (str "fractl.graphql.resolvers/" mutation-type-lower-case "-contained-entity-resolver"))) rel-key parent child)))

                                                    between
                                                    (let [[entity1 entity2] between
                                                          relation-name (name (last (str/split (name rel-key) #"\.")))]
                                                      (assoc acc
                                                             (keyword (str "Mutation/" mutation-type relation-name))
                                                             ((resolve (symbol (str "fractl.graphql.resolvers/" mutation-type-lower-case "-between-relationship-resolver"))) rel-key entity1 entity2)))
                                                    :else acc))
                                                {} relationships))]
    (merge entity-mutation-resolvers relationship-mutation-resolvers))))

(defn generate-resolver-map [schema]
  (let [query-resolvers (generate-query-resolver-map schema)
        create-mutation-resolvers (generate-mutation-resolvers schema "Create")
        update-mutation-resolvers (generate-mutation-resolvers schema "Update")
        delete-mutation-resolvers (generate-mutation-resolvers schema "Delete" false)]
    (merge query-resolvers create-mutation-resolvers update-mutation-resolvers delete-mutation-resolvers)))