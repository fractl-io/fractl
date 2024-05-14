(ns fractl.graphql.resolvers
  (:require [clojure.string :as str]
            [fractl.auth.core :as auth]
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.graphql.generator :as gg]
            [fractl.lang :as lang]
            [fractl.lang.internal :as fl]
            [fractl.lang.raw :as lr]
            [fractl.evaluator :as ev]
            [fractl.paths.internal :as pi]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.lang.internal :as li]))

(defn- find-schema [fetch-names find-schema]
  (mapv (fn [n] {n (cn/encode-expressions-in-schema (find-schema n))}) (fetch-names)))

(defn schema-info [component]
  {:records (find-schema #(cn/record-names component) lr/find-record)
   :entities (find-schema #(cn/entity-names component) lr/find-entity)
   :relationships (find-schema #(cn/relationship-names component) lr/find-relationship)})

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

(defn make-auth-event-context [context]
  (let [auth-config (:auth-config context)
        request (:request context)]
    (if (and auth-config request)
      (let [user (auth/session-user (assoc auth-config :request request))]
        (when user
          {:User (:email user)
           :Sub (:sub user)
           :UserDetails user})))))

(defn eval-patterns [component patterns context]
  (let [event (register-event component (as-vec patterns))
        auth-event-context (make-auth-event-context context)]
    (try
      (let [result (first (ev/eval-all-dataflows {event {:EventContext auth-event-context}}))]
        (if (= (:status result) :error)
          (u/throw-ex (str "Error: " (:message result)))
          (:result result)))
      (finally
        (cn/remove-event event)))))

(defn query-entity-by-attribute-resolver
  [entity-name]
  (fn [context args value]
    (let [core-component (:core-component context)
          args (:attributes args)
          query-params (append-question-to-keys args)
          dataflow-query {entity-name query-params}]
        (let [results (eval-patterns core-component dataflow-query context)
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
          core-component (:core-component context)
          schema (schema-info core-component)
          parent-guid-attribute (find-guid-or-id-attribute schema parent-name)
          parent-id ((find-guid-or-id-attribute schema parent-name) parent-instance)
          extracted-parent-name (extract-entity-name parent-name)
          extracted-child-name (extract-entity-name child-name)
          extract-entity-name (extract-entity-name relationship-name)
          dataflow-result (if (nil? args)
                            (let [fq (partial pi/as-fully-qualified-path core-component)
                                  all-children-pattern {(form-pattern-name core-component "LookupAll" extracted-child-name)
                                                        {li/path-attr (fq (str "path://" extracted-parent-name "/" parent-id "/" extract-entity-name "/" extracted-child-name "/%"))}}
                                  result (eval-patterns core-component all-children-pattern context)]
                              result)
                            (let [query-params (append-question-to-keys args)
                              dataflow-query [{parent-name
                                               {(append-question-mark parent-guid-attribute) (parent-guid-attribute parent-instance)}
                                               :as :Parent}
                                              {child-name query-params
                                               :-> [[relationship-name :Parent]]}]]
                              (eval-patterns core-component dataflow-query context)))
          results (if dataflow-result
                    (cond
                      (map? dataflow-result) [dataflow-result]
                      (coll? dataflow-result) dataflow-result
                      :else [])
                    [])]
      results)))

(defn query-between-relationship-resolver
  [relationship-name entity1-name entity2-name]
  (fn [context args value]
    (let [args (:attributes args)
          core-component (:core-component context)
          query-params (append-question-to-keys args)
          query {relationship-name query-params}]
      (eval-patterns core-component query context))))

(defn create-entity-resolver
  [entity-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          create-pattern {entity-name args}]
      (first (eval-patterns core-component create-pattern context)))))

(defn create-contained-entity-resolver
  [relationship-name parent-name child-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          schema (schema-info core-component)
          parent-guid (find-guid-or-id-attribute schema parent-name)
          parent-guid-arg-pair (get-combined-key-value args parent-name parent-guid)
          [parent-guid-attribute parent-guid-value] (first (seq parent-guid-arg-pair))
          child-params (dissoc args parent-guid-attribute)
          fetch-parent-query {parent-name {(append-question-mark parent-guid) parent-guid-value}}
          create-child-query {child-name child-params
                              :-> [[relationship-name fetch-parent-query]]}
          results (eval-patterns core-component create-child-query context)]
       (assoc results parent-guid-attribute parent-guid-value))))

(defn create-between-relationship-resolver
  [relationship-name entity1-name entity2-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          create-pattern {relationship-name args}]
      (first (eval-patterns core-component create-pattern context)))))

(defn update-entity-resolver
  [entity-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          schema (schema-info core-component)
          entity-guid-attr (find-guid-or-id-attribute schema entity-name)
          entity-guid-val (entity-guid-attr args)
          update-pattern {(form-pattern-name core-component "Update" (extract-entity-name entity-name))
                            {entity-guid-attr entity-guid-val
                             :Data (dissoc args entity-guid-attr)}}]
      (first (eval-patterns core-component update-pattern context)))))

(defn update-contained-entity-resolver
  [relationship-name parent-name child-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          schema (schema-info core-component)
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
              results (eval-patterns core-component update-pattern context)]
          (assoc (first results) parent-guid-attribute parent-guid-value))))))

(defn update-between-relationship-resolver
  [relationship-name entity1-name entity2-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          schema (schema-info core-component)
          extracted-entity1-name (keyword (extract-entity-name entity1-name))
          extracted-entity2-name (keyword (extract-entity-name entity2-name))
          entity1-guid-value (extracted-entity1-name args)
          entity2-guid-value (extracted-entity2-name args)]

      (when-not entity1-guid-value
        (throw (Exception.
                 (str "Error: GUID for '" entity1-name "' not provided. It is needed to update the associated entity."))))
      (when-not entity2-guid-value
        (throw (Exception.
                 (str "Error: GUID for '" entity2-name "' not provided. It is needed to update the associated entity."))))

      (let [query-params {(append-question-mark extracted-entity1-name) entity1-guid-value
                          (append-question-mark extracted-entity2-name) entity2-guid-value}
            relationship-params (dissoc args extracted-entity1-name extracted-entity2-name)
            update-pattern {relationship-name
                            (merge query-params relationship-params)}]
        (first (eval-patterns core-component update-pattern context))))))


(defn delete-entity-resolver
  [entity-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          schema (schema-info core-component)
          entity-guid (find-guid-or-id-attribute schema entity-name)
          delete-pattern [[:delete entity-name {entity-guid (entity-guid args)}]]]
      (eval-patterns core-component delete-pattern context))))

(defn delete-contained-entity-resolver
  [relationship-name parent-name child-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          schema (schema-info core-component)
          parent-guid (find-guid-or-id-attribute schema parent-name)
          parent-guid-arg-pair (get-combined-key-value args parent-name parent-guid)
          [parent-guid-attribute parent-guid-value] (first (seq parent-guid-arg-pair))
          child-guid (find-guid-or-id-attribute schema child-name)
          child-params (dissoc args parent-guid-attribute)]
      (when (or (nil? parent-guid-attribute) (nil? parent-guid-value))
        (throw (Exception. (str "Error: " (extract-entity-name parent-name) (name parent-guid) " not provided for " parent-name
                                ". It is needed to identify the parent entity."))))
      (let [query-children-pattern {child-name (append-question-to-keys child-params)
                                    :-> [[(append-question-mark relationship-name)
                                          {parent-name {(append-question-mark parent-guid)
                                                        parent-guid-value}}]] :as :Cs}
            delete-children-pattern [:for-each :Cs
                                       [:delete child-name {child-guid (keyword (str "%." (name child-guid)))}]]
            results (flatten (eval-patterns core-component [query-children-pattern delete-children-pattern] context))]
        (if (and (seq results) (map? (first results))) ;; map isn't returned when no records are deleted
          ;; we received guid of parent, thus returning
          (vec (map #(assoc % parent-guid-attribute parent-guid-value) results))
          {})))))

(defn delete-between-relationship-resolver
  [relationship-name entity1-name entity2-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          create-pattern [[:delete relationship-name args]]]
      (eval-patterns core-component create-pattern context))))

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
                                                          relation-name (name (last (str/split (name rel-key) #"\.")))]
                                                      (assoc acc
                                                             (keyword (str "Mutation/" mutation-type relation-name))
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

(defn remove-child-entities [schema contains-graph]
  (let [children (set (mapcat val contains-graph))
        filtered-entities (vec (filter
                                (fn [entity-map]
                                  (let [entity-key (keyword (name (first (keys entity-map))))]
                                    (not (children entity-key))))
                                (:entities schema)))]
    (assoc schema :entities filtered-entities)))

(defn generate-resolver-map [schema contains-graph]
  (let [schema-without-children (remove-child-entities schema contains-graph)
        query-resolvers (generate-query-resolver-map schema)
        create-mutation-resolvers (generate-mutation-resolvers schema-without-children "Create")
        update-mutation-resolvers (generate-mutation-resolvers schema-without-children "Update")
        delete-mutation-resolvers (generate-mutation-resolvers schema-without-children "Delete")]
    (merge query-resolvers create-mutation-resolvers update-mutation-resolvers delete-mutation-resolvers)))