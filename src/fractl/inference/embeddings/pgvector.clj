(ns fractl.inference.embeddings.pgvector
  (:import (org.postgresql.util PGobject PSQLException)
           (com.pgvector PGvector))
  (:require [clojure.string :as s]
            [next.jdbc :as jdbc]
            [cheshire.core :as json]
            [fractl.util :as u]
            [fractl.inference.embeddings.model :as model]
            [fractl.inference.embeddings.provider.openai :as openai]))

(defn open-connection [config]
  (merge {:dbtype "postgresql"} config))

(defn close-connection [db-conn]
  (when (= "postgresql" (:dbtype db-conn))
    true))

(defn- pg-floats
  "Turn supplied collection of floating-point values into a PostgreSQL
  PGVector object suitable for use as SQL param."
  [float-coll]
  (-> float-coll
      float-array
      (PGvector.)))

(defn- pg-json
  "Turn supplied JSON string into a PostgreSQL PGobject
  object suitable for use as SQL param."
  [json-string]
  (doto (PGobject.)
    (.setType "json")
    (.setValue json-string)))

(defn get-entities-classname [app-uuid]
  (str "EntitySchema_" (s/replace app-uuid "-" "")))

(defn get-document-classname [app-uuid]
  (str "KnowledgeDoc_" (s/replace app-uuid "-" "")))

(defn get-planner-classname [app-uuid]
  (str "PlannerTools_" (s/replace app-uuid "-" "")))

(def ^:private delete-all-sql
  "DELETE
  FROM text_embedding
  WHERE embedding_classname = ?")

(defn- delete-all [db-conn classname]
  (jdbc/execute! db-conn [delete-all-sql classname]))

(def ^:private delete-selected-sql
  "DELETE
  FROM text_embedding
  WHERE embedding_classname = ? AND meta_content -> ? ->> 'type' = ?")

(defn delete-selected [db-conn app-uuid tag type]
  (let [embedding-classname (get-planner-classname app-uuid)]
    (jdbc/execute! db-conn [delete-selected-sql
                            embedding-classname
                            tag
                            type])))

(defn- assert-object! [obj]
  (when-not (model/object? obj)
    (u/throw-ex (str "Invalid embedding object: " obj))))

(def ^:private create-object-sql-template
  "INSERT
  INTO text_embedding (
    embedding_classname,
    text_content,
    meta_content,
    embedding_model,
    embedding_%d
  ) VALUES (
    ?, ?, ?::json, ?, ?
  )")

(defn create-object [db-conn {classname :classname text-content :text_content
                              meta-content :meta_content embedding :embedding
                              embedding-model :embedding_model :as obj}]
  (assert-object! obj)
  (let [create-object-sql (format create-object-sql-template (count embedding))]
    (jdbc/execute! db-conn [create-object-sql
                            classname
                            text-content
                            (pg-json meta-content)
                            embedding-model
                            (pg-floats embedding)])))

(def ^:private find-similar-objects-sql-template
  "SELECT
    text_content,
    (embedding_%d <-> ?) AS euclidean_distance,
    -1 * (embedding_%d <#> ?) AS inner_product,
    1 - (embedding_%d <=> ?) AS cosine_similarity
  FROM text_embedding
  WHERE embedding_classname = ?
  ORDER BY euclidean_distance
  LIMIT ?")

(defn find-similar-objects [db-conn {classname :classname embedding :embedding :as obj} limit]
  (assert-object! obj)
  (let [embedding-sql-param (pg-floats embedding)
        dimension-count (count embedding)
        find-similar-objects-sql (format find-similar-objects-sql-template
                                         dimension-count
                                         dimension-count
                                         dimension-count)]
    (->> [find-similar-objects-sql
          embedding-sql-param
          embedding-sql-param
          embedding-sql-param
          classname
          limit]
         (jdbc/execute! db-conn)
         (mapv :text_embedding/text_content))))

(defn delete-planner-tool [db-conn app-uuid tag type]
  (delete-selected db-conn app-uuid tag type))

(defn- form-to-json [clj-form-str]
  (let [data-list (clojure.edn/read-string clj-form-str)
        data-key (second data-list)
        entity-type (first data-list)
        attributes (nth data-list 2)
        process (fn [v]
                  (cond
                    (map? v) (into {}
                                   (for [[k v] v]
                                     [(name k)
                                      (if (keyword? v)
                                        {:type (name (second (s/split (str v) #"\:")))}
                                        v)]))
                    (keyword? v) {:type (name (second (s/split (str v) #"\:")))}
                    (string? v) {:type v}))]
    (json/generate-string
     {(name entity-type)
      {"type" data-key
       "attributes" (into {} (map (fn [[k v]] [(name k) (process v)]) attributes))}})))

(defn add-planner-tool [db-conn app-uuid tool-spec meta-content]
  (let [document-classname (get-planner-classname app-uuid)
        tool-text (pr-str tool-spec)
        embedding (openai/make-openai-embedding {:text_content tool-text})]
    (create-object (model/as-object {:classname document-classname
                                     :text_content tool-text
                                     :meta_content (form-to-json meta-content)
                                     :embedding embedding}))))

(defn update-planner-tool [db-conn app-uuid tool-spec meta-content tag type]
  (delete-planner-tool db-conn app-uuid tag type)
  (add-planner-tool db-conn app-uuid tool-spec meta-content))
