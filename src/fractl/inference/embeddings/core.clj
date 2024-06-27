(ns fractl.inference.embeddings.core
  (:require [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.inference.embeddings.internal.generator :as g]
            [fractl.inference.embeddings.internal.queue :as queue]
            [fractl.inference.embeddings.internal.registry :as r]
            [fractl.inference.embeddings.pgvector]
            [fractl.inference.embeddings.protocol :as p]))

(declare embed-schema)

(defn init [config]
  (if-let [connector (r/fetch-db-connector (:vectordb config))]
    (queue/process (partial embed-schema (connector (:config config))))
    (u/throw-ex (str "Unsupported embbeddings database type: " (:vectordb config))))
  config)

(defn- rearrange-data [data-edn]
  (map (fn [[tool-name tool-spec]]
         {:tool-name tool-name
          :tool-spec tool-spec}) data-edn))

(defn- send-data-for-embedding [tool-seq db schema-obj]
  (mapv #(p/embed-tool db (merge schema-obj %)) tool-seq))

(defn embed-schema [db {app-uuid :app-uuid operation :operation
                        tag :tag type :type schema :schema :as obj}]
  (when-not db (u/throw-ex "Embedding-db not initialized"))
  (let [app-uuid (or app-uuid (u/get-app-uuid))
        meta-content-data (pr-str (symbol (str "(" tag " " type " " (str schema) ")")))
        schema-obj (assoc obj :meta-content meta-content-data)]
    (log/info (str "embedding schema: " [operation tag type]))
    (if (= tag 'component)
      (p/embed-tool db schema-obj)
      (-> (g/generate-tool-for-data tag type schema)
          (rearrange-data)
          (send-data-for-embedding db schema-obj)))))
