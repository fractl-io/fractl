(ns fractl.inference.embeddings.pgvec
  (:require [fractl.util :as u]
            [fractl.inference.embeddings.protocol :as p]
            [fractl.inference.embeddings.pgvector :as pgv]))

(defn make []
  (let [db-conn (u/make-cell)]
    (reify p/EmbeddingDb
      (open-connection [this config]
        (u/safe-set-once db-conn (pgv/open-connection config))
        this)
      (close-connection [_]
        (when (pgv/close-connection @db-conn)
          (u/safe-set-once db-conn nil)
          true))
      (embed-tool [_ spec]
        (pgv/embed-planner-tool @db-conn spec)))))

(def fetch-db
  (memoize
   #(let [db (make)]
      (when (p/open-connection
             db {:host (u/getenv "PGVECTOR_DB_HOST")
                 :port (u/getenv "PGVECTOR_DB_PORT")
                 :dbname (u/getenv "PGVECTOR_DB_NAME")
                 :user (u/getenv "PGVECTOR_DB_USERNAME")
                 :password (u/getenv "PGVECTOR_DB_PASSWORD")})
        db))))
