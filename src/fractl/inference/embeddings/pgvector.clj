(ns fractl.inference.embeddings.pgvector
  (:require [fractl.util :as u]
            [fractl.inference.embeddings.protocol :as p]
            [fractl.inference.embeddings.internal.registry :as r]
            [fractl.inference.embeddings.internal.pgvector :as pgv]))

;;;; sample config.edn entry:
;; {:publish-schema {:vectordb :pgvector
;;                   :config {:host #$ [PGVECTOR_DB_HOST "localhost"]
;;                            :port #$ [PGVECTOR_DB_PORT 5432]
;;                            :dbname #$ [PGVECTOR_DB_NAME "postgres"]
;;                            :user #$ [PGVECTOR_DB_USERNAME "postgres"]
;;                            :password #$ [PGVECTOR_DB_PASSWORD "postgres"]}}}

(defn make []
  (let [db-conn (u/make-cell)]
    (reify p/EmbeddingDb
      (open-connection [this config]
        (u/safe-set-once db-conn #(pgv/open-connection config))
        this)
      (close-connection [_]
        (when (pgv/close-connection @db-conn)
          (u/safe-set db-conn nil)
          true))
      (embed-tool [_ spec]
        (pgv/embed-planner-tool @db-conn spec)))))

(def fetch-db
  (memoize
   (fn [config]
     (let [db (make)]
      (when (p/open-connection db config)
        db)))))

(r/register-db :pgvector fetch-db)
