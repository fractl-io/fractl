(ns fractl.inference.embeddings.protocol)

(defprotocol EmbeddingDb
  "The interface for all embedding stores."
  (open-connection [db config])
  (close-connection [db])
  (embed-tool [db spec])
  (update-tool [db spec])
  (delete-tool [db spec])
  (embed-document-chunk [db app-uuid text-chunk])
  (get-document-classname [db app-uuid])
  (get-planner-classname [db app-uuid])
  (find-similar-objects [db query-spec limit]))
