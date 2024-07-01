(ns fractl.inference.embeddings.protocol)

(defprotocol EmbeddingDb
  "The interface for all embedding stores."
  (open-connection [db config])
  (close-connection [db])
  (embed-tool [db spec]))
