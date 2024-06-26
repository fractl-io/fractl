(ns fractl.inference.embeddings.provider.openai
  (:require [cheshire.core :as json]
            [org.httpkit.client :as http]
            [fractl.util :as u]
            [fractl.util.logger :as log]))

(def ^:private openai-key-env-var "OPENAI_API_KEY")
(def ^:private env-openai-api-key (System/getenv openai-key-env-var))

(defn- get-env-openai-api-key []
  (or env-openai-api-key
      (u/throw-ex
       (str "OpenAI API Key not specified, nor env var "
            openai-key-env-var " is set"))))

(def ^:private openai-embedding-api-endpoint  "https://api.openai.com/v1/embeddings")
(def openai-default-embedding-model "text-embedding-3-small")
;; -- OpenAI embedding models
;-- +------------------------+-----------------+---------+
;-- | OpenAI Embedding Model | Dimensions      | Remarks |
;-- +------------------------+-----------------+---------+
;-- | text-embedding-3-small | 512, 1536       |         |
;-- | text-embedding-3-large | 256, 1024, 3072 |         |
;-- | text-embedding-ada-002 | 1536            | Older   |
;-- +------------------------+-----------------+---------+

(defn make-openai-embedding [{text-content :text_content model-name :model_name
                              openai-api-key :openai_api_key embedding-endpoint :embedding_endpoint}]
  (let [model-name (or model-name openai-default-embedding-model)
        embedding-endpoint (or embedding-endpoint openai-embedding-api-endpoint)
        openai-api-key (or openai-api-key (get-env-openai-api-key))
        options {:headers {"Authorization" (str "Bearer " openai-api-key)
                           "Content-Type" "application/json"}
                 :body (json/generate-string {"input" text-content
                                              "model" model-name
                                              "encoding_format" "float"})}
        response @(http/post embedding-endpoint options)
        status (:status response)]
    (if (<= 200 status 299)
      (or (-> (:body response)
              json/parse-string
              (get-in ["data" 0 "embedding"]))
          (do
            (log/error
             (u/pretty-str
              (format "Failed to extract OpenAI embedding (status %s):" status)
              response))
            nil))
      (do
        (log/error
         (u/pretty-str
          (format "Failed to generate OpenAI embedding (status %s):" status)
          response))
        nil))))
