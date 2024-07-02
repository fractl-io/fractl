(ns fractl.inference.provider.openai
  (:require [cheshire.core :as json]
            [org.httpkit.client :as http]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.inference.util :as util]))

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

(def openai-completion-api-endpoint  "https://api.openai.com/v1/chat/completions")
(def openai-default-completion-model "gpt-3.5-turbo")
(def default-temperature 0)
(def default-max-tokens 500)

(def args-validator-make-openai-completion
  (util/make-validator-explainer
    [:map
     [:messages [:vector [:map
                          [:role [:enum :system :user :assistant]]
                          [:content :string]]]]
     [:model-name {:default openai-default-completion-model} :string]
     [:openai-api-key {:optional true} :string]
     [:completion-endpoint {:default openai-completion-api-endpoint} :string]
     [:temperature {:default default-temperature} :int]
     [:max-tokens {:default default-max-tokens} :int]]))

(defn make-openai-completion [options]
  (let [{:keys [messages
                model-name
                openai-api-key
                completion-endpoint
                temperature
                max-tokens
                ]} (args-validator-make-openai-completion options)
        openai-api-key (or openai-api-key
                           (get-env-openai-api-key))
        options {:headers {"Content-type"  "application/json"
                           "Authorization" (str "Bearer " openai-api-key)}
                 :body (json/generate-string {:model model-name
                                              :messages messages
                                              :temperature temperature
                                              :max_tokens max-tokens})}
        response @(http/request completion-endpoint options)]
    (-> (:body response)
        (json/parse-string)
        (get-in ["choices" 0 "message" "content"]))))
