(ns fractl.inference.provider.openai
  (:require [cheshire.core :as json]
            [org.httpkit.client :as http]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.inference.provider.model :as model]
            [fractl.inference.provider.protocol :as p]
            [fractl.inference.provider.registry :as r]))

(defn- fetch-openai-config [] (:OpenAiConfig (model/fetch-config)))

;; -- OpenAI embedding models
;-- +------------------------+-----------------+---------+
;-- | OpenAI Embedding Model | Dimensions      | Remarks |
;-- +------------------------+-----------------+---------+
;-- | text-embedding-3-small | 512, 1536       |         |
;-- | text-embedding-3-large | 256, 1024, 3072 |         |
;-- | text-embedding-ada-002 | 1536            | Older   |
;-- +------------------------+-----------------+---------+

(defn make-openai-embedding [{text-content :text-content
                              model-name :model-name
                              openai-api-key :openai-api-key
                              embedding-endpoint :embedding-endpoint :as args}]
  (let [openai-config (fetch-openai-config)
        model-name (or model-name (:EmbeddingModel openai-config))
        embedding-endpoint (or embedding-endpoint (:EmbeddingApiEndpoint openai-config))
        openai-api-key (or openai-api-key (:ApiKey openai-config))
        options {:headers {"Authorization" (str "Bearer " openai-api-key)
                           "Content-Type" "application/json"}
                 :body (json/generate-string {"input" text-content
                                              "model" model-name
                                              "encoding_format" "float"})}
        response @(http/post embedding-endpoint options)
        status (:status response)]
    (if (<= 200 status 299)
      (or (when-let [r (-> (:body response)
                           json/parse-string
                           (get-in ["data" 0 "embedding"]))]
            [r model-name])
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

(def ^:private default-temperature 0)
(def ^:private default-max-tokens 500)

(defn- assert-message! [message]
  (when-not (and (map? message)
                 (some #{(:role message)} #{:system :user :assistant})
                 (string? (:content message)))
    (u/throw-ex (str "invalid message: " message))))

(defn make-openai-completion [{messages :messages
                               model-name :model-name
                               openai-api-key :openai-api-key
                               completion-endpoint :completion-endpoint
                               temperature :temperature
                               max-tokens :max-tokens}]
  (doseq [m messages] (assert-message! m))
  (let [openai-config (fetch-openai-config)
        model-name (or model-name (:CompletionModel openai-config))
        completion-endpoint (or completion-endpoint (:CompletionApiEndpoint openai-config))
        temperature (or temperature default-temperature)
        max-tokens (or max-tokens default-max-tokens)
        openai-api-key (or openai-api-key (:ApiKey openai-config))
        options {:headers {"Content-type"  "application/json"
                           "Authorization" (str "Bearer " openai-api-key)}
                 :body (json/generate-string {:model model-name
                                              :messages messages
                                              :temperature temperature
                                              :max_tokens max-tokens})}
        response @(http/post completion-endpoint options)]
    [(-> (:body response)
         (json/parse-string)
         (get-in ["choices" 0 "message" "content"]))
     model-name]))

(r/register-provider
 :openai
 (reify p/AiProvider
   (make-embedding [_ spec]
     (make-openai-embedding spec))
   (make-completion [_ spec]
     (make-openai-completion spec))))
