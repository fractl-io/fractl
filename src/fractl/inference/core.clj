(ns fractl.inference.core
  (:import [java.time Duration]
           [dev.langchain4j.model.openai OpenAiChatModel]))

(defn make-openai-chatmodel [api-key model-name]
  (-> (OpenAiChatModel/builder)
      (.apiKey api-key)
      (.modelName model-name)
      (.temperature 1.0)
      (.timeout (Duration/ofSeconds 60))
      (.frequencyPenalty 0.0)
      (.maxTokens (int 5))
      (.maxRetries (int 3))
      (.presencePenalty 0.0)
      (.seed (int 1))
      (.stop nil)
      (.topP 1.0)
      (.user nil)
      (.logRequests true)
      (.logResponses true)
      (.build)))
