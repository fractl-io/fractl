(ns fractl.inference.langchain
  (:require [fractl.util :as u])
  (:import [java.time Duration]
           [java.util List ArrayList]
           [dev.langchain4j.model.openai OpenAiChatModel]
           [dev.langchain4j.data.message AiMessage UserMessage TextContent]))

(defn make-openai-chatmodel [props]
  (let [builder (OpenAiChatModel/builder)]
    (. builder apiKey (:apiKey props))
    (. builder modelName (or (:modelName props) "gpt-3.5-turbo"))
    (when-let [t (:temperature props)] (. builder temperature t))
    (when-let [t (:timeoutInSeconds props)] (. builder timeoutInSeconds t))
    (when-let [fp (:frequencyPenalty props)] (. builder frequencyPenalty fp))
    (when-let [mtk (:maxTokens props)] (. builder maxTokens mtk))
    (when-let [mr (:maxRetries props)] (. builder maxRetries mr))
    (when-let [pp (:presencePenalty props)] (. builder presencePenalty pp))
    (when-let [s (:seed props)] (. builder seed s))
    (when-let [s (:stop props)] (. builder stop s))
    (when-let [tp (:topP props)] (. builder topP tp))
    (when-let [u (:user props)] (. builder user u))
    (when-not (nil? (:logRequests props)) (. builder logRequests (:logRequests props)))
    (when-not (nil? (:logResponses props)) (. builder logResponses (:logResponses props)))
    (.build builder)))

(defn- ^UserMessage make-user-message [content]
  (let [txt-conts (mapv #(TextContent/from %) content)]
    (UserMessage. (ArrayList. txt-conts))))

(defn- ^AiMessage make-ai-message [^String content]
  (AiMessage. content))

(defn- as-l4j-messages [msgs]
  (let [result (mapv (fn [[tag cont]]
                       (case tag
                         :user (make-user-message cont)
                         :ai (make-ai-message cont)))
                     msgs)]
    (ArrayList. result)))

(defn openai-generate [^OpenAiChatModel model msgs]
  (let [^List list (as-l4j-messages msgs)
        ^AiMessage ai-msg (.generate model list)]
    (.text ai-msg)))
