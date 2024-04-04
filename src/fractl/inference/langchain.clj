(ns fractl.inference.langchain
  (:require [fractl.util :as u])
  (:import [java.time Duration]
           [java.util List ArrayList]
           [dev.langchain4j.model.openai OpenAiChatModel]
           [dev.langchain4j.data.message AiMessage UserMessage TextContent]))

(defn make-openai-chatmodel [props]
  (let [builder (OpenAiChatModel/builder)]
    (. builder apiKey (:apiKey props))
    ;; TODO: set optional properties
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
