(ns fractl.inference.langchain
  (:require [fractl.util :as u :refer [.i]])
  (:import [java.time Duration]
           [dev.langchain4j.model.openai OpenAiChatModel]))

(defn make-openai-chatmodel [props]
  (let [builder (OpenAiChatModel/builder)]
    (doseq [k (keys props)]
      (.i k builder (k props)))
    (.build builder)))
