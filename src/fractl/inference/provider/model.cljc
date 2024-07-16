(ns fractl.inference.provider.model
  (:require [fractl.lang :refer [component
                                 entity
                                 dataflow
                                 resolver]]
            [fractl.util :as u]))

(component :Fractl.Inference.Provider)

(entity
 :Fractl.Inference.Provider/LLM
 {:Type :Keyword ; e.g :openai
  :Name {:type :String :guid true}
  :Config :Map
  ;; example config for openai:
  ;; {:ApiKey (fractl.util/getenv "OPENAI_API_KEY")
  ;;  :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
  ;;  :EmbeddingModel "text-embedding-3-small"
  ;;  :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
  ;;  :CompletionModel "gpt-3.5-turbo"}
  })

(dataflow
 :Fractl.Inference.Provider/FindLLM
 {:Fractl.Inference.Provider/LLM
  {:Name? :Fractl.Inference.Provider/FindLLM.Name}})

(def ^:private llm-registry (atom nil))

(defn- upsert-llm [inst]
  (swap! llm-registry assoc (u/string-as-keyword (:Type inst)) inst)
  inst)

(defn- lookup-llm-by-name [[opr attr attrval]]
  (when (and (= := opr) (= attr :Name))
    (vec (filter #(= attrval (:Name %)) (vals @llm-registry)))))

(resolver
 :Fractl.Inference.Provider/LLMResolver
 {:paths [:Fractl.Inference.Provider/LLM]
  :with-methods
  {:create upsert-llm
   :delete (fn [inst]
             (swap! llm-registry dissoc (u/string-as-keyword (:Type inst)))
             inst)
   :query (fn [[_ {clause :where} :as param]]
            (if (= :* clause)
              (vals @llm-registry)
              (lookup-llm-by-name clause)))}})
