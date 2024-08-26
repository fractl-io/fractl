(ns agentlang.inference.provider.model
  (:require [agentlang.lang :refer [component
                                    entity
                                    dataflow
                                    resolver]]
            [agentlang.util.seq :as us]))

(component :Agentlang.Inference.Provider)

(entity
 :Agentlang.Inference.Provider/LLM
 {:Type {:type :String :default "openai"} ; e.g "openai"
  :Name {:type :String :guid true :default #(us/generate-code 5)}
  :Config {:type :Map :optional true}
  ;; example config for openai:
  ;; {:ApiKey (agentlang.util/getenv "OPENAI_API_KEY")
  ;;  :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
  ;;  :EmbeddingModel "text-embedding-3-small"
  ;;  :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
  ;;  :CompletionModel "gpt-3.5-turbo"}
  })

(dataflow
 :Agentlang.Inference.Provider/FindLLM
 {:Agentlang.Inference.Provider/LLM
  {:Name? :Agentlang.Inference.Provider/FindLLM.Name}})

(def ^:private llm-registry (atom nil))

(defn- upsert-llm [inst]
  (swap! llm-registry assoc (:Name inst) inst)
  inst)

(defn- lookup-llm-by-name [[opr attr attrval]]
  (when (and (= := opr) (= attr :Name))
    (when-let [r (get @llm-registry attrval)]
      [r])))
