(ns fractl.inference.provider.model
  (:require [fractl.lang :refer [component
                                 record
                                 entity
                                 dataflow]]
            [fractl.util :as u]
            [fractl.component :as cn]))

(cn/register-model
 :fractl.inference.provider
 {:config-entity :Fractl.Inference.Provider/Config})

(component :Fractl.Inference.Provider)

(record
 :Fractl.Inference.Provider/OpenAiConfig
 {:ApiKey :String
  :EmbeddingApiEndpoint {:check u/url?
                         :default "https://api.openai.com/v1/embeddings"}
  :EmbeddingModel {:type :String
                   :default "text-embedding-3-small"}
  :CompletionApiEndpoint {:check u/url?
                          :default "https://api.openai.com/v1/chat/completions"}
  :CompletionModel {:type :String
                    :default "gpt-3.5-turbo"}})

(entity
 :Fractl.Inference.Provider/Config
 {:Provider {:oneof [:openai]
             :default :openai}
  :OpenAiConfig {:type :Fractl.Inference.Provider/OpenAiConfig
                 :optional true}})

(defn verify-config [config]
  (when (= (:Provider config) :openai)
    (when-not (cn/instance-of? :Fractl.Inference.Provider/OpenAiConfig (:OpenAiConfig config) true)
      (u/throw-ex "OpenAiConfig is required")))
  config)

(dataflow
 [:after :create :Fractl.Inference.Provider/Config]
 [:eval '(fractl.inference.provider.model/verify-config :Instance)])
