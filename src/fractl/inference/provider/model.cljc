(ns fractl.inference.provider.model
  (:require [fractl.lang :refer [component
                                 record
                                 entity
                                 dataflow]]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]))

(cn/register-model
 :fractl.inference.provider
 {:config-entity :Fractl.Inference.Provider/Config})

(component :Fractl.Inference.Provider)

;; Example entry in config.edn:
;; {:Fractl.Inference.Provider/Config {:OpenAiConfig {:ApiKey #$ OPENAI_API_KEY}}}
(entity
 :Fractl.Inference.Provider/Config
 {:meta {:inherits :Fractl.Kernel.Lang/Config}
  :Provider {:oneof [:openai]
             :default :openai}
  :OpenAiConfig {:type :Map
                 :parse (partial cn/maybe-make-instance :Fractl.Inference.Provider/OpenAiConfig)
                 :optional true}})

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

(defn verify-config [inst]
  (when (= (u/string-as-keyword (:Provider inst)) :openai)
    (when-not (:OpenAiConfig inst)
      (u/throw-ex "openai configuration is not set")))
  inst)

(dataflow
 [:before :create :Fractl.Inference.Provider/Config]
 [:eval '(fractl.inference.provider.model/verify-config :Instance)])

(def fetch-config
  (memoize
   #(ev/fetch-model-config-instance :fractl.inference.provider)))
