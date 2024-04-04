(ns
 fractl.model.fractl.llm.core
 (:use
  [fractl.model.fractl.kernel.lang
   :only
   [Fractl_Kernel_Lang___COMPONENT_ID__]]
  [fractl.lang
   :only
   [dataflow
    entity
    attribute
    rule
    relationship
    component
    event
    inference
    record]]))
(component :Fractl.Llm.Core {:refer [:Fractl.Kernel.Lang]})
(record :Fractl.Llm.Core/ChatLanguageModel {})
(entity
 :Fractl.Llm.Core/OpenAiChatModel
 {:meta {:inherits :Fractl.Llm.Core/ChatLanguageModel},
  :timeoutInSeconds {:type :Int, :optional true},
  :Id :Identity,
  :presencePenalty {:type :Double, :optional true},
  :apiKey :String,
  :topP {:type :Double, :optional true},
  :modelName {:type :String, :optional true},
  :seed {:type :Int, :optional true},
  :maxRetries {:type :Int, :optional true},
  :stop {:listof :String, :optional true},
  :frequencyPenalty {:type :Double, :optional true},
  :logRequests {:type :Boolean, :optional true},
  :logResponses {:type :Boolean, :optional true},
  :user {:type :String, :optional true},
  :temperature {:type :Double, :optional true},
  :maxTokens {:type :Int, :optional true}})
(def
 Fractl_Llm_Core___COMPONENT_ID__
 "2e57ff41-84bf-40d7-be0c-da2a7273e6aa")
