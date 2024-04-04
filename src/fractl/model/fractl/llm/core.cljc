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
(record :Fractl.Llm.Core/ChatLanguageModel {:Id :Identity})
(entity
 :Fractl.Llm.Core/OpenAiChatModel
 {:meta {:inherits :Fractl.Llm.Core/ChatLanguageModel},
  :timeoutInSeconds {:type :Int, :optional true},
  :presencePenalty {:type :Double, :optional true},
  :apiKey {:type :String, :optional true},
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
(record :Fractl.Llm.Core/ChatMessage {})
(record
 :Fractl.Llm.Core/UserMessage
 {:meta {:inherits :Fractl.Llm.Core/ChatMessage},
  :Content {:listof :String}})
(record
 :Fractl.Llm.Core/AiMessage
 {:meta {:inherits :Fractl.Llm.Core/ChatMessage}, :Text :String})
(event
 :Fractl.Llm.Core/Generate
 {:Model :Fractl.Llm.Core/ChatLanguageModel,
  :Messages {:listof :Fractl.Llm.Core/ChatMessage}})
(def
 Fractl_Llm_Core___COMPONENT_ID__
 "a4aebb4f-e1ec-4ccd-b5ea-d07a4b4aa5a5")
