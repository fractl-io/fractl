(component :Sfdcdemo.Core
  {:clj-import (quote [(:require [camel.sfdc.resolver.model])
                       (:use [camel.googlesheets.resolver.model])])})

(entity
  :Lead
  {:Company :String
   :LastName :String})

{:Agentlang.Core/LLM
 {:Type "openai"
  :Name "llm01"
  :Config {:ApiKey (agentlang.util/getenv "OPENAI_API_KEY")
           :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
           :EmbeddingModel "text-embedding-3-small"
           :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
           :CompletionModel "gpt-3.5-turbo"}}}

{:Agentlang.Core/Agent
 {:Name "sfdc-agent"
  :Type "planner"
  :Tools [{:name "Camel.Googlesheets.Resolver.Core/Spreadsheet"}]
  :UserInstruction (str "You are an agent who convert lead instances to google-sheet entries. "
                        "Use the company-name as title for the spreadsheet. "
                        "You can set the instance as the value of the scheet's \"data\" parameter. "
                        "Always set the \"data\" parameter.")
  :LLM "llm01"}}

(inference :OnNewLead  {:agent "sfdc-agent"})

(dataflow
 [:after :create :Lead]
 [:eval '(println "Lead created: " :Instance)]
 {:OnNewLead {:UserInstruction '(str "new lead instance: " :Instance)}})

(resolver
 :Salesforce/Resolver
 {:type :Camel.Sfdc.Resolver.Core/Resolver :paths [:Sfdcdemo.Core/Lead]})
