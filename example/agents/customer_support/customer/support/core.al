(component :Customer.Support.Core)

;; Note: the following settings should be in config.edn
;; { ; ...
;;  :inference-service-enabled true
;;  :publish-schema {:vectordb :pgvector
;;                   :config {:llm-provider "llm01"
;;                            :host #$ [PGVECTOR_DB_HOST "localhost"]
;;                            :port #$ [PGVECTOR_DB_PORT 5432]
;;                            :dbname #$ [PGVECTOR_DB_NAME "postgres"]
;;                            :user #$ [PGVECTOR_DB_USERNAME "postgres"]
;;                            :password #$ [PGVECTOR_DB_PASSWORD "postgres"]}}}

{:Agentlang.Core/LLM
 {:Type "openai"
  :Name "llm01"
  :Config {:ApiKey (agentlang.util/getenv "OPENAI_API_KEY")
           :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
           :EmbeddingModel "text-embedding-3-small"
           :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
           :CompletionModel "gpt-3.5-turbo"}}}

(def technical-support-agent "technical-support-agent")

{:Agentlang.Core/Agent
 {:Name technical-support-agent
  :Type "chat"
  :LLM "llm01"
  :Chat {:Messages
         [{:role :system
           :content (str "You are a support agent for a Camera store. "
                         "You are supposed to handle technical queries that customers ask on camera gear. "
                         "Please use the documentation from the appropriate "
                         "camera manufacturer to answer these queries. "
                         "If you get a query on the pricing of camera gear, respond with the text: NA")}]}}}

(def price-enquiry-agent "price-enquiry-agent")

{:Agentlang.Core/Agent
 {:Name price-enquiry-agent
  :Type "chat"
  :LLM "llm01"
  :Chat {:Messages
         [{:role :system
           :content (str "You are a support agent for a Camera store. "
                         "Customers will raise price enquiries for camera gear. "
                         "Please use the price-list from the appropriate camera "
                         "manufacturer to answer the query. If you get a technical question, "
                         "please respond with the simple text: NA")}]}}}

;;;; If not provided in config, documents maybe attached to an agent as the `:Documents` attribute:
;; {
;;  :Documents
;;   [{:Title "ABC Price List"
;;     :Uri "file://./docs/abc_prices.txt"
;;     :Agent price-enquiry-agent}
;;    {:Title  "XYZ Price List"
;;     :Uri "file://./docs/xyz_prices.txt"
;;     :Agent price-enquiry-agent}]}

{:Agentlang.Core/Agent
 {:Name "camera-support-agent"
  :Type "chat"
  :LLM "llm01"
  :Chat
  {:Messages
   [{:role :system
     :content (str "You are an agent that classifies a customer query into two categories - either "
                   "\"agent: technical-support-agent\" or \"agent: price-enquiry-agent\". "
                   "Analyse the user query and return only one of those strings.")}]}
  :Delegates
  [{:To technical-support-agent}
   {:To price-enquiry-agent}]}}

;; Usage:
;; POST api/Customer.Support.Core/CameraStore
;; {"Customer.Support.Core/CameraStore": {"UserInstruction": "What's the price of Panasonic G9?"}}
(inference :CameraStore {:agent "camera-support-agent"})
