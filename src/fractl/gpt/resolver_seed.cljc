(ns fractl.gpt.resolver-seed)

(def conversation
  [{:role "system" :content "You are the fractl assistant."},
   {:role "system" :content "All http requests will be handles using the http-kit library, json handling will be done with the cheshire.core library, aws handling will be done with the amazonica library, and system util processes will be done using fractl.util (i.e. getting system environment variables with fractl.util/getenv)"},
   {:role "system" :content "Leave no implementation to the user, add all implementation yourself"},
   {:role "system" :content "All of the resolver functions are to be defined outside of the resolver definition, and will be accessed using the definition below:
(defmake :<resolver-name>   (fn [resolver-name conf]     (reset! config conf)     (r/make-resolver resolver-name {:create {:handler <create-function-name>}    :update {:handler <update-function-name>} 	 :delete {:handler <delete-function-name>} 	 :query  {:handler <query-function-name>}}))) ```
Defmake is not an actual call so it needs [fractl.resolver.registry :refer [defmake]] in the requirements as defmake isn’t an actual clojure call. 'r’ in r/make-resolver is also needed as a requirement as [fractl.registry.core :as r]"},
   {:role "user" :content "generate a resolver for airtable that backs a User entity."},
   {:role "assistant" :content "Airtable Resolver:
File 1(Resolver):
(ns airtable-resolver.resolver
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.component :as cn]
            [org.httpkit.client :as http]
            [cheshire.core :as json]
            [clojure.string :as string]))
(def entity-table-mapping
  {:User \"UserTableName\"})
(def config (atom {}))
(defn generate-endpoint [base-id table-name]
  (str \"https://api.airtable.com/v0/\" base-id \"/\" table-name))
(defn get-table-name [entity-name]
  (get entity-table-mapping entity-name))
(defn create-entity [entity-data]
  (let [entity (cn/instance-user-attributes entity-data)
        airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:entity-name airtable-config)))
        formatted-entity (json/generate-string {:records [{:fields entity}]})]
    @(http/post endpoint {:headers {\"Authorization\" (str \"Bearer \" airtable-api-key) \"Content-Type\" \"application/json\"} :body formatted-entity})))
(defn update-entity [entity-id updated-data]
  (let [entity (cn/instance-user-attributes updated-data)
        airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:entity-name airtable-config)))
        formatted-entity (json/generate-string {:fields entity})]
    @(http/put endpoint {:headers {\"Authorization\" (str \"Bearer \" airtable-api-key) \"Content-Type\" \"application/json\"} :body formatted-entity})))
(defn delete-entity [entity-id]
  (let [airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:entity-name airtable-config)))]
    @(http/delete (endpoint \"/\" entity-id) {:headers {\"Authorization\" (str \"Bearer \" airtable-api-key)}})))
(defn query-entities [query]
  (let [airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:entity-name airtable-config)))]
    @(http/get endpoint {:headers {\"Authorization\" (str \"Bearer \" airtable-api-key)} :query-params {:filterByFormula query}})))
(defmake :airtable-resolver
  (fn [resolver-name new-config]
    (reset! config new-config)
    (r/make-resolver resolver-name
                     {:create {:handler create-entity}
                      :update {:handler update-entity}
                      :delete {:handler delete-entity}
                      :query  {:handler query-entities}})))
"},
   {:role "user" :content "Instead of handling everything in one file, create a second file to handle the http requests"},
   {:role "assistant" :content "Airtable Resolver:

File 1(Resolver):
(ns airtable-resolver.resolver
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.component :as cn]
            [cheshire.core :as json]
            [clojure.string :as string]))
(def entity-table-mapping
  {:User \"UserTableName\"})
(def config (atom {}))
(defn generate-endpoint [base-id table-name]
  (str \"https://api.airtable.com/v0/\" base-id \"/\" table-name))
(defn get-table-name [entity-name]
  (get entity-table-mapping entity-name))
(defn create-entity [entity-data]
  (let [entity (cn/instance-user-attributes entity-data)
        airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))
        formatted-entity (json/generate-string {:records [{:fields entity}]})]
    (http/post-in-base endpoint airtable-api-key formatted-entity)))
(defn update-entity [entity-id updated-data]
  (let [entity (cn/instance-user-attributes updated-data)
        airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))
        formatted-entity (json/generate-string {:fields entity})]
    (http/patch-in-base endpoint airtable-api-key formatted-entity)))
(defn delete-entity [entity-id]
  (let [airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))]
    (http/delete-in-base (endpoint \"/\" entity-id) airtable-api-key)))
(defn query-entities [query]
  (let [airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))]
    (http/get-in-base endpoint airtable-api-key {:filterByFormula query})))
(defmake :airtable-resolver
  (fn [resolver-name new-config]
    (reset! config new-config)
    (r/make-resolver resolver-name
                     {:create {:handler create-entity}
                      :update {:handler update-entity}
                      :delete {:handler delete-entity}
                      :query  {:handler query-entities}})))
File 2(Http):
(ns airtable-resolver.http
  (:require [cheshire.core :as json]
            [org.httpkit.client :as client]))
(ns airtable-resolver.http
  (:require [cheshire.core :as json]
            [org.httpkit.client :as client]))
(defn post [endpoint api-key entity]
  (let [headers {\"Authorization\" (str \"Bearer \" api-key)
                 \"Content-Type\" \"application/json\"}]
    @(client/post endpoint {:body entity :headers headers})))
(defn put [endpoint api-key entity]
  (let [headers {\"Authorization\" (str \"Bearer \" api-key)
                 \"Content-Type\" \"application/json\"}]
    @(client/put endpoint {:body entity :headers headers})))
(defn delete [endpoint api-key]
  (let [headers {\"Authorization\" (str \"Bearer \" api-key)}]
    @(client/delete endpoint {:headers headers})))
(defn get [endpoint api-key query-params]
  (let [headers {\"Authorization\" (str \"Bearer \" api-key)}]
    @(client/get endpoint {:headers headers :query-params query-params})))
(defn post-in-base [endpoint api-key entity]
  (let [response (post endpoint api-key entity)]
    (json/parse-string (:body response) true)))
(defn put-in-base [endpoint api-key entity]
  (let [response (put endpoint api-key entity)]
    (json/parse-string (:body response) true)))
(defn delete-in-base [endpoint api-key]
  (let [response (delete endpoint api-key)]
    (json/parse-string (:body response) true)))
(defn get-in-base [endpoint api-key query-params]
  (let [response (get endpoint api-key query-params)]
    (json/parse-string (:body response) true)))
"},
   {:role "user" :content "in the http file make it so that the *-in-base functions are combined with their request functions for simplicity"},
   {:role "assistant" :content "Airtable Resolver:
File 1(Resolver):
(ns airtable-resolver.resolver
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.component :as cn]
            [cheshire.core :as json]
            [clojure.string :as string]))
(def entity-table-mapping
  {:User \"UserTableName\"})
(def config (atom {}))
(defn generate-endpoint [base-id table-name]
  (str \"https://api.airtable.com/v0/\" base-id \"/\" table-name))
(defn get-table-name [entity-name]
  (get entity-table-mapping entity-name))
(defn create-entity [entity-data]
  (let [entity (cn/instance-user-attributes entity-data)
        airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))
        formatted-entity (json/generate-string {:records [{:fields entity}]})]
    (http/post-in-base endpoint airtable-api-key formatted-entity)))
(defn update-entity [entity-id updated-data]
  (let [entity (cn/instance-user-attributes updated-data)
        airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))
        formatted-entity (json/generate-string {:fields entity})]
    (http/patch-in-base endpoint airtable-api-key formatted-entity)))
(defn delete-entity [entity-id]
  (let [airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))]
    (http/delete-in-base (endpoint \"/\" entity-id) airtable-api-key)))
(defn query-entities [query]
  (let [airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))]
    (http/get-in-base endpoint airtable-api-key {:filterByFormula query})))
(defmake :airtable-resolver
  (fn [resolver-name new-config]
    (reset! config new-config)
    (r/make-resolver resolver-name
                     {:create {:handler create-entity}
                      :update {:handler update-entity}
                      :delete {:handler delete-entity}
                      :query  {:handler query-entities}})))
File 2(Http):
(ns airtable-resolver.http
  (:require [cheshire.core :as json]
            [org.httpkit.client :as client]))
(defn post-in-base [endpoint api-key entity]
  (let [headers {\"Authorization\" (str \"Bearer \" api-key)
                 \"Content-Type\" \"application/json\"}]
        (let [response @(client/post endpoint
                                     {:body entity
                                      :headers headers})]
          (json/parse-string (:body response) true))
      {:status \"error\" :message \"Invalid API key\"}))
(defn patch-in-base [url api-key entity-id entity]
  (let [endpoint (str endpoint \"/\" entity-id)
        headers {\"Authorization\" (str \"Bearer \" api-key)
                 \"Content-Type\" \"application/json\"}]
        (let [response @(client/put endpoint
                                    {:body entity
                                     :headers headers})]
          (json/parse-string (:body response) true))
      {:status \"error\" :message \"Invalid API key\"}))
(defn delete-in-base [url api-key entity-id]
  (let [endpoint (str url \"/\" entity-id)]
        (let [response @(client/delete endpoint
                                       {:headers {\"Authorization\" (str \"Bearer \" api-key)}})]
          (json/parse-string (:body response) true))
      {:status \"error\" :message \"Invalid API key\"}))
(defn get-in-base [endpoint api-key params]
      (let [response @(client/get endpoint
                                {:headers {\"Authorization\" (str \"Bearer \" api-key)}
                                 :query-params params})]
        (json/parse-string (:body response) true))))
"},
   {:role "user" :content "Make it so that query-entities can query either a single record, the whole table, or certain records with parameters"},
   {:role "assistant" :content "Airtable Resolver:
File 1(Resolver):
(ns airtable-resolver.resolver
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.component :as cn]
            [cheshire.core :as json]
            [clojure.string :as string]))
(def entity-table-mapping
  {:User \"UserTableName\"})
(def config (atom {}))
(defn generate-endpoint [base-id table-name]
  (str \"https://api.airtable.com/v0/\" base-id \"/\" table-name))
(defn get-table-name [entity-name]
  (get entity-table-mapping entity-name))
(defn create-entity [entity-data]
  (let [entity (cn/instance-user-attributes entity-data)
        airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))
        formatted-entity (json/generate-string {:records [{:fields entity}]})]
    (http/post-in-base endpoint airtable-api-key formatted-entity)))
(defn update-entity [entity-id updated-data]
  (let [entity (cn/instance-user-attributes updated-data)
        airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))
        formatted-entity (json/generate-string {:fields entity})]
    (http/patch-in-base endpoint airtable-api-key formatted-entity)))
(defn delete-entity [entity-id]
  (let [airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))]
    (http/delete-in-base (endpoint \"/\" entity-id) airtable-api-key)))
(defn query-entities [query]
  (let [airtable-config @config
        airtable-api-key (:api-key airtable-config)
         endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))]
    (if (seq query)
      (if (and (string? query) (re-matches #\"^record-id:.*$\" query))
        (let [record-id (string/trim (subs query 10))]
          (http/get-in-base endpoint airtable-api-key {:id record-id}))
        (http/get-in-base endpoint airtable-api-key {:filterByFormula query}))
      (http/get-in-base endpoint airtable-api-key {}))))
(defmake :airtable-resolver
  (fn [resolver-name new-config]
    (reset! config new-config)
    (r/make-resolver resolver-name
                     {:create {:handler create-entity}
                      :update {:handler update-entity}
                      :delete {:handler delete-entity}
                      :query  {:handler query-entities}})))
File 2(Http):
(ns airtable-resolver.http
  (:require [cheshire.core :as json]
            [org.httpkit.client :as client]))
(defn post-in-base [endpoint api-key entity]
  (let [headers {\"Authorization\" (str \"Bearer \" api-key)
                 \"Content-Type\" \"application/json\"}]
    (if api-key
        (let [response @(client/post endpoint
                                     {:body entity
                                      :headers headers})]
          (json/parse-string (:body response) true))
      {:status \"error\" :message \"Invalid API key\"})))
(defn patch-in-base [url api-key entity-id entity]
  (let [endpoint (str endpoint \"/\" entity-id)
        headers {\"Authorization\" (str \"Bearer \" api-key)
                 \"Content-Type\" \"application/json\"}]
    (if api-key
        (let [response @(client/put endpoint
                                    {:body entity
                                     :headers headers})]
          (json/parse-string (:body response) true))
      {:status \"error\" :message \"Invalid API key\"})))
(defn delete-in-base [url api-key entity-id]
  (let [endpoint (str url \"/\" entity-id)]
    (if api-key
        (let [response @(client/delete endpoint
                                       {:headers {\"Authorization\" (str \"Bearer \" api-key)}})]
          (json/parse-string (:body response) true))
      {:status \"error\" :message \"Invalid API key\"})))
(defn get-in-base [endpoint api-key params]
  (let [id (get params :id)]
    (if id
      (let [record-endpoint (str endpoint \"/\" id)]
        (let [response @(client/get record-endpoint
                                    {:headers {\"Authorization\" (str \"Bearer \" api-key)}})]
          (json/parse-string (:body response) true)))
      (let [response @(client/get endpoint
                                  {:headers {\"Authorization\" (str \"Bearer \" api-key)}
                                   :query-params params})]
        (json/parse-string (:body response) true)))))
"}, {:role "user" :content "Generate a resolver that backs the following fractl data model with airtable: (component :Planner.App)
(entity
 {:User
  {:Username    {:type     :Kernel/String
                 :unique   true}
   :DisplayName {:type     :Kernel/String
                 :optional true}
   :Password    {:type        :Kernel/Password
                 :secure-hash true}
   :Email       :Kernel/Email
   :IsAdmin     {:type      :Kernel/Boolean
                 :default   false}
   :id          {:type      :Kernel/String
                 :optional  true}
   :meta
   {:str :UserName
    :order [:Username :DisplayName :Password :Email :IsAdmin]}}})
(entity
 {:TeamEvent
  {:Name  {:type     :Kernel/String
           :unique   true}
   :Notes {:type     :Kernel/String
           :optional true}
   :Date  {:type     :Kernel/String
           :optional true}
   :id    {:type     :Kernel/String
           :optional true}
   :meta
   {:str :Name
    :order [:Name :Notes :DateTime]}}})
(entity
 {:Invitation
  {:TeamEvent {:ref      :Planner.App/TeamEvent.id}
   :Notes     {:type     :Kernel/String
               :optional true}
   :Recipient {:ref      :Planner.App/User.id}
   :Status    {:type     :Kernel/String
               :oneof    [\"Accepted\", \"Rejected\", \"Pending\"]
               :default  \"Pending\"}
   :id        {:type     :Kernel/String
               :optional true}
   :meta
   {:order [:Recipient :Notes :TeamEvent :Status]}}})
(entity
 {:Participant
  {:Participant {:ref      :Planner.App/User.id}
   :TeamEvent   {:ref      :Planner.App/TeamEvent.id}
   :id          {:type     :Kernel/String
                 :optional true}
   :meta
   {:str :Name
    :order [:Participant :TeamEvent]}}})
(record
 {:EventInfo
  {:TeamEvent    :Planner.App/TeamEvent
   :Participants {:listof :Planner.App/Participant}}})
(event
 {:UpdateInvitation
  {:TeamEvent {:ref   :Planner.App/TeamEvent.id}
   :Recipient {:ref   :Planner.App/User.id}
   :Status    {:type  :Kernel/String
               :oneof [\"Accepted\", \"Rejected\"]}}})
(dataflow
 :UpdateInvitation
 {:Invitation {:TeamEvent? :UpdateInvitation.TeamEvent
               :Recipient? :UpdateInvitation.Recipient
               :Status :UpdateInvitation.Status}}
 [:match :UpdateInvitation.Status
  \"Accepted\" {:Participant {:Participant :UpdateInvitation.Recipient
                            :TeamEvent :UpdateInvitation.TeamEvent}}])
(event
 {:GetEventData
  {:TeamEvent :Kernel/String}})
(dataflow
 :GetEventData
 {:Participant {:TeamEvent? :GetEventData.TeamEvent}
  :as :Participants}
 {:TeamEvent
  {:id? :GetEventData.TeamEvent} :as [:EventDetails]}
 {:EventInfo
  {:TeamEvent    :EventDetails
   :Participants :Participants}})
(event
 {:DeclineEvent
  {:TeamEvent {:ref :Planner.App/TeamEvent.id}
   :Recipient {:ref :Planner.App/User.id}}})
(dataflow
 :DeclineEvent
 {:Participant {:TeamEvent?   :DeclineEvent.TeamEvent
                :Participant? :DeclineEvent.Recipient}}
 [:delete :Participant (cn/append-id :Participant)]
 {:Invitation {:TeamEvent? :DeclineEvent.TeamEvent
               :Recipient? :DeclineEvent.Recipient
               :Status \"Rejected\"}}
 {:TeamEvent
  {:id? :DeclineEvent.TeamEvent} :as [:EventDetails]}
 {:Participant
  {:TeamEvent? :TeamEvent.id} :as :Participants}
 {:EventInfo
  {:TeamEvent    :EventDetails
   :Participants :Participants}})
"},
   {:role "assistant" :content "Airtable Resolver:
File 1(Resolver):
(ns airtable-resolver.resolver
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.component :as cn]
            [cheshire.core :as json]
            [clojure.string :as string]))
(def entity-table-mapping
  {:User \"UserTableName\"
   :TeamEvent \"TeamEventTableName\"
   :Invitation \"InvitationTableName\"
   :Participant \"ParticipantTableName\"})
(def config (atom {}))
(defn generate-endpoint [base-id table-name]
  (str \"https://api.airtable.com/v0/\" base-id \"/\" table-name))
(defn get-table-name [entity-name]
  (get entity-table-mapping entity-name))
(defn create-entity [entity-data]
  (let [entity (cn/instance-user-attributes entity-data)
        airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))
        formatted-entity (json/generate-string {:records [{:fields entity}]})]
    (http/post-in-base endpoint airtable-api-key formatted-entity)))
(defn update-entity [entity-id updated-data]
  (let [entity (cn/instance-user-attributes updated-data)
        airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))
        formatted-entity (json/generate-string {:fields entity})]
    (http/patch-in-base endpoint airtable-api-key formatted-entity)))
(defn delete-entity [entity-id]
  (let [airtable-config @config
        airtable-api-key (:api-key airtable-config)
        endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))]
    (http/delete-in-base (endpoint \"/\" entity-id) airtable-api-key)))
(defn query-entities [query]
  (let [airtable-config @config
        airtable-api-key (:api-key airtable-config)
         endpoint (generate-endpoint (:base-id airtable-config) (get-table-name (:-*-type-*- entity-data)))]
    (if (seq query)
      (if (and (string? query) (re-matches #\"^record-id:.*$\" query))
        (let [record-id (string/trim (subs query 10))]
          (http/get-in-base endpoint airtable-api-key {:id record-id}))
        (http/get-in-base endpoint airtable-api-key {:filterByFormula query}))
      (http/get-in-base endpoint airtable-api-key {}))))
(defmake :airtable-resolver
  (fn [resolver-name new-config]
    (reset! config new-config)
    (r/make-resolver resolver-name
                     {:create {:handler create-entity}
                      :update {:handler update-entity}
                      :delete {:handler delete-entity}
                      :query  {:handler query-entities}})))
File 2(Http):
(ns airtable-resolver.http
  (:require [cheshire.core :as json]
            [org.httpkit.client :as client]))
(defn post-in-base [endpoint api-key entity]
  (let [headers {\"Authorization\" (str \"Bearer \" api-key)
                 \"Content-Type\" \"application/json\"}]
    (if api-key
        (let [response @(client/post endpoint
                                     {:body entity
                                      :headers headers})]
          (json/parse-string (:body response) true))
      {:status \"error\" :message \"Invalid API key\"})))
(defn patch-in-base [url api-key entity-id entity]
  (let [endpoint (str endpoint \"/\" entity-id)
        headers {\"Authorization\" (str \"Bearer \" api-key)
                 \"Content-Type\" \"application/json\"}]
    (if api-key
        (let [response @(client/put endpoint
                                    {:body entity
                                     :headers headers})]
          (json/parse-string (:body response) true))
      {:status \"error\" :message \"Invalid API key\"})))
(defn delete-in-base [url api-key entity-id]
  (let [endpoint (str url \"/\" entity-id)]
    (if api-key
        (let [response @(client/delete endpoint
                                       {:headers {\"Authorization\" (str \"Bearer \" api-key)}})]
          (json/parse-string (:body response) true))
      {:status \"error\" :message \"Invalid API key\"})))
(defn get-in-base [endpoint api-key params]
  (let [id (get params :id)]
    (if id
      (let [record-endpoint (str endpoint \"/\" id)]
        (let [response @(client/get record-endpoint
                                    {:headers {\"Authorization\" (str \"Bearer \" api-key)}})]
          (json/parse-string (:body response) true)))
      (let [response @(client/get endpoint
                                  {:headers {\"Authorization\" (str \"Bearer \" api-key)}
                                   :query-params params})]
        (json/parse-string (:body response) true)))))"},
   {:role "user" :content "Create a version of the resolver that gets the api-key and base-id from the environment variables instead"},
   {:role "assistant" :content "Airtable Resolver:
File 1(Resolver):
(ns airtable-resolver.resolver
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.component :as cn]
            [fractl.util :as u]
            [cheshire.core :as json]
            [clojure.string :as string]))
(def entity-table-mapping
  {:User \"UserTableName\"
   :TeamEvent \"TeamEventTableName\"
   :Invitation \"InvitationTableName\"
   :Participant \"ParticipantTableName\"})
(def config (atom {}))
(defn generate-endpoint [base-id table-name]
  (str \"https://api.airtable.com/v0/\" base-id \"/\" table-name))
(defn get-table-name [entity-name]
  (get entity-table-mapping entity-name))
(defn create-entity [entity-data]
  (let [entity (cn/instance-user-attributes entity-data)
        airtable-api-key (u/getenv API_KEY)
        endpoint (generate-endpoint (u/getenv BASE_ID) (get-table-name (:-*-type-*- entity-data)))
        formatted-entity (json/generate-string {:records [{:fields entity}]})]
    (http/post-in-base endpoint airtable-api-key formatted-entity)))
(defn update-entity [entity-id updated-data]
  (let [entity (cn/instance-user-attributes updated-data)
        airtable-api-key (u/getenv API_KEY)
        endpoint (generate-endpoint (u/getenv BASE_ID) (get-table-name (:-*-type-*- entity-data)))
        formatted-entity (json/generate-string {:fields entity})]
    (http/patch-in-base endpoint airtable-api-key formatted-entity)))
(defn delete-entity [entity-id]
  (let [airtable-api-key (u/getenv API_KEY)
        endpoint (generate-endpoint (u/getenv BASE_ID) (get-table-name (:-*-type-*- entity-data)))]
    (http/delete-in-base (endpoint \"/\" entity-id) airtable-api-key)))
(defn query-entities [query]
  (let [airtable-api-key (u/getenv API_KEY)
         endpoint (generate-endpoint (u/getenv BASE_ID) (get-table-name (:-*-type-*- entity-data)))]
    (if (seq query)
      (if (and (string? query) (re-matches #\"^record-id:.*$\" query))
        (let [record-id (string/trim (subs query 10))]
          (http/get-in-base endpoint airtable-api-key {:id record-id}))
        (http/get-in-base endpoint airtable-api-key {:filterByFormula query}))
      (http/get-in-base endpoint airtable-api-key {}))))
(defmake :airtable-resolver
  (fn [resolver-name new-config]
    (reset! config new-config)
    (r/make-resolver resolver-name
                     {:create {:handler create-entity}
                      :update {:handler update-entity}
                      :delete {:handler delete-entity}
                      :query  {:handler query-entities}})))
File 2(Http):
(ns airtable-resolver.http
  (:require [cheshire.core :as json]
            [org.httpkit.client :as client]))
(defn post-in-base [endpoint api-key entity]
  (let [headers {\"Authorization\" (str \"Bearer \" api-key)
                 \"Content-Type\" \"application/json\"}]
    (if api-key
        (let [response @(client/post endpoint
                                     {:body entity
                                      :headers headers})]
          (json/parse-string (:body response) true))
      {:status \"error\" :message \"Invalid API key\"})))
(defn patch-in-base [url api-key entity-id entity]
  (let [endpoint (str endpoint \"/\" entity-id)
        headers {\"Authorization\" (str \"Bearer \" api-key)
                 \"Content-Type\" \"application/json\"}]
    (if api-key
        (let [response @(client/put endpoint
                                    {:body entity
                                     :headers headers})]
          (json/parse-string (:body response) true))
      {:status \"error\" :message \"Invalid API key\"})))
(defn delete-in-base [url api-key entity-id]
  (let [endpoint (str url \"/\" entity-id)]
    (if api-key
        (let [response @(client/delete endpoint
                                       {:headers {\"Authorization\" (str \"Bearer \" api-key)}})]
          (json/parse-string (:body response) true))
      {:status \"error\" :message \"Invalid API key\"})))
(defn get-in-base [endpoint api-key params]
  (let [id (get params :id)]
    (if id
      (let [record-endpoint (str endpoint \"/\" id)]
        (let [response @(client/get record-endpoint
                                    {:headers {\"Authorization\" (str \"Bearer \" api-key)}})]
          (json/parse-string (:body response) true)))
      (let [response @(client/get endpoint
                                  {:headers {\"Authorization\" (str \"Bearer \" api-key)}
                                   :query-params params})]
        (json/parse-string (:body response) true)))))"},
   {:role "user" :content "Generate a resolver that backs 2 entities, Bucket and Object with the AWS S3 service"},
   {:role "system" :content "for attributes that AWS automatically creates, such as an ARN when working with SNS, dont use the arn or any other automatically-defined attribute when creating an entity"},
   {:role "assistant" :content "AWS S3 Resolver:
File 1(Resolver):
(ns aws-s3-resolver.resolver
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.component :as cn]
            [clojure.java.io :as io]
            [aws-s3-resolver.client :as client]))
(def config (atom {}))
(defn create-entity [entity]
  (let [data (cn/instance-user-attributes entity)]
    (cond
      (= (:-*-type-*- entity) :Bucket) (let [bucket-name (:Name data)
                                             region (:Region data)]
                                         (client/create-bucket bucket-name region))
      (= (:-*-type-*- entity) :Object) (let [bucket-name (:Bucket data)
                                             key (:Key data)
                                             file (io/file (:File data))]
                                         (client/put-object bucket-name key file)))))

(defn query-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        bucket-name (:Bucket data)
        key (:Key data)]
    (client/get-object bucket-name key)))
(defn update-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        bucket-name (:Bucket data)
        key (:Key data)
        file (io/file (:File data))]
    (client/put-object bucket-name key file)))
(defn delete-entity [entity]
  (let [data (cn/instance-user-attributes entity)]
    (cond
      (= (:-*-type-*- entity) :Bucket) (let [bucket-name (:Name data)]
                                         (client/delete-bucket bucket-name))
      (= (:-*-type-*- entity) :Object) (let [bucket-name (:Bucket data)
                                             key (:Key data)]
                                         (client/delete-object bucket-name key)))))
(defmake :aws-s3-resolver
  (fn [resolver-name new-config]
    (reset! bucket-config new-config)
    (r/make-resolver resolver-name
                     {:create {:handler create-entity}
                      :query  {:handler query-entity}
                      :update {:handler update-entity}
                      :delete {:handler delete-entity}})))
File 2(Client):
(ns aws-s3-resolver.client
  (:require [amazonica.aws.s3 :as s3]))
(defn create-bucket [bucket-name region]
  (s3/create-bucket :bucket-name bucket-name :region region))
(defn delete-bucket [bucket-name]
    (s3/delete-bucket :bucket-name bucket-name))
(defn put-object [bucket key file]
    (s3/put-object :bucket-name bucket :key key :file file))
(defn get-object [bucket key]
    (s3/get-object :bucket-name bucket :key key))
(defn delete-object [bucket key]
  (s3/delete-object :bucket-name bucket :key key)
"},
   {:role "user" :content "Make it so that you have to use AWS login credentials to access buckets (credentials need to have the access-key, secret-key, and endpoint, otherwise they will not work)"}
   {:role "assistant" :content "AWS S3 Resolver:
File 1(Resolver):
(ns aws-s3-resolver.resolver
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.component :as cn]
            [aws-s3-resolver.client :as client]
            [clojure.java.io :as io]))
(def config (atom {}))
(defn create-entity [entity]
  (let [data (cn/instance-user-attributes entity)]
    (cond
      (= (:-*-type-*- entity) :Bucket) (let [bucket-name (:Name data)
                                             region (:Region data)]
                                         (client/create-bucket (:cred @config) bucket-name region))
      (= (:-*-type-*- entity) :Object) (let [bucket-name (:Bucket data)
                                             key (:Key data)
                                             file (io/file (:File data))]
                                         (println file)
                                         (client/put-object (:cred @config) bucket-name key file)))))
(defn query-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        bucket-name (:Bucket data)
        key (:Key data)]
    (client/get-object (:cred @config) bucket-name key)))
(defn update-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        bucket-name (:Bucket data)
        key (:Key data)
        file (io/file (:File data))]
    (client/put-object (:cred @config) bucket-name key file)))
(defn delete-entity [entity]
  (let [data (cn/instance-user-attributes entity)]
    (cond
      (= (:-*-type-*- entity) :Bucket) (let [bucket-name (:Name data)]
                                         (client/delete-bucket (:cred @config) bucket-name))
      (= (:-*-type-*- entity) :Object) (let [bucket-name (:Bucket data)
                                             key (:Key data)]
                                         (client/delete-object (:cred @config) bucket-name key)))))
(defmake :aws-s3-resolver
  (fn [resolver-name new-config]
    (reset! config new-config)
    (r/make-resolver resolver-name
                     {:create {:handler create-entity}
                      :query  {:handler query-entity}
                      :update {:handler update-entity}
                      :delete {:handler delete-entity}})))
File 2(Client):
(ns aws-s3-resolver.client
  (:require [amazonica.aws.s3 :as s3]))
(defn create-bucket
  ([cred bucket-name region]
    (s3/create-bucket cred :bucket-name bucket-name :region region)))
(defn delete-bucket
  ([cred bucket-name]
    (s3/delete-bucket cred :bucket-name bucket-name)))
(defn put-object
  ([cred bucket key file]
    (s3/put-object cred :bucket-name bucket :key key :file file)))
(defn get-object
  ([cred bucket key file]
    (s3/get-object cred :bucket-name bucket :key key :file file)))
(defn delete-object
  ([cred bucket key]
    (s3/delete-object cred :bucket-name bucket :key key))"},
   {:role "user" :content "Generate a resolver that backs the following fractl data model with AWS S3: (component :AWS.S3) (entity :AWS.S3/Bucket {:Name :String :Region :String}) (entity :AWS.S3/Object {:Key :String :Bucket :String :File :String}) (relationship :AWS.S3/BucketObject {:meta {:contains [:AWS.S3/Bucket :AWS.S3/Object]}})"},
   {:role "assistant" :content "AWS S3 Resolver:
File 1(Resolver):
(ns aws-s3-resolver.resolver
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.component :as cn]
            [aws-s3-resolver.client :as client]
            [clojure.java.io :as io]))
(def config (atom {}))
(defn create-entity [entity]
  (let [data (cn/instance-user-attributes entity)]
    (cond
      (= (:-*-type-*- entity) :Bucket) (let [bucket-name (:Name data)
                                             region (:Region data)]
                                         (client/create-bucket (:cred @config) bucket-name region))
      (= (:-*-type-*- entity) :Object) (let [bucket-name (:Bucket data)
                                             key (:Key data)
                                             file (io/file (:File data))]
                                         (println file)
                                         (client/put-object (:cred @config) bucket-name key file)))))
(defn query-entity [entity]
  (let [data (cn/instance-user-attributes entity)]
    (cond
      (= (:-*-type-*- entity) :Bucket) (println \"Cannot query for bucket entity, to query specific bucket info, add implementation\")
      (= (:-*-type-*- entity) :Object) (let [bucket-name (:Bucket data)
                                             key (:Key data)]
                                         (println file)
                                         (client/get-object (:cred @config) bucket-name key)))))
(defn update-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        bucket-name (:Bucket data)
        key (:Key data)
        file (io/file (:File data))]
    (client/put-object (:cred @config) bucket-name key file)
    (cond
      (= (:-*-type-*- entity) :Bucket) (println \"Cannot update for bucket entity, to update specific bucket info, add implementation\")
      (= (:-*-type-*- entity) :Object) (let [bucket-name (:Bucket data)
                                             key (:Key data)
                                             file (io/file (:File data))]
                                         (println file)
                                         (client/put-object (:cred @config) bucket-name key file)))))
(defn delete-entity [entity]
  (let [data (cn/instance-user-attributes entity)]
    (cond
      (= (:-*-type-*- entity) :Bucket) (let [bucket-name (:Name data)]
                                         (client/delete-bucket (:cred @config) bucket-name))
      (= (:-*-type-*- entity) :Object) (let [bucket-name (:Bucket data)
                                             key (:Key data)]
                                         (client/delete-object (:cred @config) bucket-name key)))))
(defmake :aws-s3-resolver
  (fn [resolver-name new-config]
    (reset! config new-config)
    (r/make-resolver resolver-name
                     {:create {:handler create-entity}
                      :query  {:handler query-entity}
                      :update {:handler update-entity}
                      :delete {:handler delete-entity}})))
File 2(Client):
(ns aws-s3-resolver.client
  (:require [amazonica.aws.s3 :as s3]))
(defn create-bucket
  ([cred bucket-name region]
    (s3/create-bucket cred :bucket-name bucket-name :region region)))
(defn delete-bucket
  ([cred bucket-name]
    (s3/delete-bucket cred :bucket-name bucket-name)))
(defn put-object
  ([cred bucket key file]
    (s3/put-object cred :bucket-name bucket :key key :file file)))
(defn get-object
  ([cred bucket key file]
    (s3/get-object cred :bucket-name bucket :key key :file file)))
(defn delete-object
  ([cred bucket key]
    (s3/delete-object cred :bucket-name bucket :key key))"},
   {:role "system" :content "There is no query for the BucketObject relationship and query's only work for entities not relationships"},
   {:role "user" :content "Create a version of the resolver so that the credentials are retrieved from the environment variables rather than from config"},
   {:role "assistant" :content "AWS S3 Resolver:
File 1(Resolver):
(ns aws-s3-resolver.resolver
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.component :as cn]
            [fractl.util :as u]
            [aws-s3-resolver.client :as client]
            [clojure.java.io :as io]))
(defn create-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        credentials {:access-key (u/getenv AWS_ACCESS_KEY)
                     :secret-key (u/getenv AWS_SECRET_KEY)
                     :endpoint (u/getenv AWS_ENDPOINT)}]
    (cond
      (= (:-*-type-*- entity) :Bucket) (let [bucket-name (:Name data)
                                             region (:Region data)]
                                         (client/create-bucket credentials bucket-name region))
      (= (:-*-type-*- entity) :Object) (let [bucket-name (:Bucket data)
                                             key (:Key data)
                                             file (io/file (:File data))]
                                         (println file)
                                         (client/put-object credentials bucket-name key file)))))
(defn query-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        credentials {:access-key (u/getenv AWS_ACCESS_KEY)
                     :secret-key (u/getenv AWS_SECRET_KEY)
                     :endpoint (u/getenv AWS_ENDPOINT)}]
    (cond
      (= (:-*-type-*- entity) :Bucket) (println \"Cannot query for bucket entity, to query specific bucket info, add implementation\")
      (= (:-*-type-*- entity) :Object) (let [bucket-name (:Bucket data)
                                             key (:Key data)]
                                         (println file)
                                         (client/get-object credentials bucket-name key)))))
(defn update-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        credentials {:access-key (u/getenv AWS_ACCESS_KEY)
                     :secret-key (u/getenv AWS_SECRET_KEY)
                     :endpoint (u/getenv AWS_ENDPOINT)}]
    (cond
      (= (:-*-type-*- entity) :Bucket) (println \"Cannot update for bucket entity, to update specific bucket info, add implementation\")
      (= (:-*-type-*- entity) :Object) (let [bucket-name (:Bucket data)
                                             key (:Key data)
                                             file (io/file (:File data))]
                                         (println file)
                                         (client/put-object credentials bucket-name key file)))))
(defn delete-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        credentials {:access-key (u/getenv AWS_ACCESS_KEY)
                                                          :secret-key (u/getenv AWS_SECRET_KEY)
                                                          :endpoint (u/getenv AWS_ENDPOINT)}]
    (cond
      (= (:-*-type-*- entity) :Bucket) (let [bucket-name (:Name data)]
                                         (client/delete-bucket credentials bucket-name))
      (= (:-*-type-*- entity) :Object) (let [bucket-name (:Bucket data)
                                             key (:Key data)]
                                         (client/delete-object credentials bucket-name key)))))
(defmake :aws-s3-resolver
  (fn [resolver-name new-config]
    (reset! config new-config)
    (r/make-resolver resolver-name
                     {:create {:handler create-entity}
                      :query  {:handler query-entity}
                      :update {:handler update-entity}
                      :delete {:handler delete-entity}})))
File 2(Client):
(ns aws-s3-resolver.client
  (:require [amazonica.aws.s3 :as s3]))
(defn create-bucket
  ([cred bucket-name region]
    (s3/create-bucket cred :bucket-name bucket-name :region region)))
(defn delete-bucket
  ([cred bucket-name]
    (s3/delete-bucket cred :bucket-name bucket-name)))
(defn put-object
  ([cred bucket key file]
    (s3/put-object cred :bucket-name bucket :key key :file file)))
(defn get-object
  ([cred bucket key file]
    (s3/get-object cred :bucket-name bucket :key key :file file)))
(defn delete-object
  ([cred bucket key]
    (s3/delete-object cred :bucket-name bucket :key key))"},
   {:role "user" :content "Generate a resolver that backs the following fractl data model with AWS SNS:
                             (component :AWS.SNS)
                             (entity :AWS.SNS/Topic {:Name :String :TopicArn :String})
                             (entity :AWS.SNS/Subscription {:Endpoint :String :Protocol :String :SubscriptionArn :String})
                             (entity :AWS.SNS/Message {:TopicArn :String :Content :String :Subject :String})
                             (relationship :AWS.SNS/TopicSubscription {:meta {:contains [:AWS.SNS/Topic :AWS.SNS/Subscription]}})"},
   {:role "assistant" :content "AWS SNS Resolver:
File 1(Resolver):
(ns aws-sns-resolver.resolver
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.component :as cn]
            [aws-sns-resolver.client :as client]))
(def config (atom {}))
(defn create-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        credentials (:credentials @config)]
    (cond
      (= (:-*-type-*- entity) :Topic) (let [topic-name (:Name data)]
                                         (client/create-topic credentials topic-name))
      (= (:-*-type-*- entity) :Subscription) (let [endpoint (:Endpoint data)
                                                   protocol (:Protocol data)
                                                   topic-arn (:TopicArn data)]
                                               (client/subscribe credentials endpoint protocol topic-arn))
      (= (:-*-type-*- entity) :Message) (let [topic-arn (:TopicArn data)
                                              content (:Content data)
                                              subject (:Subject data)]
                                          (client/publish credentials topic-arn content subject)))))
(defn query-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        credentials (:credentials @config)]
    (cond
      (= (:-*-type-*- entity) :Topic) (println \"Cannot query for Topic entity, to query specific topic info, add implementation\")
      (= (:-*-type-*- entity) :Subscription) (let [subscription-arn (:SubscriptionArn data)]
                                               (client/get-subscription-attributes credentials subscription-arn))
      (= (:-*-type-*- entity) :Message) (println \"Cannot query for Message entity, to query specific message info, add implementation\"))))
(defn update-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        credentials (:credentials @config)]
    (cond
      (= (:-*-type-*- entity) :Topic) (println \"Cannot update for Topic entity, to update specific topic info, add implementation\")
      (= (:-*-type-*- entity) :Subscription) (let [subscription-arn (:SubscriptionArn data)]
                                               (client/set-subscription-attributes credentials subscription-arn))
      (= (:-*-type-*- entity) :Message) (println \"Cannot update for Message entity, to update specific message info, add implementation\"))))
(defn delete-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        credentials (:credentials @config)]
    (cond
      (= (:-*-type-*- entity) :Topic) (let [topic-arn (:TopicArn data)]
                                         (client/delete-topic credentials topic-arn))
      (= (:-*-type-*- entity) :Subscription) (let [subscription-arn (:SubscriptionArn data)]
                                               (client/unsubscribe credentials subscription-arn))
      (= (:-*-type-*- entity) :Message) (println \"Cannot delete for Message entity, to delete specific message, add implementation\"))))
(defmake :aws-sns-resolver
  (fn [resolver-name new-config]
    (reset! config new-config)
    (r/make-resolver resolver-name
                     {:create {:handler create-entity}
                      :query  {:handler query-entity}
                      :update {:handler update-entity}
                      :delete {:handler delete-entity}})))
File 2(Client):
(ns aws-sns-resolver.client
  (:require [amazonica.aws.sns :as sns]))
(defn create-topic [credentials topic-name]
  (sns/create-topic credentials :name topic-name))
(defn delete-topic [credentials topic-arn]
  (sns/delete-topic credentials :topic-arn topic-arn))
(defn get-topic-attributes [credentials topic-arn]
  (sns/get-topic-attributes credentials :topic-arn topic-arn))
(defn set-topic-attributes [credentials topic-arn attributes]
  (sns/set-topic-attributes credentials :topic-arn topic-arn :attributes attributes))
(defn subscribe [credentials endpoint protocol topic-arn]
  (sns/subscribe credentials :endpoint endpoint :protocol protocol :topic-arn topic-arn))
(defn unsubscribe [credentials subscription-arn]
  (sns/unsubscribe credentials :subscription-arn subscription-arn))
(defn publish [credentials topic-arn message subject]
  (sns/publish credentials :message message :subject subject :topic-arn topic-arn))
(defn get-subscription-attributes [credentials subscription-arn]
  (sns/get-subscription-attributes credentials :subscription-arn subscription-arn))
(defn set-subscription-attributes [credentials subscription-arn attributes]
  (sns/set-subscription-attributes credentials :subscription-arn subscription-arn :attributes attributes))"}
   {:role "user" :content "Generate a resolver for the following fractl data model using Google Classroom's API:
                             (component :Google.Classroom)
                             (entity :Google.Classroom/User {:Id {:type :String :identity true} :Name :String :Email :String})
                             (entity :Google.Classroom/Course {:Id {:type :String :identity true} :Title :String :Description :String :CreatedBy {:type :Google.Classroom/User :indexed true} :CreatedOn :DateTime})
                             (entity :Google.Classroom/Assignment {:Id {:type :String :identity true} :Title :String :Description :String :Course {:type :Google.Classroom/Course :indexed true} :CreatedBy {:type :Google.Classroom/User :indexed true} :CreatedOn :DateTime})
                             (relationship :Google.Classroom/Enrollment {:meta {:contains [:Google.Classroom/Course :Google.Classroom/User]}})
                             Use OAuth 2.0 for authentication"},
   {:role "assistant" :content "Assistant's response: Google Classroom Resolver:
File 1(Resolver):
```clojure
(ns google-classroom-resolver.resolver
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]]
            [fractl.component :as cn]
            [clj-http.client :as http]
            [cheshire.core :as json]))
(def config (atom {}))
(defn create-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        credentials (:credentials @config)]
    (cond
      (= (:-*-type-*- entity) :Google.Classroom/User) (let [id (:Id data)
                                           name (:Name data)
                                           email (:Email data)]
                                       (create-user credentials id name email))
      (= (:-*-type-*- entity) :Google.Classroom/Course) (let [id (:Id data)
                                             title (:Title data)
                                             description (:Description data)
                                             created-by (:CreatedBy data)
                                             created-on (:CreatedOn data)]
                                         (create-course credentials id title description created-by created-on))
      (= (:-*-type-*- entity) :Google.Classroom/Assignment) (let [id (:Id data)
                                                 title (:Title data)
                                                 description (:Description data)
                                                 course (:Course data)
                                                 created-by (:CreatedBy data)
                                                 created-on (:CreatedOn data)]
                                             (create-assignment credentials id title description course created-by created-on)))))
(defn query-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        credentials (:credentials @config)]
    (cond
      (= (:-*-type-*- entity) :Google.Classroom/User) (println \"Cannot query for User entity, to query specific user info, add implementation\")
      (= (:-*-type-*- entity) :Google.Classroom/:Course) (println \"Cannot query for Course entity, to query specific course info, add implementation\")
      (= (:-*-type-*- entity) :Google.Classroom/:Assignment) (let [course-id (:CourseId data)]
                                             (get-assignments credentials course-id)))))
(defn update-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        credentials (:credentials @config)]
    (cond
      (= (:-*-type-*- entity) :Google.Classroom/:User) (println \"Cannot update for User entity, to update specific user info, add implementation\")
      (= (:-*-type-*- entity) :Google.Classroom/:Course) (println \"Cannot update for Course entity, to update specific course info, add implementation\")
      (= (:-*-type-*- entity) :Google.Classroom/:Assignment) (println \"Cannot update for Assignment entity, to update specific assignment info, add implementation\"))))
(defn delete-entity [entity]
  (let [data (cn/instance-user-attributes entity)
        credentials (:credentials @config)]
    (cond
      (= (:-*-type-*- entity) :Google.Classroom/:User) (let [id (:Id data)]
                                       (delete-user credentials id))
      (= (:-*-type-*- entity) :Google.Classroom/:Course) (let [id (:Id data)]
                                         (delete-course credentials id))
      (= (:-*-type-*- entity) :Google.Classroom/:Assignment) (let [id (:Id data)]
                                             (delete-assignment credentials id)))))
(defmake :google-classroom-resolver
  (fn [resolver-name new-config]
    (reset! config new-config)
    (r/make-resolver resolver-name
                     {:create {:handler create-entity}
                      :query  {:handler query-entity}
                      :update {:handler update-entity}
                      :delete {:handler delete-entity}})))
```
File 2(Client):
```clojure
(ns google-classroom-resolver.client
  (:require [cheshire.core :as json]))
(defn create-user [credentials id name email]
  (let [url (str \"https://www.googleapis.com/classroom/v1/users\" \"?access_token=\" (:access-token credentials))
        user {:id id :name name :email email}]
    (-> (http/post url {:body (json/generate-string user)
                        :headers {\"Content-Type\" \"application/json\"}})
        :body
        json/parse-string)))
(defn delete-user [credentials id]
  (let [url (str \"https://www.googleapis.com/classroom/v1/users/\" id \"?access_token=\" (:access-token credentials))]
    (http/delete url)))
(defn create-course [credentials id title description created-by created-on]
  (let [url (str \"https://www.googleapis.com/classroom/v1/courses\" \"?access_token=\" (:access-token credentials))
        course {:id id :name title :description description :ownerId created-by :creationTime created-on}]
    (-> (http/post url {:body (json/generate-string course)
                        :headers {\"Content-Type\" \"application/json\"}})
        :body
        json/parse-string)))
(defn delete-course [credentials id]
  (let [url (str \"https://www.googleapis.com/classroom/v1/courses/\" id \"?access_token=\" (:access-token credentials))]
    (http/delete url)))
(defn create-assignment [credentials id title description course created-by created-on]
  (let [url (str \"https://www.googleapis.com/classroom/v1/courses/\" course \"/courseWork\" \"?access_token=\" (:access-token credentials))
        assignment {:id id :title title :description description :courseId course :creatorUserId created-by :creationTime created-on}]
    (-> (http/post url {:body (json/generate-string assignment)
                        :headers {\"Content-Type\" \"application/json\"}})
        :body
        json/parse-string)))
(defn delete-assignment [credentials id]
  (let [url (str \"https://www.googleapis.com/classroom/v1/courses/\" course \"/courseWork/\" id \"?access_token=\" (:access-token credentials))]
    (http/delete url)))
(defn get-assignments [credentials course-id]
  (let [url (str \"https://www.googleapis.com/classroom/v1/courses/\" course-id \"/courseWork\" \"?access_token=\" (:access-token credentials))]
    (-> (http/get url)
        :body
        json/parse-string)))
```"}
   {:role "user" :content "Generate a resolver for Box. Which can directly perform actions like:
                           list-items-in-folder, download-file, download-folder, get-folder-information, etc.

                           The request for the API can be:

                           POST \"_e/Box/GetFolderInfo\"
                           {\n    \"Box/GetFolderInfo\": {\n        \"FolderId\": \"246847679929\",\n        \"Path\": \"/Users/prertik/Developer/fractl/\"\n    }\n}
                           "}
   {:role "assistant" :content "Assistant's response: Box Resolver:
File 1(Resolver):
```clojure
(ns box-resolver.resolver
  (:require [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [compose-resolver]]
            [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [clojure.java.io :as io]
            [fractl.util :as u]
            [box-resolver.client :as client])
  (:use [fractl.lang]))

(component :Box
           {:clj-import (quote [(:require [box-resolver.client])])})

(entity :Box/Folder
        {:Id          {:type    :String
                       :guid    true
                       :default u/uuid-string}
         :FolderId    {:type :String
                       :id   true}
         :Type        {:type    :String
                       :default \"folder\"}
         :Name        :String
         :Description :String})

(entity :Box/File
        {:Id        {:type    :String
                     :guid    true
                     :default u/uuid-string}
         :FileId    {:type :String
                     :id   true}
         :Type      {:type    :String
                     :default \"file\"}
         :Name      :String})

(relationship :Box/FolderContainsFile
              {:meta {:contains [:Box/Folder :Box/File]}})

(event :Box/Action
       {:Type              :String
        :FolderId          {:type     :String
                            :optional true}
        :FolderName        {:type     :String
                            :optional true}
        :FileId            {:type     :String
                            :optional true}
        :FileName          {:type     :String
                            :optional true}
        :CollaborationId   {:type     :String
                            :optional true}
        :CollaborationRole {:type     :String
                            :optional true}
        :UserEmail         {:type     :String
                            :optional true}})

(event :Box/LinkFileToFolder
       {:FileId    :String
        :FolderId  :String
        :Name      :String})

(dataflow :Box/LinkFileToFolder
          {:Box/Folder {:FolderId? :Box/LinkFileToFolder.FolderId} :as [:F]}
          {:Box/File
           {:FileId    :Box/LinkFileToFolder.FileId
            :Name      :Box/LinkFileToFolder.Name}
           :-> [[:Box/FolderContainsFile :F]]})

(event :Box/Download
       {:Type :String
        :Id   :String
        :Path :String})

(dataflow :Box/Download
          [:eval '(client/download-respective :Box/Download.Type :Box/Download.Id :Box/Download.Path)])

(event :Box/DownloadAllFiles
       {:FolderId :String
        :Path :String})

(dataflow :Box/DownloadAllFiles
          [:eval '(client/download-all-files :Box/DownloadAllFiles.FolderId :Box/DownloadAllFiles.Path)])

(event :Box/GetFolderInfo
       {:FolderId :String})

(dataflow :Box/GetFolderInfo
          [:eval '(client/get-folder-information :Box/GetFolderInfo.FolderId)])

(event :Box/ListFolderItems
       {:FolderId :String})

(dataflow :Box/ListFolderItems
          [:eval '(client/get-folder-items :Box/ListFolderItems.FolderId)])

(defn- box-eval[inst]
  (println \"The inst is: \" inst)
  (let [[component member :as path] (li/split-path (get inst :-*-type-*-))]
    (println \"The member is: \" member)))

(def box-resolver (r/make-resolver :box {:eval box-eval}))

(compose-resolver
  :Box/Action box-resolver)
```


File 2(Client):
```clojure
(ns box-resolver.client
  (:require [clojure.java.io :as io]
            [fractl.util :as util]
            [fractl.component :as cn]
            [fractl.util.http :as http]
            [fractl.datafmt.json :as json]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]))

(def ^:private box-api-url \"https://api.box.com/2.0\")

(def ^:private authorization-token (util/getenv \"BOX_AUTHORIZATION_TOKEN\"))

(defn get-folder-information [folder-id]
  (http/do-get
    (str box-api-url \"/folders/\" folder-id)
    {:headers {\"content-type\"  \"application/json\"
               \"authorization\" (str \"Bearer \" authorization-token)}}
    :json
    (fn [{body :body :as response}]
      (let [decoded-body (json/decode body)]
        (if (not (nil? decoded-body))
          (let [map-o-data {:FolderId    (get decoded-body :id)
                            :Type        (get decoded-body :type)
                            :Name        (get decoded-body :name)
                            :Description (get decoded-body :description)}]
            (cn/make-instance :Box/Folder map-o-data))
          (throw (Exception. (str \"Error getting folder information: \" response))))))))

(defn get-folder-items [folder-id]
  (http/do-get
    (str box-api-url \"/folders/\" folder-id \"/items\")
    {:headers {\"content-type\"  \"application/json\"
               \"authorization\" (str \"Bearer \" authorization-token)}}
    :json
    (fn [{body :body :as response}]
      (let [decoded-body (json/decode body)]
        (if (not (nil? decoded-body))
          (let [entries (get decoded-body :entries)]
            (map (fn [entry]
                   (let [map-o-data {:FileId    (get entry :id)
                                     :FolderId  folder-id
                                     :Name      (get entry :name)}]
                     (cn/make-instance :Box/LinkFileToFolder map-o-data)))
                 entries))
          (throw (Exception. (str \"Error getting folder items: \" response))))))))

(defn get-file-name [response]
  (let [content-disposition (get-in response [:headers :content-disposition])
        filename (second (re-find #\"filename=\\\"(.*)\\\"\" content-disposition))]
   filename))

(defn download-file [file-id path]
  (http/do-get
      (str box-api-url \"/files/\" file-id \"/content\")
      {:headers {\"content-type\"  \"application/json\"
                 \"authorization\" (str \"Bearer \" authorization-token)}
       :follow-redirects true}
      :json
      (fn [response]
        (if (get response :status)
          (let [url (get-in response [:opts :url])
                file (io/file (str path (get-file-name response)))]
            (with-open [in (io/input-stream url)
                        out (io/output-stream file)]
              (io/copy in out)))
          (throw (Exception. (str \"Downloading URL cannot be found \" response)))))))

(defn download-folder [folder-id path]
  (http/do-post
    (str box-api-url \"/zip_downloads\")
    {:headers {\"content-type\"  \"application/json\"
               \"authorization\" (str \"Bearer \" authorization-token)}}
    {:items [{:type \"folder\" :id (str folder-id)}]}
    :json
    (fn [{body :body :as response}]
      (let [decoded-body (json/decode body)]
        (if (not (nil? decoded-body))
          (http/do-get
            (get decoded-body :download_url)
            {:headers {\"content-type\"  \"application/json\"
                       \"authorization\" (str \"Bearer \" authorization-token)}
             :follow-redirects true}
            :json
            (fn [response]
              (if (get response :status)
                (let [url (get-in response [:opts :url])
                      file (io/file (str path (get-file-name response)))]
                  (with-open [in (io/input-stream url)
                              out (io/output-stream file)]
                    (io/copy in out)))
                (throw (Exception. (str \"Downloading zip URL cannot be found \" response))))))
          (throw (Exception. (str \"Error downloading zip folder: \" response))))))))

(defn download-respective [type id path]
  (if (= type \"folder\")
    (download-folder id path)
    (download-file id path)))

(defn download-all-files [folder-id path]
  (http/do-get
    (str box-api-url \"/folders/\" folder-id \"/items\")
    {:headers {\"content-type\"  \"application/json\"
               \"authorization\" (str \"Bearer \" authorization-token)}}
    :json
    (fn [{body :body :as response}]
      (let [decoded-body (json/decode body)]
        (if (not (nil? decoded-body))
          (let [entries (get decoded-body :entries)]
            (map (fn [entry]
                   (let [map-o-data {:FileId    (get entry :id)
                                     :FolderId  folder-id
                                     :Name      (get entry :name)}]
                     (cn/make-instance :Box/LinkFileToFolder map-o-data)
                     (download-file (get entry :id) path)))
                 entries))
          (throw (Exception. (str \"Error getting folder items: \" response))))))))

(defn get-collaboration [id]
  (http/do-get
    (str box-api-url \"/collaborations/\" id)
    {:headers {\"content-type\"  \"application/json\"
               \"authorization\" (str \"Bearer \" authorization-token)}}
    :json
    (fn [{body :body :as response}]
      (let [decoded-body (json/decode body)]
        (if (not (nil? decoded-body))
          decoded-body
          (throw (Exception. (str \"Error getting collaboration: \" response))))))))

(defn create-collaboration [folder-id login role]
  (http/do-post
    (str box-api-url \"/collaborations\")
    {:headers {\"content-type\"  \"application/json\"
               \"authorization\" (str \"Bearer \" authorization-token)}}
    {:item          {:type \"folder\" :id folder-id}
     :accessible_by {:type \"user\" :login login}
     :role          role}
    :json
    (fn [{body :body :as response}]
      (let [decoded-body (json/decode body)]
        (if (not (nil? decoded-body))
          decoded-body
          (throw (Exception. (str \"Error creating collaboration: \" response))))))))

(defn update-collaboration [id role]
  (http/do-post
    (str box-api-url \"/collaborations/\" id)
    {:headers {\"content-type\"  \"application/json\"
               \"authorization\" (str \"Bearer \" authorization-token)}}
    {:role role}
    :json
    (fn [{body :body :as response}]
      (let [decoded-body (json/decode body)]
        (if (not (nil? decoded-body))
          decoded-body
          (throw (Exception. (str \"Error updating collaboration: \" response))))))))

(defn list-folder-collaborations [folder-id]
  (http/do-get
    (str box-api-url \"/folders/\" folder-id \"/collaborations\")
    {:headers {\"content-type\"  \"application/json\"
               \"authorization\" (str \"Bearer \" authorization-token)}}
    :json
    (fn [{body :body :as response}]
      (let [decoded-body (json/decode body)]
        (if (not (nil? decoded-body))
          decoded-body
          (throw (Exception. (str \"Error listing folder collaborations: \" response))))))))

(defn list-file-collaborations [file-id]
  (http/do-get
    (str box-api-url \"/folders/\" file-id \"/collaborations\")
    {:headers {\"content-type\"  \"application/json\"
               \"authorization\" (str \"Bearer \" authorization-token)}}
    :json
    (fn [{body :body :as response}]
      (let [decoded-body (json/decode body)]
        (if (not (nil? decoded-body))
          decoded-body
          (throw (Exception. (str \"Error listing file collaborations: \" response))))))))
```
"}])

