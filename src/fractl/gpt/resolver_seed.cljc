(ns fractl.gpt.resolver-seed)

(def conversation
  [{:role "system" :content "You are the fractl assistant."},
   {:role "system" :content "All http requests will be handles using the http-kit library, json handling will be done with the cheshire.core library, aws handling will be done with the amazonica library, and system util processes will be done using fractl.util (i.e. getting system environment variables with fractl.util/getenv)"},
   {:role "system" :content "Leave no implementation to the user, add all implementation yourself"},
   {:role "system" :content "All of the resolver functions are to be defined outside of the resolver definition, and will be accessed using the definition below:

(defmake :<resolver-name>   (fn [resolver-name conf]     (reset! config conf)     (r/make-resolver resolver-name {:create {:handler <create-function-name>}    :update {:handler <update-function-name>} 	 :delete {:handler <delete-function-name>} 	 :query  {:handler <query-function-name>}}))) ``` 

Defmake is not an actual call so it needs [fractl.resolver.registry :refer [defmake]] in the requirements as defmake isn’t an actual clojure call. 'r’ in r/make-resolver is also needed as a requirement as [fractl.registry.core :as r]"},
   {:role "user" :content "generate a resolver for airtable that backs a User entity."},
   {:role "assistant" :content
    "(do
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
                            :query  {:handler query-entities}}))))"},
   {:role "user" :content "Instead of handling everything in one file, create a second file to handle the http requests"},
   {:role "assistant" :content
    "(do
      (do
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
                              :query  {:handler query-entities}}))))

    (do
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
          (json/parse-string (:body response) true)))))"}
   {:role "user" :content "in the http file make it so that the *-in-base functions are combined with their request functions for simplicity"},
   {:role "assistant" :content
    "(do
      (do
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
                              :query  {:handler query-entities}}))))
    (do
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
              (json/parse-string (:body response) true)))))"}
   {:role "user" :content "Make it so that query-entities can query either a single record, the whole table, or certain records with parameters"},
   {:role "assistant" :content
    "(do
      (do
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
                              :query  {:handler query-entities}}))))
    (do
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
              (json/parse-string (:body response) true)))))))"}])

