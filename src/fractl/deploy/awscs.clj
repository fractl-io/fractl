(ns fractl.deploy.awscs
  "AWS container services"
  (:require [fractl.util :as u])
  (:import [fractl.deploy.aws Container]))

(defn repository-client [region]
  (Container/buildClient region))

(defn- create-repository-response-as-map [r]
  (into
   {}
   (map
    (fn [[k v]] [(keyword k) v])
    (Container/createRepositoryResponseAsMap r))))

(defn- handle-create-repo-result [[r err]]
  (if err
    (u/throw-ex (str "failed to create AWS repository: " err))
    (create-repository-response-as-map r)))

(defn create-repository [client repo-name]
  (let [result (promise)
        fetch #(deref result 100 nil)]
    (Container/createRepository
     client repo-name
     #(deliver result [%1 %2]))
    (loop [r (fetch)]
      (if r
        (handle-create-repo-result r)
        (recur (fetch))))))
