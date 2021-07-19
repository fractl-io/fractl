(ns fractl.deploy.awscs
  "AWS container services"
  (:require [fractl.util :as u])
  (:import [fractl.deploy.aws Container]))

(defn repository-client [config]
  (Container/buildClient
   (get config :region "us-east-2")))

(defn- handle-create-repo-result [[r err]]
  (when err
    (u/throw-ex (str "failed to create AWS repository: " err)))
  r)

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
