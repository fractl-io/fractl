(ns fractl.deploy.awscs
  "AWS container services"
  (:require [clojure.string :as s]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.deploy.util :as ud])
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

(def ^:private cluster-spec-yml
  "apiVersion: v1
kind: Service
metadata:
  name: $app-name
spec:
  selector:
    app: $app-name
  ports:
    - port: 8080
      targetPort: 8080
  type: LoadBalancer
---
apiVersion: apps/v1
kind: Deployment
metadata:
  name: $app-name
spec:
  replicas: 1
  selector:
    matchLabels:
      app: $app-name
  template:
    metadata:
      labels:
        app: $app-name
    spec:
      containers:
        - name: $app-name
          image: $image-name
          ports:
            - containerPort: 8080
#          env:
#            - name: <ENV_VAR>
#              value: <env_val>
          imagePullPolicy: Always")

(defn create-cluster [region model-name image-name]
  (let [app-name (str model-name "-" (u/uuid-string))
        cfg (s/replace
             (s/replace cluster-spec-yml #"\$app-name" app-name)
             #"\$image-name" image-name)
        cfg-file (str model-name ".yml")]
    (spit cfg-file cfg)
    (log/info (str "creating cluster " app-name " with image " image-name))
    (ud/run-shell-command ["eksctl" "create" "cluster"
                           (str "--region=" region)
                           (str "--name=" app-name)])
    (ud/run-shell-command ["kubectl" "apply" "-f" cfg-file])
    (ud/run-shell-command-ignore-error ["kubectl" "get" "service" app-name] true)
    (log/info (str "deployed cluster - " app-name))
    (log/info (str "to undeploy, run - `eksctl delete cluster --region=" region " --name=" app-name "`"))
    app-name))
