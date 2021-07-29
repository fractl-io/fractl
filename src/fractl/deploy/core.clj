(ns fractl.deploy.core
  (:require [clojure.string :as s]
            [fractl.util :as u]
            [fractl.util.seq :as us]
            [fractl.deploy.util :as ud]
            [fractl.deploy.docker :as docker]
            [fractl.deploy.awscs :as awscs])
  (:use [clojure.java.shell :only [sh]])
  (:import [java.io File]
           [fractl.filesystem Util]))

(def ^:private runtime-jar-pattern "fractl*standalone*.jar")

(def ^:private deploy-fns
  {:container
   {:docker docker/generate-container}
   :repository
   {:aws {:open awscs/repository-client
          :create awscs/create-repository}}
   :cluster
   {:aws {:create awscs/create-cluster}}})

(defn- assoc-defaults [config]
  (-> (us/maybe-assoc config :container :docker)
      (us/maybe-assoc :repository :aws)
      (us/maybe-assoc :cluster :aws)))

(defn- get-deploy-fn [config k]
  (if-let [f (get-in deploy-fns [k (k config)])]
    f
    (u/throw-ex (str "no deployment action associated with " k))))

(defn- find-runtime-jar []
  (if-let [^File jar
           (first
            (concat
             (Util/listFilesByName "target" runtime-jar-pattern)
             (Util/listFilesByName "." runtime-jar-pattern)))]
    (.getAbsolutePath jar)
    (u/throw-ex "runtime jar file not found")))

(defn- upload-image [model-name region repository]
  (let [uri (:uri repository)
        root-uri (first (s/split uri #"/"))
        image-name (str model-name ":latest")
        dest (str uri ":latest")
        aws-auth (str "aws ecr get-login-password --region "
                      region " | docker login --username AWS --password-stdin "
                      root-uri)]
    (ud/run-shell-command ["/bin/sh" "-c" aws-auth])
    (ud/run-shell-command ["docker" "tag" image-name dest])
    (ud/run-shell-command ["docker" "push" dest])
    dest))

(defn deploy [model-dir config]
  (let [config (assoc-defaults config)
        model-name (last (s/split model-dir (re-pattern u/path-sep)))
        dfn (partial get-deploy-fn config)
        region (get config :region "us-east-2")]
    (when ((dfn :container)
           model-name (find-runtime-jar) model-dir)
      (let [repo (dfn :repository)
            conn ((:open repo) region)
            image-name (upload-image
                        model-name region
                        ((:create repo) conn (str model-name "-repository")))
            create-cluster (:create (dfn :cluster))]
        (create-cluster config region model-name image-name)))))
