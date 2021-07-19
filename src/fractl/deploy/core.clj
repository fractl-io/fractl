(ns fractl.deploy.core
  (:require [clojure.string :as s]
            [fractl.util :as u]
            [fractl.util.seq :as us]
            [fractl.deploy.docker :as docker]
            [fractl.deploy.awscs :as awscs])
  (:import [java.io File]
           [fractl.filesystem Util]))

(def ^:private runtime-jar-pattern "fractl*standalone*.jar")

(def ^:private deploy-fns
  {:container
   {:docker docker/generate-container}
   :repository
   {:aws {:open awscs/repository-client
          :create awscs/create-repository}}})

(defn- assoc-defaults [config]
  (-> (us/maybe-assoc config :container :docker)
      (us/maybe-assoc :repository :aws)))

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

(defn deploy [model-dir config]
  (let [config (assoc-defaults config)
        model-name (last (s/split model-dir (re-pattern u/path-sep)))
        dfn (partial get-deploy-fn config)]
    (when ((dfn :container)
           model-name (find-runtime-jar) model-dir)
      (let [repo (dfn :repository)
            conn ((:open repo) config)]
        ((:create repo) conn (str model-name "-repository"))))))
