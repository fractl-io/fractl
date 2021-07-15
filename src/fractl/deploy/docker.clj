(ns fractl.deploy.docker
  (:require [clojure.string :as s]
            [fractl.util :as u])
  (:use [clojure.java.shell :only [sh]])
  (:import [java.io File]
           [fractl.filesystem Util]))

(defn- file-info [^File f]
  (vec (.getAbsolutePath f) (.getName f)))

(defn- copy-all-components [model-dir model-name]
  (let [file-paths (doall
                    (map
                     file-info
                     (Util/listFilesByExtn model-dir ".fractl")))]
    (map #(str "COPY " (first %) "/" model-name "/" (second %)) file-paths)))

(defn generate-container [runtime-jar model-dir]
  (let [model-name (last (s/split model-dir (re-pattern u/path-sep)))]
    (spit
     "Dockerfile"
     (apply
      u/concat-lines
      "FROM adoptopenjdk:14-jre-hotspot"
      (str "COPY " runtime-jar "/fractl-runtime.jar")
      (str "COPY " model-dir u/path-sep "config.edn /config.edn")
      (str "RUN mkdir /" model-name)
      (concat
       (copy-all-components model-dir)
       [(str "CMD [\"java\", \"-jar\", \"fractl-runtime.jar\", \"-c\", \"config.edn\", "
             "\"" model-name "/model.fractl\"]")])))
    (sh "docker" "build" "-t" model-name ".")))
