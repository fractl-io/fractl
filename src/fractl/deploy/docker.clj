(ns fractl.deploy.docker
  (:require [clojure.string :as s]
            [fractl.util :as u]
            [fractl.util.logger :as log])
  (:use [clojure.java.shell :only [sh]])
  (:import [java.io File]
           [fractl.filesystem Util]))

(defn- maybe-path-sep [^String dir]
  (if (.endsWith dir u/path-sep)
    ""
    u/path-sep))

(def ^:private ^String pwd
  (let [p (System/getProperty "user.dir")]
    (str p (maybe-path-sep p))))

(def ^:private pwd-len (.length pwd))

(defn- relative-path [^String path]
  (let [i (.indexOf path pwd)]
    (if (zero? i)
      (.substring path pwd-len (.length path))
      path)))

(defn- file-info [^File f]
  [(relative-path (.getAbsolutePath f)) (.getName f)])

(defn- copy-all-components [model-dir model-name]
  (let [file-paths (doall
                    (map
                     file-info
                     (Util/listFilesByExtn model-dir ".fractl")))]
    (map #(str "COPY " (first %) " /" model-name "/" (second %)) file-paths)))

(defn- run-docker [model-name]
  (let [cmd ["docker" "build" "-t" model-name "."]]
    (log/info (s/join " " cmd))
    (let [r (apply sh cmd)
          status (:exit r)]
      (log/info (:out r))
      (if (zero? status)
        true
        (u/throw-ex
         (str "docker command failed with exit code - " status))))))

(defn generate-container [runtime-jar model-dir]
  (log/info (str "generating container - " [runtime-jar model-dir]))
  (let [model-name (last (s/split model-dir (re-pattern u/path-sep)))]
    (spit
     "Dockerfile"
     (apply
      u/concat-lines
      "FROM adoptopenjdk:14-jre-hotspot"
      (str "COPY " (relative-path runtime-jar) " /fractl-runtime.jar")
      (str "COPY " model-dir (maybe-path-sep model-dir)
           "config.edn /config.edn")
      (str "RUN mkdir /" model-name)
      (concat
       (copy-all-components model-dir model-name)
       [(str "CMD [\"java\", \"-jar\", \"fractl-runtime.jar\", \"-c\", \"config.edn\", "
             "\"" model-name "/model.fractl\"]")])))
    (run-docker model-name)))
