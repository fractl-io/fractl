(ns fractl.deploy.core
  (:require [fractl.util :as u]
            [fractl.deploy.docker :as docker])
  (:import [java.io File]
           [fractl.filesystem Util]))

(def ^:private runtime-jar-pattern "fractl*standalone*.jar")

(defn- find-runtime-jar []
  (if-let [^File jar
           (first
            (concat
             (Util/listFilesByName "target" runtime-jar-pattern)
             (Util/listFilesByName "." runtime-jar-pattern)))]
    (.getAbsolutePath jar)
    (u/throw-ex "runtime jar file not found")))

(defn deploy [model-dir]
  (docker/generate-container
   (find-runtime-jar) model-dir))
