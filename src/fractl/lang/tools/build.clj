(ns fractl.lang.tools.build
  "Compile a fractl-model to a Java binary package"
  (:require [clojure.string :as s]
            [clojure.pprint :as pprint]
            [clojure.java.io :as io]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.lang.tools.loader :as loader])
  (:import [java.io File]
           [org.apache.commons.io FileUtils]
           [org.apache.commons.exec CommandLine Executor DefaultExecutor]))

(defn- get-system-model-paths []
  (if-let [paths (System/getenv "FRACTL_MODEL_PATHS")]
    (s/split paths #":")
    ["."]))

(def cljout "cljout")
(def ^:private cljout-file (File. cljout))

(defn- clj-io [model-name]
  (let [prefix (str cljout u/path-sep model-name u/path-sep)]
    [#(read-string (slurp (str prefix %)))
     #(pprint/pprint %2 (io/writer (str prefix %1)))]))

(defn- create-clj-project [model-name]
  (let [cmd (str "lein new fractl-model " model-name)
        ^CommandLine cmd-line (CommandLine/parse cmd)
        ^Executor executor (DefaultExecutor.)]
    (.setWorkingDirectory executor cljout-file)
    (zero? (.execute executor cmd-line))))

(defn- finalize-project-spec [model project-spec]
  (when-let [deps (:clj-dependencies model)]
    (loop [spec project-spec, final-spec []]
      (if-let [s (first spec)]
        (if (= :dependencies s)
          (recur (rest (rest spec))
                 (conj
                  final-spec :dependencies
                  (vec (concat (second spec) deps))))
          (recur (rest spec) (conj final-spec s)))
        (seq final-spec)))))

(defn- build-clj-project [model-name model-root model components]
  (if (create-clj-project model-name)
    (let [[rd wr] (clj-io model-name)]
      (when-let [spec (finalize-project-spec model (rd "project.clj"))]
        (wr "project.clj" spec)))
    (log/error (str "failed to create clj project for " model-name))))

(defn build-model
  ([model-paths model-name]
   (let [[model model-root] (loader/read-model model-paths model-name)
         components (loader/read-components-from-model model model-root)]
     (FileUtils/deleteDirectory cljout-file)
     (if (.mkdir cljout-file)
       (build-clj-project model-name model-root model components)
       (u/throw-ex (str "failed to create project directory - " cljout)))))
  ([model-name]
   (build-model (get-system-model-paths) model-name)))
