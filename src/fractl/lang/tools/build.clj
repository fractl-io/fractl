(ns fractl.lang.tools.build
  "Compile a fractl-model to a Java binary package"
  (:require [clojure.string :as s]
            [fractl.util :as u]
            [fractl.lang.tools.loader :as loader])
  (:import [java.io File]
           [org.apache.commons.io FileUtils]))

(defn- get-system-model-paths []
  (if-let [paths (System/getenv "FRACTL_MODEL_PATHS")]
    (s/split paths #":")
    ["."]))

(def cljout "cljout")
(def ^:private cljout-file (File. cljout))

(defn- create-clj-project [model-name model-root components]
  ;; TODO: create lein template project and customize it
  model-name)

(defn build-model
  ([model-paths model-name]
   (let [[model model-root] (loader/read-model model-paths model-name)
         components (loader/read-components-from-model model model-root)]
     (FileUtils/deleteDirectory cljout-file)
     (if (.mkdir cljout-file)
       (create-clj-project model-name model-root components)
       (u/throw-ex (str "failed to create project directory - " cljout)))))
  ([model-name]
   (build-model (get-system-model-paths) model-name)))
