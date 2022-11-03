(ns fractl.lang.tools.build
  "Compile a fractl-model to a Java binary package"
  (:require [fractl.lang.tools.loader :as loader]))

(defn build-model
  ([model-paths model-name]
   (loader/read-model model-paths model-name))
  ([model-name]
   (build-model ["."] model-name)))
