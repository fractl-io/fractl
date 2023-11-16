(ns fractl.lang.tools.util
  (:require [clojure.string :as s]))  

(defn get-system-model-paths []
  #?(:clj
     (if-let [paths (System/getenv "FRACTL_MODEL_PATHS")]
       (s/split paths #":")
       ["."])
     :cljs ["."]))
