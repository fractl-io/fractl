(ns fractl.example.camel.core
  (:require [fractl.core :as fractl]))

(defn -main [& args]
  (fractl/run-script [(first args)]))
