(ns fractl.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [fractl.http :as h])
  (:gen-class))

(def cli-options
  [["-c" "--config CONFIG" "Configuration file"]
   ["-h" "--help"]])

(defn- load-components [component-scripts]
  ;; TODO: load each component
  )

(defn- run-cmd [args config]
  (let [components (load-components args)]
    (when-let [server-cfg (:service config)]
      (h/run-server components server-cfg))))

(defn -main [& args]
  (let [{options :options args :arguments
         summary :summary errors :errors} (parse-opts args cli-options)]
    (cond
      errors (println errors)
      (:help options) (println summary)
      :else (run-cmd args (:config options)))))
