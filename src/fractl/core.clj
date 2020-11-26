(ns fractl.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [fractl.http :as h]
            [fractl.lang.loader :as loader])
  (:gen-class))

(def cli-options
  [["-c" "--config CONFIG" "Configuration file"]
   ["-h" "--help"]])

(defn- load-components [component-scripts component-root-path]
  (doall (map (partial loader/load-script component-root-path)
              component-scripts)))

(defn- run-cmd [args config]
  (when (every? keyword? (load-components args (:component-root config)))
    (when-let [server-cfg (:service config)]
      (h/run-server server-cfg))))

(defn- read-config [options]
  (read-string (slurp (get options :config "./config.edn"))))

(defn -main [& args]
  (let [{options :options args :arguments
         summary :summary errors :errors} (parse-opts args cli-options)]
    (cond
      errors (println errors)
      (:help options) (println summary)
      :else (run-cmd args (read-config options)))))
