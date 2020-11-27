(ns fractl.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [taoensso.timbre :as log]
            [fractl.http :as h]
            [fractl.evaluator :as e]
            [fractl.lang.loader :as loader])
  (:gen-class))

(def cli-options
  [["-c" "--config CONFIG" "Configuration file"]
   ["-h" "--help"]])

(defn- load-components [component-scripts component-root-path]
  (doall (map (partial loader/load-script component-root-path)
              component-scripts)))

(defn- log-service-info! [components server-cfg]
  (loop [components components, s "Components - "]
    (if-let [c (first components)]
      (let [cs (rest components)
            sep (if (seq (rest cs)) " " "")]
        (recur cs (str s sep c)))
      (log/info s)))
  (log/info (str "Server config - " server-cfg)))

(defn- run-cmd [args config]
  (let [components (load-components args (:component-root config))]
    (when (every? keyword? components)
      (when-let [server-cfg (:service config)]
        (log-service-info! components server-cfg)
        (h/run-server (e/evaluator) server-cfg)))))

(defn- read-config [options]
  (read-string (slurp (get options :config "./config.edn"))))

(defn -main [& args]
  (let [{options :options args :arguments
         summary :summary errors :errors} (parse-opts args cli-options)]
    (cond
      errors (println errors)
      (:help options) (println summary)
      :else (run-cmd args (read-config options)))))
