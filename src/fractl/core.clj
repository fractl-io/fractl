(ns fractl.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [taoensso.timbre :as log]
            [fractl.http :as h]
            [fractl.resolver.registry :as rr]
            [fractl.evaluator :as e]
            [fractl.lang.loader :as loader]
            [fractl.deps :as deps]
            [fractl.util :as util]
            [clojure.string :as string])
  (:gen-class))

(def cli-options
  [["-c" "--config CONFIG" "Configuration file"]
   ["-h" "--help"]
   ["-j" "--jar JAR" "Name and version of jar"]])

(defn- load-components [component-scripts component-root-path]
  (doall (map (partial loader/load-script component-root-path)
              component-scripts)))

(defn- log-seq! [prefix xs]
  (loop [xs xs, s (str prefix " - ")]
    (if-let [c (first xs)]
      (let [cs (rest xs)
            sep (if (seq (rest cs)) " " "")]
        (recur cs (str s sep c)))
      (log/info s))))

(defn- register-resolvers! [resolver-specs]
  (when-let [rns (seq (rr/register-resolvers resolver-specs))]
    (log-seq! "Resolvers" rns)))

(defn- run-cmd [args config]
  (let [components (load-components args (:component-root config))]
    (when (and (seq components) (every? keyword? components))
      (log-seq! "Components" components)
      (register-resolvers! (:resolvers config))
      (when-let [server-cfg (:service config)]
        (log/info (str "Server config - " server-cfg))
        (h/run-server (e/evaluator (:store config)) server-cfg)))))

(defn- read-config [options]
  (read-string (slurp (get options :config "./config.edn"))))

(defn jar-resolver
  [jar cfg]
  (let [prop (jar :jar)
        strprop (str prop)
        name-ver (string/split strprop #"\ ")
        [cname cv] name-ver]
    (if-let [nsnames (deps/component-nslist cname cv)]
      (run-cmd nsnames cfg)
      (util/throw-ex (str "Failed to find namespaces to execute " cname)))))

(defn -main [& args]
  (let [{options :options args :arguments
         summary :summary errors :errors} (parse-opts args cli-options)]
    (cond
      errors (println errors)
      (:help options) (println summary)
      (:jar options) (jar-resolver options (read-config args))
      :else (run-cmd args (read-config options)))))
