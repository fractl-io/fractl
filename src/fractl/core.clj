(ns fractl.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as s]
            [fractl.util.logger :as log]
            [fractl.http :as h]
            [fractl.resolver.registry :as rr]
            [fractl.policy.rbac :as rbac]
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.deploy.core :as d]
            [fractl.lang.loader :as loader])
  (:gen-class))

(def script-extn ".fractl")
(def model-script-name "model.fractl")

(def cli-options
  [["-c" "--config CONFIG" "Configuration file"]
   ["-d" "--deploy MODEL-DIRECTORY" "Root directory of the model to deploy"]
   ["-h" "--help"]])

(defn- script-name-from-component-name [component-name]
  (loop [s (subs (str component-name) 1), sep "", result []]
    (if-let [c (first s)]
      (cond
        (Character/isUpperCase c) (recur (rest s) "_" (conj result sep (Character/toLowerCase c)))
        (= \/ c) (recur (rest s) "" (conj result java.io.File/separator))
        :else (recur (rest s) sep (conj result c)))
      (str (s/join result) script-extn))))

(defn- load-components [component-scripts component-root-path]
  (doall (map (partial loader/load-script component-root-path)
              component-scripts)))

(defn- load-components-from-model [model component-root-path]
  (load-components (map script-name-from-component-name (:components model))
                   component-root-path))

(defn- log-seq! [prefix xs]
  (loop [xs xs, sep "", s (str prefix " - ")]
    (when-let [c (first xs)]
      (let [s (str s sep c)]
        (if-let [cs (seq (rest xs))]
          (recur cs " " s)
          (log/info s))))))

(defn- register-resolvers! [resolver-specs]
  (when-let [rns (seq (rr/register-resolvers resolver-specs))]
    (log-seq! "Resolvers" rns)))

(defn- maybe-load-model [args]
  (when (and (= (count args) 1) (s/ends-with? (first args) model-script-name))
    (read-string (slurp (first args)))))

(defn- log-app-init-result! [result]
  (cond
    (map? result)
    (let [f (if (= :ok (:status result))
              log/info
              log/error)]
      (f (str "app-init: " result)))

    (seqable? result)
    (doseq [r result] (log-app-init-result! r))

    :else (log/error (str "app-init: " result))))

(defn- trigger-appinit-event! [evaluator data]
  (let [result (evaluator
                (cn/make-instance
                 {:Kernel/AppInit
                  {:Data (or data {})}}))]
    (log-app-init-result! result)))

(defn- run-cmd [args config]
  (let [model (maybe-load-model args)
        config (merge (:config model) config)
        comp-root (:component-root config)
        components (if model
                     (load-components-from-model model comp-root)
                     (load-components args comp-root))]
    (when (and (seq components) (every? keyword? components))
      (log-seq! "Components" components)
      (register-resolvers! (:resolvers config))
      (let [ev (e/public-evaluator (:store config) true)]
        (trigger-appinit-event! ev (:init-data model))
        (rbac/init!)
        (e/zero-trust-rbac!
         (let [f (:zero-trust-rbac config)]
           (or (nil? f) f)))
        (when-let [server-cfg (:service config)]
          (log/info (str "Server config - " server-cfg))
          (h/run-server ev server-cfg))))))

(defn- read-config [options]
  (when-let [config-file (get options :config)]
    (read-string (slurp config-file))))

(defn -main [& args]
  (let [{options :options args :arguments
         summary :summary errors :errors} (parse-opts args cli-options)]
    (cond
      errors (println errors)
      (:deploy options) (d/deploy (:deploy options))
      (:help options) (println summary)
      :else (run-cmd args (read-config options)))))
