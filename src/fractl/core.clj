(ns fractl.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [cheshire.core :as json]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.util.logger :as log]
            [fractl.http :as h]
            [fractl.resolver.registry :as rr]
            [fractl.policy.rbac :as rbac]
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.store :as store]
            [fractl.global-state :as gs]
            [fractl.lang :as ln]
            [fractl.lang.internal :as li]
            [fractl.lang.loader :as loader])
  (:import [java.util Properties]
           [java.net URL]
           [java.io File])
  (:gen-class
   :name fractl.core
   :methods [#^{:static true} [process_request [Object Object] clojure.lang.IFn]]))

(def cli-options
  [["-c" "--config CONFIG" "Configuration file"]
   ["-h" "--help"]])

(defn- find-model-paths [model current-model-paths config]
  (let [mpkey :model-paths
        mp (or (mpkey model)
               (mpkey config)
               ".")]
    (set
     (concat
      current-model-paths
      (if (vector? mp)
        mp
        [mp])))))

(defn- script-name-from-component-name [component-name]
  (loop [s (subs (str component-name) 1), sep "", result []]
    (if-let [c (first s)]
      (cond
        (Character/isUpperCase c) (recur (rest s) "_" (conj result sep (Character/toLowerCase c)))
        (= \/ c) (recur (rest s) "" (conj result java.io.File/separator))
        :else (recur (rest s) sep (conj result c)))
      (str (s/join result) (u/get-script-extn)))))

(defn- load-components [component-scripts model-root load-from-resource]
  (mapv
   #(loader/load-script
     model-root
     (if load-from-resource
       (io/resource (str "model/" %))
       %))
   component-scripts))

(defn- load-components-from-model [model model-root load-from-resource]
  (load-components
   (mapv script-name-from-component-name (:components model))
   model-root load-from-resource))

(defn read-model [model-file]
  [(read-string (slurp model-file))
   (.getParent
    (java.io.File. (.getParent (java.io.File. model-file))))])

(defn- read-model-from-paths [model-paths model-name]
  (let [s (s/lower-case (name model-name))]
    (loop [mps model-paths]
      (if-let [mp (first mps)]
        (let [p (str mp u/path-sep s u/path-sep (u/get-model-script-name))]
          (if (.exists (java.io.File. p))
            (read-model p)
            (recur (rest mps))))
        (u/throw-ex
         (str model-name " - model not found in any of "
              model-paths))))))

(defn load-model [model model-root model-paths config]
  (let [nm (s/lower-case (name (:name model)))
        model-paths (find-model-paths model model-paths config)
        rmp (partial read-model-from-paths model-paths)]
    (doseq [d (:dependencies model)]
      (let [[m mr] (rmp d)]
        (load-model m mr model-paths config)))
    (load-components-from-model
     model model-root
     (:load-model-from-resource config))))

(defn- log-seq! [prefix xs]
  (loop [xs xs, sep "", s (str prefix " - ")]
    (when-let [c (first xs)]
      (let [s (str s sep c)]
        (if-let [cs (seq (rest xs))]
          (recur cs " " s)
          (log/info s))))))

(defn- register-resolvers! [resolver-specs]
  (when-let [rns (rr/register-resolvers resolver-specs)]
    (log-seq! "Resolvers" rns)))

(defn- maybe-read-model [args]
  (when (and (= (count args) 1)
             (s/ends-with? (first args) (u/get-model-script-name)))
    (read-model (first args))))

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

(defn- run-appinit-tasks! [evaluator store model components]
  (trigger-appinit-event! evaluator (:init-data model)))

(defn- init-runtime [model components config]
  (register-resolvers! (:resolvers config))
  (let [store (e/store-from-config (:store config))
        ev (e/public-evaluator store true)]
    (run-appinit-tasks! ev store model components)
    (rbac/init!)
    (e/zero-trust-rbac!
     (let [f (:zero-trust-rbac config)]
       (or (nil? f) f)))
    [ev store]))

(defn- finalize-config [model config]
  (let [final-config (merge (:config model) config)]
    (gs/merge-app-config! final-config)
    final-config))

(defn run-service [args [[model model-root] config]]
  (let [config (finalize-config model config)
        components (if model
                     (load-model model model-root nil config)
                     (load-components args (:component-root config) false))]
    (when (and (seq components) (every? keyword? components))
      (log-seq! "Components" components)
      (when-let [server-cfg (:service config)]
        (let [[evaluator store] (init-runtime model components config)
              query-fn (e/query-fn store)]
          (log/info (str "Server config - " server-cfg))
          (h/run-server [evaluator query-fn] server-cfg))))))

(defn read-model-and-config [args options]
  (let [config-file (get options :config)
        config (when config-file
                 (read-string (slurp config-file)))]
    (when-let [extn (:script-extn config)]
      (u/set-script-extn! extn))
    (let [[model _ :as m] (maybe-read-model args)]
      [m (merge (:config model) config)])))

(defn- read-model-from-resource [component-root]
  (if-let [model (read-string
                  (slurp
                   (io/resource
                    (str "model/" component-root "/" (u/get-model-script-name)))))]
    model
    (u/throw-ex (str "failed to load model from " component-root))))

(defn load-model-from-resource []
  (when-let [cfgres (io/resource "config.edn")]
    (let [config (read-string (slurp cfgres))]
      (if-let [component-root (:component-root config)]
        (let [model (read-model-from-resource component-root)
              config (merge (:config model) config)
              components (load-model
                          model component-root nil
                          (assoc config :load-model-from-resource true))]
          (when (seq components)
            (log-seq! "Components loaded from resources" components)
            [config model components]))
        (u/throw-ex "component-root not defined in config")))))

(defn initialize []
  (System/setProperties
   (doto (Properties. (System/getProperties))
     (.put "com.mchange.v2.log.MLog" "com.mchange.v2.log.FallbackMLog")
     (.put "com.mchange.v2.log.FallbackMLog.DEFAULT_CUTOFF_LEVEL" "OFF"))))

(defn- attach-params [request]
  (if (:params request)
    request
    (let [inst (json/parse-string (:body request) true)
          [c n] (li/split-path (first (keys inst)))]
      (assoc request :body inst :params {:component c :event n}))))

(defn- normalize-external-request [request]
  (attach-params
   (if (string? request)
     (json/parse-string request true)
     (su/keys-as-keywords request))))

(defn process_request [evaluator request]
  (let [e (or evaluator
              (do
                (initialize)
                (let [[config model components] (load-model-from-resource)]
                  (when-not (seq components)
                    (u/throw-ex (str "no components loaded from model " model)))
                  (first (init-runtime model components config)))))
        parsed-request (normalize-external-request request)]
    [(json/generate-string (h/process-request e parsed-request)) e]))

(defn -process_request [a b]
  (process_request a b))

(defn -main [& args]
  (initialize)
  (let [{options :options args :arguments
         summary :summary errors :errors} (parse-opts args cli-options)]
    (cond
      errors (println errors)
      (:help options) (println summary)
      :else (run-service args (read-model-and-config args options)))))
