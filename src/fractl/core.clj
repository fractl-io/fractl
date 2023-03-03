(ns fractl.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [fractl.datafmt.json :as json]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.util.logger :as log]
            [fractl.http :as h]
            [fractl.resolver.registry :as rr]
            [fractl.compiler :as c]
            [fractl.component :as cn]
            [fractl.evaluator :as e]
            [fractl.evaluator.intercept :as ei]
            [fractl.store :as store]
            [fractl.global-state :as gs]
            [fractl.lang :as ln]
            [fractl.lang.internal :as li]
            [fractl.lang.tools.loader :as loader]
            [fractl.lang.tools.build :as build]
            [fractl.lang.tools.deploy :as d]
            [fractl.auth :as auth]
            [fractl.rbac.core :as rbac])
  (:import [java.util Properties]
           [java.net URL]
           [java.io File])
  (:gen-class
   :name fractl.core
   :methods [#^{:static true} [process_request [Object Object] clojure.lang.IFn]]))

(def cli-options
  [["-c" "--config CONFIG" "Configuration file"]
   ["-h" "--help"]])

(defn- complete-model-paths [model current-model-paths config]
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

(defn- store-from-config [config]
  (or (:store-handle config)
      (e/store-from-config (:store config))))

(defn- set-aot-dataflow-compiler! [config]
  (when-let [store (store-from-config config)]
    (cn/set-aot-dataflow-compiler!
     (partial
      c/maybe-compile-dataflow
      (partial store/compile-query store)))))

(defn- load-components [components model-root config]
  (set-aot-dataflow-compiler! config)
  (loader/load-components components model-root))

(defn- load-components-from-model [model model-root config]
  (set-aot-dataflow-compiler! config)
  (loader/load-components-from-model
   model model-root
   (:load-model-from-resource config)))

(defn load-model [model model-root model-paths config]
  (loader/load-model
   model model-root
   (complete-model-paths model model-paths config)
   (:load-model-from-resource config)))

(defn- log-seq! [prefix xs]
  (loop [xs xs, sep "", s (str prefix " - ")]
    (when-let [c (first xs)]
      (let [s (str s sep c)]
        (if-let [cs (seq (rest xs))]
          (recur cs " " s)
          (log/info s))))))

(defn- register-resolvers! [config evaluator]
  (when-let [resolver-specs (:resolvers config)]
    (when-let [rns (rr/register-resolvers resolver-specs)]
      (log-seq! "Resolvers" rns)))
  (when-let [auth-config (:authentication config)]
    (when (auth/setup-resolver auth-config evaluator)
      (log/info "authentication resolver inited"))))

(defn- model-name-from-args [args]
  (and (seq (su/nonils args))
       (= (count args) 1)
       (let [f (first args)]
         (and (s/ends-with? f (u/get-model-script-name))
              f))))

(defn- maybe-read-model [args]
  (when-let [n (and args (model-name-from-args args))]
    (loader/read-model n)))

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
                 {:Fractl.Kernel.Lang/AppInit
                  {:Data (or data {})}}))]
    (log-app-init-result! result)))

(defn- run-appinit-tasks! [evaluator store init-data]
  (trigger-appinit-event! evaluator init-data))

(defn- merge-resolver-configs [app-config resolver-configs]
  (let [app-resolvers (:resolvers app-config)]
    (mapv
     #(let [n (:name %)]
        (if-let [ac (first
                     (filter
                      (fn [x] (= (:name x) n))
                      app-resolvers))]
          (assoc % :config (merge (:config ac) (:config %)))
          %))
     resolver-configs)))

(defn- run-initconfig [app-config evaluator]
  (let [result (evaluator
                (cn/make-instance
                 {:Fractl.Kernel.Lang/InitConfig {}}))
        configs (first (mapv :Data (:result (first result))))
        resolver-configs (merge-resolver-configs
                          app-config
                          (vec
                           (apply
                            concat
                            (mapv :resolvers configs))))
        other-configs (mapv #(dissoc % :resolvers) configs)]
    (merge
     (assoc
      (apply merge other-configs)
      :resolvers resolver-configs)
     (dissoc app-config :resolvers))))

(defn- normalize-interceptors [ins]
  (let [ks (keys ins)]
    (if (and (some #{:rbac} ks)
             (not (some #{:instance-meta} ks)))
      (assoc ins :instance-meta {:enabled true})
      ins)))

(defn- init-runtime [model config]
  (let [store (store-from-config config)
        ev (e/public-evaluator store true)
        ins (normalize-interceptors (:interceptors config))
        resolved-config (run-initconfig config ev)]
    (register-resolvers! config ev)
    (when (seq (:resolvers resolved-config))
      (register-resolvers! resolved-config ev))
    (run-appinit-tasks! ev store (or (:init-data model)
                                     (:init-data config)))
    (when (some #{:rbac} (keys ins))
      (when-not (rbac/init (:rbac ins))
        (log/error "failed to initialize rbac")))
    (ei/init-interceptors ins)
    [ev store]))

(defn- finalize-config [model config]
  (let [final-config (merge (:config model) config)]
    (gs/merge-app-config! final-config)
    final-config))

(defn- make-server-config [app-config]
  (assoc (:service app-config) :authentication
         (:authentication app-config)))

(defn run-service [args [[model model-root] config]]
  (let [config (finalize-config model config)
        store (e/store-from-config (:store config))
        config (assoc config :store-handle store)
        components (or
                    (if model
                      (load-model model model-root nil config)
                      (load-components args (:component-root config) config))
                    (cn/component-names))]
    (when (and (seq components) (every? keyword? components))
      (log-seq! "Components" components))
    (when-let [server-cfg (make-server-config config)]
      (let [[evaluator store] (init-runtime model config)
            query-fn (e/query-fn store)]
        (log/info (str "Server config - " server-cfg))
        (h/run-server [evaluator query-fn] server-cfg)))))

(defn- find-model-to-read [args config]
  (or (seq (su/nonils args))
      [(:full-model-path config)]))

(defn- read-env-var [x]
  (cond
    (symbol? x)
    (when-let [v (System/getenv (name x))]
      (let [s (try
                (read-string v)
                (catch Exception _e v))]
        (cond
          (not= (str s) v) v
          (symbol? s) (str s)
          :else s)))

    (vector? x)
    (first (su/nonils (mapv read-env-var x)))

    :else x))

(defn- read-config-file [config-file]
  (binding [*data-readers* {'$ read-env-var}]
    (read-string (slurp config-file))))

(defn read-model-and-config [args options]
  (let [config-file (get options :config)
        config (when config-file
                 (read-config-file config-file))]
    (when-let [extn (:script-extn config)]
      (u/set-script-extn! extn))
    (let [[model _ :as m] (maybe-read-model (find-model-to-read args config))]
      [m (merge (:config model) config)])))

(defn- read-model-from-resource [component-root]
  (let [^String s (slurp
                   (io/resource
                    (str "model/" component-root "/" (u/get-model-script-name))))]
    (if-let [model (loader/read-model-expressions (io/input-stream (.getBytes s)))]
      model
      (u/throw-ex (str "failed to load model from " component-root)))))

(def ^:private resource-cache (atom nil))

(defn load-model-from-resource []
  (when-let [cfgres (io/resource "config.edn")]
    (let [config (read-string (slurp cfgres))]
      (when-let [extn (:script-extn config)]
        (u/set-script-extn! extn))
      (if-let [component-root (:component-root config)]
        (let [model (read-model-from-resource component-root)
              config (merge (:config model) config)
              components (load-model
                          model component-root nil
                          (assoc config :load-model-from-resource true))]
          (when (seq components)
            (log-seq! "Components loaded from resources" components)
            (let [r [config model components]]
              (reset! resource-cache r) r)))
        (u/throw-ex "component-root not defined in config")))))

(defn initialize []
  (System/setProperties
   (doto (Properties. (System/getProperties))
     (.put "com.mchange.v2.log.MLog" "com.mchange.v2.log.FallbackMLog")
     (.put "com.mchange.v2.log.FallbackMLog.DEFAULT_CUTOFF_LEVEL" "OFF"))))

(defn- attach-params [request]
  (if (:params request)
    request
    (let [inst (if-let [b (:body request)]
                 (json/decode b)
                 request)
          [c n] (li/split-path (first (keys inst)))]
      (assoc request :body inst :params {:component c :event n}))))

(defn- normalize-external-request [request]
  (attach-params
   (if (string? request)
     (json/decode request)
     (su/keys-as-keywords request))))

(defn process_request [evaluator request]
  (let [e (or evaluator
              (do
                (initialize)
                (let [[config model components]
                      (or @resource-cache (load-model-from-resource))]
                  (when-not (seq components)
                    (u/throw-ex (str "no components loaded from model " model)))
                  (first (init-runtime model components config)))))
        parsed-request (normalize-external-request request)
        auth (h/make-auth-handler (first @resource-cache))]
    [(json/encode (h/process-request e auth parsed-request)) e]))

(defn -process_request [a b]
  (process_request a b))

(defn- run-plain-option [args opt callback]
  (when (= (first args) opt)
    (callback (rest args))
    (first args)))

(defn- publish-library [args]
  (if (= (count args) 1)
    (build/publish-library nil (keyword (first args)))
    (build/publish-library (first args) (keyword (second args)))))

(defn- print-help []
  (doseq [opt cli-options]
    (println (str "  " (s/join " " opt))))
  (println "  run MODEL-NAME Load and run a model")
  (println "  compile MODEL-NAME Compile a model into a Clojure project")
  (println "  build MODEL-NAME Compile a model to produce a standalone application")
  (println "  publish MODEL-NAME TARGET Publish the model to one of the targets - `local`, `clojars` or `github`")
  (println "  exec MODEL-NAME build and run the model as a standalone application")
  (println)
  (println "For `run`, `compile`, `build`, `publish` and `exec` the model will be searched in the local directory")
  (println "or under the paths pointed-to by the `FRACTL_MODEL_PATHS` environment variable.")
  (println "If `MODEL-NAME` is not required it the fractl command is executed from within the")
  (println "model directory itself.")
  (println)
  (println "To run a model script, pass the .fractl filename as the command-line argument, with")
  (println "optional configuration (--config)"))

(defn- load-config [options]
  (read-config-file (get options :config "config.edn")))

(defn -main [& args]
  (when-not args
    (print-help)
    (System/exit 0))
  (let [{options :options args :arguments
         summary :summary errors :errors} (parse-opts args cli-options)]
    (initialize)
    (cond
      errors (println errors)
      (:help options) (print-help)
      :else
      (or (some identity (mapv (partial run-plain-option args)
                               ["run" "compile" "build" "exec" "publish" "deploy"]
                               [#(when (build/load-model (first %))
                                   (run-service nil (read-model-and-config nil options)))
                                #(println (build/compile-model (first %)))
                                #(println (build/standalone-package (first %)))
                                #(println (build/run-standalone-package (first %)))
                                #(println (publish-library %))
                                #(println (d/deploy
                                           (:deploy (load-config options))
                                           (first %)))]))
          (run-service args (read-model-and-config args options))))))
