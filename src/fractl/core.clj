(ns fractl.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :as pprint]
            [nrepl.middleware.dynamic-loader :as dynamic-loader]
            nrepl.middleware
            nrepl.middleware.completion
            nrepl.middleware.load-file
            nrepl.middleware.lookup
            nrepl.middleware.session
            nrepl.middleware.sideloader
            nrepl.server
            fractl.lang.tools.nrepl.middleware.interruptible-eval
            [fractl.datafmt.json :as json]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.util.logger :as log]
            [fractl.http :as h]
            [fractl.component :as cn]
            [fractl.store.migration :as mg]
            [fractl.global-state :as gs]
            [fractl.lang.internal :as li]
            [fractl.lang.tools.build :as build]
            [fractl.lang.tools.deploy :as d]
            [fractl.lang.tools.repl :as repl]
            [fractl.gpt.core :as gpt]
            [fractl.inference]
            [fractl.swagger.doc :as doc]
            [fractl.graphql.core :as gc]
            [fractl.graphql.generator :as gn]
            [fractl.swagger.docindex :as docindex]
            [fractl.graphql.generator :as gg]
            [fractl.util.core :as uc]
            [fractl.lang.tools.nrepl.core :as nrepl])
  (:import [java.util Properties]
           [java.io File]
           [org.apache.commons.exec CommandLine Executor DefaultExecutor])

  (:gen-class
   :name fractl.core
   :methods [#^{:static true} [process_request [Object Object] clojure.lang.IFn]]))

(defn run-service
  ([args model-info]
   (let [[[evaluator _] config] (uc/prepare-runtime args model-info)]
     (when-let [server-cfg (uc/make-server-config config)]
       (log/info (str "Server config - " server-cfg))
       (h/run-server evaluator server-cfg))))
  ([model-info] (run-service nil model-info)))

(defn generate-swagger-doc [model-name args]
  (let [model-path (first args)]
    (if (build/compiled-model? model-path model-name)
      (let [components (remove (cn/internal-component-names)
                               (cn/component-names))]
        (.mkdir (File. "doc"))
        (.mkdir (File. "doc/api"))

        (docindex/gen-index-file model-name components)

        (doseq [component components]
          (let [comp-name (clojure.string/replace
                           (name component) "." "")
                doc-path "doc/api/"
                json-file (str doc-path comp-name ".json")
                html-file (str doc-path comp-name ".html")]
            (with-open [w (clojure.java.io/writer
                           json-file)]
              (.write w (doc/generate-swagger-json component)))
            (let [^CommandLine cmd-line
                  (CommandLine/parse
                   (str "redoc-cli bundle -o " html-file " " json-file))
                  ^Executor executor (DefaultExecutor.)]
              (.execute executor cmd-line))))
        (uc/log-seq! "components" components))
      (build/exec-with-build-model (str "lein run -s " model-name " .") nil model-name))))

(defn generate-graphql-schema [model-name args]
  (let [model-path (first args)]
    (if (build/compiled-model? model-path model-name)
      (let [components (remove (cn/internal-component-names)
                                 (cn/component-names))]
          (doseq [component components]
            (let [comp-name (clojure.string/replace
                             (name component) "." "")]
              (gc/save-schema (first (gg/generate-graphql-schema (cn/schema-info component))) "graphql-schema.edn")))
          (uc/log-seq! "components" components)
        (log/info "Finished processing compiled model."))
      (do
        (log/error (str "Compiled model not found, executing build model for model-name:" model-name))
        (build/exec-with-build-model (str "lein run -g " model-name " .") nil model-name)
        (log/info (str "Finished executing build model for model-name:" model-name))))))

(defn initialize []
  (System/setProperties
   (doto (Properties. (System/getProperties))
     (.put "com.mchange.v2.log.MLog" "com.mchange.v2.log.FallbackMLog")
     (.put "com.mchange.v2.log.FallbackMLog.DEFAULT_CUTOFF_LEVEL" "OFF"))))

(defn run-script
  ([script-names options]
   (let [options (if (uc/config-data-key options)
                   options
                   (second (uc/merge-options-with-config options)))]
     (run-service
      script-names
      (uc/read-model-and-config script-names options))))
  ([script-names]
   (run-script script-names {:config "config.edn"})))

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
                      (or @uc/resource-cache (uc/load-model-from-resource))]
                  (when-not (seq components)
                    (u/throw-ex (str "no components loaded from model " model)))
                  (first (uc/init-runtime model components config)))))
        parsed-request (normalize-external-request request)
        auth (h/make-auth-handler (first @uc/resource-cache))]
    [(json/encode (h/process-request e auth parsed-request)) e]))

(defn -process_request [a b]
  (process_request a b))

(defn- run-plain-option [args opt callback]
  (when (= (first args) (name opt))
    (callback (rest args))
    (first args)))

(defn- publish-library [args]
  (if (= (count args) 1)
    (build/publish-library nil (keyword (first args)))
    (build/publish-library (first args) (keyword (second args)))))

(def ^:private cli-options
  [["-c" "--config CONFIG" "Configuration file"]
   ["-s" "--doc MODEL" "Generate documentation in .html"]
   ["-i" "--interactive 'app-description'" "Invoke AI-assist to model an application"]
   ["-h" "--help"]
   ["-g" "--graphql MODEL" "Generate GraphQL schema for reference"]
   ["-n" "--nrepl" "Start nREPL server"]])

(defn- print-help []
  (println "This is the command-line interface for the Fractl language tool-chain which includes the")
  (println "compiler, runtime, REPL and the code-deployer.")
  (println)
  (println (str "Version: " (gs/fractl-version)))
  (println)
  (println "Usage: fractl [arg*] [command] [MODEL-NAME | SCRIPT]")
  (println)
  (println "Valid commands are: ")
  (println "  run MODEL-NAME             Load and run a model")
  (println "  compile MODEL-NAME         Compile a model into a Clojure project")
  (println "  build MODEL-NAME           Compile a model to produce a standalone application")
  (println "  publish MODEL-NAME TARGET  Publish the model to the target - local, clojars or github")
  (println "  exec MODEL-NAME            Build and run the model as a standalone application")
  (println "  repl MODEL-NAME            Launch the Fractl REPL")
  (println)
  (println "The model will be searched in the local directory or under the paths pointed-to by")
  (println "the `FRACTL_MODEL_PATHS` environment variable. If `MODEL-NAME` is not provided,")
  (println "the fractl command will try to load a model available in the current directory.")
  (println)
  (println "The command-line arguments accepted by fractl are:")
  (println "  -c --config CONFIG          Configuration file")
  (println "  -s --doc MODEL             Generate HTML documentation")
  (println "  -i --interactive 'desc'    Use AI to generate a model from the textual description")
  (println "  -h --help                  Print this help and quit")
  (println "  -g --graphql MODEL         Generate GraphQL schema for reference")
  (println)
  (println "To run a model script, pass the .fractl filename as the command-line argument, with")
  (println "optional configuration (--config)"))

(defn- db-migrate [model-name config]
  ;; config: {:db:migrate {:from "version"}}
  (if-let [mg-config (:db:migrate config)]
    (let [store (uc/store-from-config config)]
      (mg/migrate store model-name mg-config))
    (println "No configuration found for db:migrate.")))

(defn- gpt-bot [request]
  (println (str "Your request: '" request "' is being serviced..."))
  (if request
    (if-let [code (gpt/bot request)]
      (do (pprint/pprint code)
          (System/exit 0))
      (println "ERROR: GPT failed to generate model, please try again."))
    (println "Please enter a description of the app after the -i option.")))

(def default-middleware
  "Middleware vars that are implicitly merged with any additional
   middleware provided to nrepl.server/default-handler."
  [#'nrepl.middleware/wrap-describe
   #'nrepl.middleware.completion/wrap-completion
   #'fractl.lang.tools.nrepl.middleware.interruptible-eval/interruptible-eval
   #'nrepl.middleware.load-file/wrap-load-file
   #'nrepl.middleware.lookup/wrap-lookup
   #'nrepl.middleware.session/add-stdin
   #'nrepl.middleware.session/session
   #'nrepl.middleware.sideloader/wrap-sideloader
   #'nrepl.middleware.dynamic-loader/wrap-dynamic-loader])

(defn fractl-nrepl-handler
  "A handler supporting interruptible evaluation, stdin, sessions,
   readable representations of evaluated expressions via `pr`, sideloading, and
   dynamic loading of middleware.

   Additional middleware to mix into the default stack may be provided; these
   should all be values (usually vars) that have an nREPL middleware descriptor
   in their metadata (see `nrepl.middleware/set-descriptor!`).

   This handler bootstraps by initiating with just the dynamic loader, then
   using that to load the other middleware."
  [model-name options & additional-middleware]
  (nrepl/init-repl-eval-func model-name options)
  (let [initial-handler (dynamic-loader/wrap-dynamic-loader nil)
        state           (atom {:handler initial-handler
                               :stack   [#'nrepl.middleware.dynamic-loader/wrap-dynamic-loader]})]
    (binding [dynamic-loader/*state* state]
      (initial-handler {:op          "swap-middleware"
                        :state       state
                        :middleware (concat default-middleware additional-middleware)}))
    (fn [msg]
      (binding [dynamic-loader/*state* state]
        ((:handler @state) msg)))))

(defn -main [& args]
  (when-not args
    (print-help)
    (System/exit 0))
  (let [{options :options args :arguments
         summary :summary errors :errors} (parse-opts args cli-options)
        [basic-config options] (uc/merge-options-with-config options)]
    (when-let [syslog-cfg (get-in basic-config [:logging :syslog])]
      (log/create-syslogger syslog-cfg))
    (when (get-in basic-config [:logging :dev-mode])
      (log/enable-dev-logging!))
    (initialize)
    (gs/set-app-config! basic-config)
    (cond
      errors (println errors)
      (:help options) (print-help)
      (:graphql options) (generate-graphql-schema
                           (:graphql options)
                           args)
      (:doc options) (generate-swagger-doc
                      (:doc options)
                      args)
      (:interactive options) (gpt-bot (:interactive options))
      :else
      (or (some
           identity
           (map #(apply (partial run-plain-option args) %)
                {:run #(uc/call-after-load-model
                        (first %) (fn [] (run-service (uc/read-model-and-config options))))
                 :compile #(println (build/compile-model (first %)))
                 :build #(println (build/standalone-package (first %)))
                 :exec #(println (build/run-standalone-package (first %)))
                 :repl (uc/run-repl-func options
                                           (fn [model-name opts]
                                             (println (uc/force-call-after-load-model
                                                       model-name
                                                       (fn []
                                                         (let [model-info (uc/read-model-and-config opts)
                                                               [[ev store] _] (uc/prepare-repl-runtime model-info)]
                                                           (repl/run model-name store ev)))))))
                 :nrepl (uc/run-repl-func options
                                            (fn [model-name opts]
                                              (nrepl.server/start-server :port 7888 :handler (fractl-nrepl-handler model-name opts))))
                 :publish #(println (publish-library %))
                 :deploy #(println (d/deploy (:deploy basic-config) (first %)))
                 :db:migrate #(uc/call-after-load-model
                               (first %)
                               (fn []
                                 (db-migrate
                                  (keyword (first %))
                                  (second (uc/read-model-and-config options)))))}))
          (run-script args options)))))
