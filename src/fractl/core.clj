(ns fractl.core
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.pprint :as pprint]
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
            [fractl.store.migration :as mg]
            [fractl.global-state :as gs]
            [fractl.lang :as ln]
            [fractl.lang.internal :as li]
            [fractl.lang.rbac :as lr]
            [fractl.lang.tools.loader :as loader]
            [fractl.lang.tools.build :as build]
            [fractl.lang.tools.deploy :as d]
            [fractl.lang.tools.repl :as repl]
            [fractl.auth :as auth]
            [fractl.rbac.core :as rbac]
            [fractl.gpt.core :as gpt]
            [fractl.swagger.doc :as doc]
            [fractl.swagger.docindex :as docindex]
            [fractl-config-secrets-reader.core :as fractl-secret-reader])
  (:import [java.util Properties]
           [java.net URL]
           [java.io File]
           [org.apache.commons.exec CommandLine Executor DefaultExecutor])

  (:gen-class
   :name fractl.core
   :methods [#^{:static true} [process_request [Object Object] clojure.lang.IFn]]))

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

(defn generate-and-save-rsa-keys! []
  (let [keys (u/generate-key-pair)
        pubkey (str "-----BEGIN PUBLIC KEY-----" "\n"
                    (u/encode-base64 (.getEncoded (:public keys))) "\n"
                    "-----END PUBLIC KEY-----")
        privkey (str "-----BEGIN PRIVATE KEY-----" "\n"
                     (u/encode-base64 (.getEncoded (:private keys))) "\n"
                     "-----END PRIVATE KEY-----")]
    (spit "publickey.pem" pubkey)
    (spit "privatekey.pem" privkey)))

(defn- run-appinit-tasks! [evaluator init-data config]
  (when (:authentication config) (generate-and-save-rsa-keys!))
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

(def ^:private repl-mode-key :-*-repl-mode-*-)
(def ^:private repl-mode? repl-mode-key)

(defn- init-runtime [model config]
  (let [store (store-from-config config)
        ev ((if (repl-mode? config)
              e/internal-evaluator
              e/public-evaluator)
            store)
        ins (:interceptors config)]
    (when (store/init-all-schema store)
      (let [resolved-config (run-initconfig config ev)
            has-rbac (some #{:rbac} (keys ins))]
        (register-resolvers! config ev)
        (when (seq (:resolvers resolved-config))
          (register-resolvers! resolved-config ev))
        (if has-rbac
          (lr/finalize-events ev)
          (lr/reset-events!))
        (run-appinit-tasks! ev
                            (or (:init-data model) (:init-data config))
                            config)
        (when has-rbac
          (when-not (rbac/init (merge (:rbac ins) (:authentication config)))
            (log/error "failed to initialize rbac")))
        (ei/init-interceptors ins)
        [ev store]))))

(defn- finalize-config [model config]
  (let [final-config (merge (:config model) config)]
    (gs/merge-app-config! final-config)
    final-config))

(defn- make-server-config [app-config]
  (assoc (:service app-config) :authentication
         (:authentication app-config)))

(defn prepare-runtime
  ([args [[model model-root] config]]
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
     [(init-runtime model config) config]))
  ([model-info] (prepare-runtime nil model-info)))

(defn- prepare-repl-runtime [[[model model-root] config]]
  (prepare-runtime [[model model-root] (assoc config repl-mode-key true)]))

(defn run-service
  ([args model-info]
   (let [[[evaluator _] config] (prepare-runtime args model-info)]
     (when-let [server-cfg (make-server-config config)]
       (log/info (str "Server config - " server-cfg))
       (h/run-server evaluator server-cfg))))
  ([model-info] (run-service nil model-info)))

(defn generate-swagger-doc [model-name args]
  (let [model-path (first args)]
    (if (build/compiled-model? model-path model-name)
      (let [components (remove #{:Fractl.Kernel.Identity :Fractl.Kernel.Lang
                                 :Fractl.Kernel.Store :Fractl.Kernel.Rbac
                                 :raw :-*-containers-*-}
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
        (log-seq! "components" components))
      (build/exec-with-build-model (str "lein run -s " model-name " .") nil model-name))))

(defn- find-model-to-read [args config]
  (or (seq (su/nonils args))
      [(:full-model-path config)]))

(defn- preproc-config [config]
  (if (:rbac-enabled config)
    (let [opt (:service config)
          serv (if-not (find opt :call-post-sign-up-event)
                 (assoc opt :call-post-sign-up-event true)
                 opt)
          auth (or (:authentication config)
                   {:service :cognito
                    :superuser-email (u/getenv "FRACTL_SUPERUSER_EMAIL" "superuser@superuser.com")
                    :whitelist? false})
          opt (:interceptors config)
          inter (if-not (:rbac opt)
                  (assoc opt :rbac {:enabled true})
                  opt)]
      (assoc (dissoc config :rbac-enabled)
             :service serv
             :authentication auth
             :interceptors inter))
    config))

(defn- load-config [options]
  (preproc-config
   (u/read-config-file (get options :config "config.edn"))))

(def ^:private config-data-key :-*-config-data-*-)

(defn read-model-and-config
  ([args options]
   (let [config (or (config-data-key options) (load-config options))]
     (when-let [extn (:script-extn config)]
       (u/set-script-extn! extn))
     (let [[model _ :as m] (maybe-read-model (find-model-to-read args config))
           config (merge (:config model) config)]
       (try
         [m (fractl-secret-reader/read-secret-config config)]
         (catch Exception e
           (u/throw-ex (str "error reading secret config " e)))))))
  ([options] (read-model-and-config nil options)))

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
   ["-h" "--help"]])

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
  (println "  -c --config CONFIG         Configuration file")
  (println "  -s --doc MODEL             Generate HTML documentation")
  (println "  -i --interactive 'desc'    Use AI to generate a model from the textual description")
  (println "  -h --help                  Print this help and quit")
  (println)
  (println "To run a model script, pass the .fractl filename as the command-line argument, with")
  (println "optional configuration (--config)"))

(defn- call-after-load-model
  ([model-name f ignore-load-error]
   (gs/in-script-mode!)
   (when (try
           (build/load-model model-name)
           (catch Exception ex
             (if ignore-load-error true (throw ex))))
     (f)))
  ([model-name f]
   (call-after-load-model model-name f false)))

(defn- force-call-after-load-model [model-name f]
  (try
    (call-after-load-model model-name f)
    (catch Exception ex
      (println (str "ERROR - " (.getMessage ex)))
      (f))))

(defn- db-migrate [model-name config]
  ;; config: {:db:migrate {:from "version"}}
  (if-let [mg-config (:db:migrate config)]
    (let [store (store-from-config config)]
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

(defn -main [& args]
  (when-not args
    (print-help)
    (System/exit 0))
  (let [{options :options args :arguments
         summary :summary errors :errors} (parse-opts args cli-options)
        basic-config (load-config options)
        options (assoc options config-data-key basic-config)]
    (initialize)
    (gs/set-app-config! basic-config)
    (cond
      errors (println errors)
      (:help options) (print-help)
      (:doc options) (generate-swagger-doc
                      (:doc options)
                      args)
      (:interactive options) (gpt-bot (:interactive options))
      :else
      (or (some
           identity
           (map #(apply (partial run-plain-option args) %)
                {:run #(call-after-load-model
                        (first %) (fn [] (run-service (read-model-and-config options))))
                 :compile #(println (build/compile-model (first %)))
                 :build #(println (build/standalone-package (first %)))
                 :exec #(println (build/run-standalone-package (first %)))
                 :repl #(let [opt (first %)
                              with-logs (= opt ":with-logs")
                              args (if with-logs
                                     (rest %)
                                     (do (log/log-capture! :fractl) %))
                              model-name (first args)]
                          (println (force-call-after-load-model
                                    model-name
                                    (fn []
                                      (let [model-info (read-model-and-config options)
                                            [[ev store] _] (prepare-repl-runtime model-info)]
                                        (repl/run model-name store ev))))))
                 :publish #(println (publish-library %))
                 :deploy #(println (d/deploy (:deploy basic-config) (first %)))
                 :db:migrate #(call-after-load-model
                               (first %)
                               (fn []
                                 (db-migrate
                                  (keyword (first %))
                                  (second (read-model-and-config options)))))}))
          (run-service args (read-model-and-config args options))))))
