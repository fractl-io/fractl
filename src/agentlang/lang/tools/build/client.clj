(ns agentlang.lang.tools.build.client
  (:require [agentlang.util :as u]
            [agentlang.global-state :as gs]))

(defn build-project [model-name model-version agentlang-version path build-type]
  (let [config (gs/get-app-config)]
    (if-let [build-config (:client (:build config))]
      (let [mn (name model-name)
            app-root (:root-entity build-config)
            api-host (:api-host build-config)
            port (get-in config [:service :port] 8080)
            auth-url (get build-config :auth-url (str "http://localhost:" port "/auth"))
            auth-service (get-in config [:authentication :service] :none)
            ui-enabled (:ui-enabled config)
            inference-service-enabled (:inference-service-enabled config)
            inference-service-app-id (:inference-service-app-id config)
            inference-service-use-schema (:inference-service-use-schema config)
            inference-service-use-docs (:inference-service-use-docs config)]
        (when-not app-root
          (u/throw-ex "required configuration not found - build -> client -> root-entity"))
        (when-not api-host
          (u/throw-ex "required configuration not found - build -> client -> api-host"))
        (u/exec-in-directory
         path (str "lein new fx-app " mn ":" model-version ":" agentlang-version
                   " -- " api-host " " app-root " " build-type " " auth-url " "
                   auth-service " " ui-enabled " " inference-service-enabled " " inference-service-app-id " "
                   inference-service-use-schema " " inference-service-use-docs)))
      (println "no build -> client spec in config, skipping client-app generation"))
    model-name))
