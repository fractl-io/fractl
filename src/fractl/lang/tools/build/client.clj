(ns fractl.lang.tools.build.client
  (:require [fractl.util :as u]
            [fractl.global-state :as gs]))

(defn build-project [model-name model-version fractl-version path build-type]
  (let [config (gs/get-app-config)]
    (if-let [build-config (:client (:build config))]
      (let [mn (name model-name)
            app-root (:root-entity build-config)
            api-host (:api-host build-config)
            port (get config [:service :port] 8080)
            auth-url (get build-config :auth-url (str "http://localhost:" port "/auth"))
            auth-service (get config [:authentication :service] :none)
            ui-enabled (:ui-enabled config)
            copilot-enabled (:copilot-enabled config)
            copilot-app-id (:copilot-app-id config)
            copilot-use-schema (:copilot-use-schema config)
            copilot-use-docs (:copilot-use-docs config)]
        (when-not app-root
          (u/throw-ex "required configuration not found - build -> client -> root-entity"))
        (when-not api-host
          (u/throw-ex "required configuration not found - build -> client -> api-host"))
        (u/exec-in-directory
         path (str "lein new fx-app " mn ":" model-version ":" fractl-version
                   " -- " api-host " " app-root " " build-type " " auth-url " "
                   auth-service " " ui-enabled " " copilot-enabled " " copilot-app-id " " 
                   copilot-use-schema " " copilot-use-docs)))
      (println "no build -> client spec in config, skipping client-app generation"))
    model-name))
