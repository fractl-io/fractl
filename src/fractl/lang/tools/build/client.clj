(ns fractl.lang.tools.build.client
  (:require [fractl.util :as u]
            [fractl.global-state :as gs]))

(defn build-project [model-name model-version path]
  (let [mn (name model-name)
        build-config (:client (:build (gs/get-app-config)))
        app-root (:root-entity build-config)
        api-host (:api-host build-config)]
    (when-not app-root
      (u/throw-ex "required configuration not found - build -> client -> root-entity"))
    (when-not app-root
      (u/throw-ex "required configuration not found - build -> client -> api-host"))
    (u/exec-in-directory path (str "lein new fx-app " mn ":" model-version " -- " api-host " " app-root))
    model-name))
