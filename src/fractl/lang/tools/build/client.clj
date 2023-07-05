(ns fractl.lang.tools.build.client
  (:require [fractl.util :as u]
            [fractl.util.io :as iou]
            [fractl.global-state :as gs]))

(defn build-project [model-name model-version path build-type]
  (if-let [build-config (:client (:build (gs/get-app-config)))]
    (let [mn (name model-name)
          app-root (:root-entity build-config)
          api-host (:api-host build-config)]
      (when-not app-root
        (u/throw-ex "required configuration not found - build -> client -> root-entity"))
      (when-not api-host
        (u/throw-ex "required configuration not found - build -> client -> api-host"))
      (iou/exec-in-directory
       path (str "lein new fx-app " mn ":" model-version
                 " -- " api-host " " app-root " " build-type)))
    (println "no build -> client spec in config, skipping client-app generation"))
  model-name)
