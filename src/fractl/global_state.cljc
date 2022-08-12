(ns fractl.global-state)

(def ^:private app-config (atom nil))

(defn merge-app-config! [config]
  (reset! app-config (merge @app-config config)))

(defn get-app-config []
  @app-config)

(def ^:dynamic active-event-context)
