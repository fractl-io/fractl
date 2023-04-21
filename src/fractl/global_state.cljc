(ns fractl.global-state)

(def ^:private app-config (atom nil))

(defn set-app-config! [config]
  (reset! app-config config))

(defn merge-app-config! [config]
  (reset! app-config (merge @app-config config)))

(defn get-app-config []
  @app-config)

(def ^:dynamic active-event-context nil)

(defn active-user [] (:User active-event-context))

(def ^:dynamic active-store-connection nil)
