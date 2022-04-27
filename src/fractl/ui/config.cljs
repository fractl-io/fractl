(ns fractl.ui.config)

(def ^:private config (atom {}))

(defn set-config! [cfg]
  (reset! config cfg))

(defn get-config []
  @config)

(defn dashboard
  ([config]
   (get-in config [:view :dashboard]))
  ([] (dashboard @config)))

(defn component
  ([config]
   (get-in config [:view :component]))
  ([] (component @config)))
