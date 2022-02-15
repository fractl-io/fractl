(ns fractl.global-state)

(def config (atom nil))

(defn set-config! [cfg]
  (reset! config cfg))

(defn fetch-config []
  (partial get-in @config))
