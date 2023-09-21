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

(def ^:private script-mode (atom false))

(defn in-script-mode! []
  (reset! script-mode true))

(defn in-script-mode? [] @script-mode)

#?(:clj
   (def ^ThreadLocal error-code (ThreadLocal.))
   :cljs
   (def error-code (atom nil)))

(defn set-error-code! [code]
  #?(:clj (.set error-code code)
     :cljs (reset! error-code code)))

(defn get-error-code []
  #?(:clj (.get error-code)
     :cljs @error-code))

(defn set-error-no-perm! []
  (set-error-code! :no-permission))

(defn error-no-perm? []
  (= (get-error-code) :no-permission))
