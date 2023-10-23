(ns fractl.global-state
  (:require [environ.core :as environ]
            #?(:clj [clojure.java.io :as io])))

(def ^:private app-config (atom nil))

(defn set-app-config! [config]
  (reset! app-config config))

(defn merge-app-config! [config]
  (reset! app-config (merge @app-config config)))

(defn get-app-config []
  @app-config)

(def ^:dynamic active-event-context nil)

(defn active-user [] (:User active-event-context))

(def ^:private active-txn (ThreadLocal.))

(defn set-active-txn! [txn]
  (.set active-txn txn))

(defn get-active-txn []
  (.get active-txn))

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


#?(:clj
   (def fractl-version
     (memoize (fn []
                (or (:fractl-version environ/env)
                    (let [projfile (io/resource "META-INF/leiningen/fractl-io/fractl/project.clj")
                          project (read-string (slurp projfile))]
                      (nth project 2))))))

   :cljs
   (def fractl-version
     (memoize (fn [] (:fractl-version environ/env)))))
