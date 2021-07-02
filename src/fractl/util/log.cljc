(ns fractl.util.log
  (:require [taoensso.timbre :as timbre])
  #?(:clj
     (:require [taoensso.timbre.appenders.3rd-party.rolling :as rolling]
               [taoensso.timbre.appenders.3rd-party.rotor :as rotor]))
  #?(:clj (:import [java.io StringWriter PrintWriter])))

(defn- println-appender [config]
  config)

(def ^:private appender-fns (merge {:println println-appender}
                             #?(:clj
                                {:rolling rolling/rolling-appender
                                 :rotor rotor/rotor-appender}
                                :cljs {})
                             ;; Add more appenders here.
                             ))

(defn- appender->fn [[appender config]]
  (when-let [f (appender appender-fns)]
    [appender (f config)]))

(defn- appenders->fns [appenders]
  (let [result (map appender->fn appenders)]
    (into {} result)))

(defn- disable-ascii-colors [config]
  (assoc config :output-fn
         (partial timbre/default-output-fn {:stacktrace-fonts {}})))

(defn init!
  "Initialize the logging sub-system by setting the global configuration."
  [log-config]
  (let [ascii-colors? (:ascii-colors? log-config)
        log-config (dissoc log-config :ascii-colors?)
        final-config (if-let [appenders (:appenders log-config)]
                       (assoc log-config :appenders
                              (appenders->fns appenders))
                       log-config)]
    (timbre/merge-config! (if ascii-colors?
                            final-config
                            (disable-ascii-colors final-config)))))

(defn error [msg]
  (timbre/error msg))

(defn debug [msg]
  (timbre/debug msg))

(defn info [msg]
  (timbre/info msg))

(defn warn [msg]
  (timbre/warn msg))

(defn exception [ex]
  #?(:clj
     (do (error (.getMessage ex))
         (let [^StringWriter sw (StringWriter.)
               ^PrintWriter pw (PrintWriter. sw)]
           (.printStackTrace ex pw)
           (.close pw)
           (debug (.toString sw))))
     :cljs (error ex)))
