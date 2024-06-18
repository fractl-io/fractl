(ns fractl.util.logger
  #?(:clj (:require [clojure.tools.logging :as logger]))
  #?(:clj (:import [org.slf4j LoggerFactory]
                   [ch.qos.logback.classic Level]
                   [ch.qos.logback.classic Logger]
                   [ch.qos.logback.classic LoggerContext]
                   [ch.qos.logback.classic.spi ILoggingEvent]
                   [ch.qos.logback.classic.net SyslogAppender])))

;; flag applies only to low-priority log-modes (debug and info)
(def logging-enabled (atom #?(:clj true :cljs false)))
(def dev-logging-enabled (atom false))

(defn disable-logging! [] (reset! logging-enabled false))
(defn enable-logging! [] (reset! logging-enabled true))

(defn enable-dev-logging! [] (reset! dev-logging-enabled true))

(defn logging-enabled? [] @logging-enabled)
(defn dev-logging-enabled? [] @dev-logging-enabled)

#?(:clj (def log-capture! logger/log-capture!))

#?(:cljs
   (do
     (defn prn-log [tag msg]
       (let [d (js/Date.)]
         (println (str (.toISOString d) " " (name tag) " " msg))))

     (def prn-error (partial prn-log :ERROR))
     (def prn-debug (partial prn-log :DEBUG))
     (def prn-info (partial prn-log :INFO))
     (def prn-warn (partial prn-log :WARN))))

#?(:clj
   (defmacro error [msg] `(logger/error ~msg))
   :cljs
   (defn error [msg] (prn-error msg)))

#?(:clj
   (defmacro debug [msg]
     `(when (logging-enabled?)
        (logger/debug ~msg)))
   :cljs
   (defn debug [msg]
     (when @logging-enabled
       (prn-debug msg))))

#?(:clj
   (defmacro dev-debug [msg]
     `(when (dev-logging-enabled?)
        (debug ~msg)))
   :cljs
   (defn dev-debug [msg]
     (when @dev-logging-enabled
       (debug msg))))

#?(:clj
   (defmacro info [msg]
     `(when (logging-enabled?)
        (logger/info ~msg)))
   :cljs
   (defn info [msg]
     (when @logging-enabled
       (prn-info msg))))

#?(:clj
   (defmacro warn [msg] `(logger/warn ~msg))
   :cljs
   (defn warn [msg] (prn-warn msg)))

#?(:clj
   (defmacro exception [ex]
     `(do (error (.getMessage ~ex))
          (let [^java.io.StringWriter sw# (java.io.StringWriter.)
                ^java.io.PrintWriter pw# (java.io.PrintWriter. sw#)]
            (.printStackTrace ~ex pw#)
            (.close pw#)
            (debug (.toString sw#)))))
   :cljs
   (defn exception [ex]
     (prn-error ex)))

#?(:clj
   (do
     (defn- as-level [k]
       (case k
         :debug Level/DEBUG
         :warn Level/WARN
         :info Level/INFO
         :error Level/ERROR
         :trace Level/TRACE
         Level/ALL))

     (defn create-syslogger
       ([logger-name config]
        (let [^LoggerContext lc (LoggerFactory/getILoggerFactory)
              ^SyslogAppender appender (SyslogAppender.)]
          (.setSyslogHost appender (or (:syslog-host config) "localhost"))
          (.setPort appender (or (:port config) 514))
          (.setFacility appender (or (:facility config) "SYSLOG"))
          (.setContext appender lc)
          (.start appender)
          (let [^Logger logger (LoggerFactory/getLogger logger-name)]
            (.addAppender logger appender)
            (.setLevel logger (as-level (or (:level config) :debug)))
            (.setAdditive logger (or (:is-additive config) false))
            logger)))
       ([config] (create-syslogger "ROOT" config))
       ([] (create-syslogger nil)))))
