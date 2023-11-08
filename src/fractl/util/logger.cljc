(ns fractl.util.logger
  #?(:clj (:require [clojure.tools.logging :as logger])))

;; flag applies only to low-priority log-modes (debug and info)
(def logging-enabled (atom #?(:clj true :cljs false)))

(defn disable-logging! [] (reset! logging-enabled false))
(defn enable-logging! [] (reset! logging-enabled true))

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

(defn error [msg]
  (#?(:clj logger/error
      :cljs prn-error)
   msg))

(defn debug [msg]
  (when @logging-enabled
    (#?(:clj logger/debug
        :cljs prn-debug)
     msg)))

(defn info [msg]
  (when @logging-enabled
    (#?(:clj logger/info
        :cljs prn-info)
     msg)))

(defn warn [msg]
  (#?(:clj logger/warn
      :cljs prn-warn)
   msg))

(defn exception [ex]
  #?(:clj
     (do (error (.getMessage ex))
         (let [^java.io.StringWriter sw (java.io.StringWriter.)
               ^java.io.PrintWriter pw (java.io.PrintWriter. sw)]
           (.printStackTrace ex pw)
           (.close pw)
           (debug (.toString sw))))
     :cljs (prn-error ex)))
