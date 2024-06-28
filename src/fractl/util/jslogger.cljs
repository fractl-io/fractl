(ns fractl.util.jslogger)

(defn prn-log [tag msg]
  (let [d (js/Date.)]
    (println (str (.toISOString d) " " (name tag) " " msg))))

(def prn-error (partial prn-log :ERROR))
(def prn-debug (partial prn-log :DEBUG))
(def prn-info (partial prn-log :INFO))
(def prn-warn (partial prn-log :WARN))

(defn error [msg] (prn-error msg))

(defn debug [msg]
  (when @logging-enabled
    (prn-debug msg)))

(defn dev-debug [msg]
  (when @dev-logging-enabled
    (debug msg)))

(defn info [msg]
  (when @logging-enabled
    (prn-info msg)))

(defn warn [msg] (prn-warn msg))

(defn exception [ex] (prn-error ex))
