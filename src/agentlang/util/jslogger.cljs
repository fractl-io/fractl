(ns agentlang.util.jslogger)

(defn prn-log [tag msg]
  (let [d (js/Date.)]
    (println (str (.toISOString d) " " (name tag) " " msg))))

(def error (partial prn-log :ERROR))
(def debug (partial prn-log :DEBUG))
(def info (partial prn-log :INFO))
(def warn (partial prn-log :WARN))
(def exception error)
