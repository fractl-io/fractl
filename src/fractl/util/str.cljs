(ns fractl.util.str
  "String utilities."
  (:require [cljc.java-time.local-date :as ld]
            [cljc.java-time.local-time :as lt]
            [cljc.java-time.format.date-time-formatter :as format]))

(defn try-parse-date [s formatter]
  (try
    (ld/parse s formatter)
    (catch :default ex false)))

(defn try-parse-time [s formatter]
  (try
    (lt/parse s formatter)
    (catch :default ex false)))

(defn parse-date-time
  ([s pat]
   (try-parse-date s (format/of-pattern pat)))
  ([s]
   (try-parse-date s format/iso-offset-date-time)))