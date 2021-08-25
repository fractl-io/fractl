(ns fractl.lang.datetime
  (:require [fractl.util.logger :as log]
            [cljc.java-time.local-date :as ld]
            [cljc.java-time.local-time :as lt]
            [cljc.java-time.local-date-time :as ldt]
            [cljc.java-time.format.date-time-formatter :as format]
            [cljc.java-time.temporal.chrono-unit :as cu]))

(defn try-parse-date [s formatter]
  (try
    (ldt/parse s formatter)
    (catch #?(:clj Exception :cljs :default) ex
      (log/error ex)
      false)))

(defn parse-date-time
  ([s pat]
   (try-parse-date s (format/of-pattern pat)))
  ([s]
   (try-parse-date s format/iso-local-date-time)))

;; TODO: implement parse-date and parse-time
(defn parse-date [s]
  true)

(defn parse-time [s]
  true)

(defn as-string
  ([dt pat]
   (format/format
    (if pat
      (format/of-pattern pat)
      format/iso-local-date-time)
    dt))
  ([dt] (as-string dt nil)))

(defn now []
  (as-string (ldt/now)))

(def now-raw ldt/now)

(defn difference-in-seconds [dt1 dt2]
  (cu/between cu/seconds dt1 dt2))
