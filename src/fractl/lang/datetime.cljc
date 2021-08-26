(ns fractl.lang.datetime
  (:require [fractl.util.logger :as log]
            [cljc.java-time.local-date :as ld]
            [cljc.java-time.local-time :as lt]
            [cljc.java-time.local-date-time :as ldt]
            [cljc.java-time.format.date-time-formatter :as format]
            [cljc.java-time.temporal.chrono-unit :as cu]))

(defn- valid-format? [parser formatter s]
  (try
    (parser s formatter)
    (catch #?(:clj Exception :cljs :default) ex
      (log/error ex)
      false)))

(def try-parse-date-time (partial valid-format? ldt/parse))
(def try-parse-date (partial valid-format? ld/parse))
(def try-parse-time (partial valid-format? lt/parse))

(defn parse-date-time
  ([s pat]
   (try-parse-date-time (format/of-pattern pat) s))
  ([s]
   (try-parse-date-time format/iso-local-date-time s)))

(def ^:private date-formatters
  (map
   format/of-pattern
   ["MMMM d, yyyy" ; January 8, 2021
    "yyyy-MMM-dd"  ; 2021-Jan-08
    "MMM-dd-yyyy"  ; Jan-08-2021
    "dd-MMM-yyyy"  ; 08-Jan-2021
    "yyyyMMdd"]))  ; 20210108

(def ^:private time-formatters
  (map
   format/of-pattern
   ["HH:mm:ss.SSS"  ; 04:05:06.789
    "HH:mm"         ; 04:05
    "HHmmss"        ; 040506
    "hh:mm a"       ; 04:05 pm, hour <= 12
    "HH:mm:ss z"])) ; 04:05:06 PST or 04:05:06 America/New_York

(defn parse-date [s]
  (some #(try-parse-date % s) date-formatters))

(defn parse-time [s]
  (some #(try-parse-time % s) time-formatters))

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
