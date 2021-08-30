(ns fractl.lang.datetime
  (:require [clojure.string :as str]
            [fractl.util :as u]
            [fractl.util.logger :as log]
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

(def ^:private date-time-formatters
  (concat
   [format/iso-local-date-time]  ; 2011-12-03T10:15:30
   (mapv
    format/of-pattern
    ["yyyy-MM-dd HH:mm:ss"       ; 2021-01-08 04:05:06
     "yyyy-MM-dd HH:mm:ss.SSS"   ; 2021-01-08 04:05:06.789
     "yyyyMMddHHmmss"            ; 20210108040506
     "yyyy-MM-dd HH:mm:ss z"]))) ; 2021-01-08 04:05:06 PST or America/New_York

(def ^:private date-formatters
  (map
   format/of-pattern
   ["MMMM d, yyyy" ; January 8, 2021
    "yyyy-MMM-dd"  ; 2021-Jan-08
    "yyyy-MM-dd"   ; 2021-01-30
    "MMM-dd-yyyy"  ; Jan-08-2021
    "dd-MMM-yyyy"  ; 08-Jan-2021
    "yyyyMMdd"]))  ; 20210108

(def ^:private time-formatters
  (map
   format/of-pattern
   ["HH:mm:ss.SSS"  ; 04:05:06.789
    "HH:mm:ss"      ; 04:05:06
    "HH:mm"         ; 04:05
    "HHmmss"        ; 040506
    ;;; 12-hour format time is manually parsed because
    ;;; Java11 and Java14 behave differently for the
    ;;; following pattern.
    ;; "hh:mm a"    ; 04:05 pm, hour <= 12
    "HH:mm:ss z"])) ; 04:05:06 PST or 04:05:06 America/New_York

(defn- find-format [try-parse-fn formatters s]
  (some #(try-parse-fn % s) formatters))

(def parse-date-time (partial find-format try-parse-date-time date-time-formatters))
(def parse-date (partial find-format try-parse-date date-formatters))

(defn- am-pm? [s]
  (let [s (str/lower-case s)]
    (or (= s "am") (= s "pm"))))

(defn- parse-12hr-time [s]
  (let [n (count s)]
    (when (and (= n 8)
               (= \: (nth s 2))
               (am-pm? (subs s 6)))
      (let [h (u/parse-string (subs s 0 2))
            m (u/parse-string (subs s 3 5))]
        (and (number? h) (number? m)
             (<= 0 h 12) (<= 0 m 59))))))

(defn parse-time [s]
  (or (find-format try-parse-time time-formatters s)
      (parse-12hr-time s)))

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
