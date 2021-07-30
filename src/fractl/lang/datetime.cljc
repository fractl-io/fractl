(ns fractl.lang.datetime
  (:require [fractl.util.logger :as log]
            [cljc.java-time.local-date :as ld]
            [cljc.java-time.local-time :as lt]
            [cljc.java-time.format.date-time-formatter :as format]
            [cljc.java-time.temporal.chrono-unit :as cu])
  #?(:clj (:import [java.time LocalDateTime]
                   [java.time.format DateTimeFormatter]
                   [java.time.format DateTimeParseException])))

(defn try-parse-date [s formatter]
  #?(:clj (try
            (LocalDateTime/parse s formatter)
            (catch DateTimeParseException ex
              (log/error ex)
              false))
     :cljs (try
             (ld/parse s formatter)
             (catch :default ex
               (log/error ex)
               false))))

#?(:clj
   (defn parse-date-time
     ([s pat]
      (try-parse-date s (if pat
                          (DateTimeFormatter/ofPattern pat)
                          (DateTimeFormatter/ISO_LOCAL_DATE_TIME))))
     ([s]
      (parse-date-time s nil)))
   :cljs
   (defn parse-date-time
     ([s pat]
      (try-parse-date s (format/of-pattern pat)))
     ([s]
      (try-parse-date s format/iso-local-date-time))))

(defn as-string
  ([dt pat]
   #?(:clj (let [^DateTimeFormatter fmt
                 (if pat
                   (DateTimeFormatter/ofPattern pat)
                   (DateTimeFormatter/ISO_LOCAL_DATE_TIME))]
             (.format fmt dt))
      :cljs dt))
  ([dt] (as-string dt nil)))

(defn now
  ([pat]
   #?(:clj (as-string (LocalDateTime/now) pat)
      :cljs (ld/now)))
  ([] (now nil)))

(defn add-days-to-datetime
  ([days date]
   (if (string? date)
     (ld/plus-days (ld/parse date) days)
     (log/error "Date isn't a string!")))
  ([days]
   (add-days-to-datetime days (str (ld/now)))))

(defn now-raw []
  #?(:clj (LocalDateTime/now)
     :cljs (ld/now)))

(defn difference-in-seconds [dt1 dt2]
  (cu/between cu/seconds dt1 dt2))
