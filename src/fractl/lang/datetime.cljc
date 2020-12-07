(ns fractl.lang.datetime
  (:require [taoensso.timbre :as log])
  #?(:clj (:import [java.time LocalDate LocalTime LocalDateTime]
                   [java.time.format DateTimeFormatter]
                   [java.time.format DateTimeParseException])))

(defn try-parse-date [s formatter]
  #?(:clj (try
            (LocalDate/parse s formatter)
            (catch DateTimeParseException ex
              (log/error ex)
              false))))

(defn try-parse-time [s formatter]
  #?(:clj (try
            (LocalTime/parse s formatter)
            (catch DateTimeParseException ex
              (log/error ex)
              false))))

(def default-fmt #?(:clj (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSSSSS'Z'")))

(defn parse-date-time
  ([s pat]
   #?(:clj (try-parse-date s (if pat
                               (DateTimeFormatter/ofPattern pat)
                               default-fmt))))
  ([s]
   (parse-date-time s nil)))

(defn now
  ([pat]
   #?(:clj
      (let [^DateTimeFormatter fmt (if pat
                                     (DateTimeFormatter/ofPattern pat)
                                     default-fmt)]
        (.format fmt (LocalDateTime/now)))))
  ([] (now nil)))
