(ns fractl.lang.datetime
  #?(:clj (:require [taoensso.timbre :as log])
     :cljs (:require [taoensso.timbre :as log]
                     [cljc.java-time.local-date :as ld]
                     [cljc.java-time.local-time :as lt]
                     [cljc.java-time.format.date-time-formatter :as format]))
  #?(:clj (:import [java.time LocalDate LocalTime LocalDateTime]
                   [java.time.format DateTimeFormatter]
                   [java.time.format DateTimeParseException])))

(defn try-parse-date [s formatter]
  #?(:clj (try
            (LocalDate/parse s formatter)
            (catch DateTimeParseException ex
              (log/error ex)
              false))
     :cljs (try
             (ld/parse s formatter)
             (catch :default ex false))))

(defn try-parse-time [s formatter]
  #?(:clj (try
            (LocalTime/parse s formatter)
            (catch DateTimeParseException ex
              (log/error ex)
              false))
     :cljs (try
             (lt/parse s formatter)
             (catch :default ex false))))

#?(:clj (def default-fmt #?(:clj (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss.SSSSSS'Z'"))))

#?(:clj
   (defn parse-date-time
     ([s pat]
      (try-parse-date s (if pat
                          (DateTimeFormatter/ofPattern pat)
                          default-fmt)))
     ([s]
      (parse-date-time s nil)))
   :cljs
   (defn parse-date-time
     ([s pat]
      (try-parse-date s (format/of-pattern pat)))
     ([s]
      (try-parse-date s format/iso-offset-date-time))))

(defn now
  ([pat]
   #?(:clj
      (let [^DateTimeFormatter fmt (if pat
                                     (DateTimeFormatter/ofPattern pat)
                                     default-fmt)]
        (.format fmt (LocalDateTime/now)))
      :cljs (ld/now)))
  ([] (now nil)))
