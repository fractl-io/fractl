(ns fractl.util.str
  "String utilities."
  (:import [java.time LocalDate LocalTime]
           [java.time.format DateTimeFormatter]
           [java.time.format DateTimeParseException]))

(defn try-parse-date [s formatter]
  (try
    (LocalDate/parse s formatter)
    (catch DateTimeParseException ex
      false)))

(defn try-parse-time [s formatter]
  (try
    (LocalTime/parse s formatter)
    (catch DateTimeParseException ex
      false)))

(defn parse-date-time
  ([s pat]
   (try-parse-date s (DateTimeFormatter/ofPattern pat)))
  ([s]
   (try-parse-date s DateTimeFormatter/ISO_OFFSET_DATE_TIME)))
