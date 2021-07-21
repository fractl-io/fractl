(ns fractl.read-file
  "Utils to read files."
  (:require [fractl.util :as u]
            [fractl.util.logger :as log]
            [clojure.string :as string]
            [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.data.xml :as xml]
            [cheshire.core :as json]
            [clojure.edn :as edn]))

(defn get-extension-from-file [fname]
  (second (string/split fname #"\.")))

(defn csv->maps
  "Create maps from csv taking keyword from first row
  and keys from following rows"
  [csv-file]
  (map zipmap
       (->> (first csv-file)
            (map keyword)
            repeat)
       (rest csv-file)))

(defn read-csv
  "Eagerly/lazily read csv."
  ([csv-file eager? column]
   (try
     (with-open [reader (io/reader csv-file)]
       (if (true? eager?)
         (doall
           (csv/read-csv reader))
         (let [data (csv/read-csv reader)]
           (map #(nth % column) data))))
     (catch Exception e
       (log/error (str "Couldn't open '%s': %s\n" csv-file (.getMessage e))))
     (catch RuntimeException e
       (log/error (str "Error parsing csv file '%s': %s\n" csv-file (.getMessage e))))))
  ([csv-file eager?]
   (read-csv csv-file eager? nil)))

(defn json->maps
  "Eagerly read json."
  [json-file]
  (try
    (with-open [r (io/reader json-file)]
      (json/parse-string (slurp r) true))
    (catch Exception e
      (log/error (str "Couldn't open '%s': %s\n" json-file (.getMessage e))))
    (catch RuntimeException e
      (log/error (str "Error parsing json file '%s': %s\n" json-file (.getMessage e))))))

(defn xml->maps
  "A basic implementation, walking in tags will
  require further manipulation."
  [xml-file]
  (try
    (with-open [r (io/reader xml-file)]
      (xml/parse (java.io.StringReader. (slurp r))))
    (catch Exception e
      (log/error (str "Couldn't open '%s': %s\n" xml-file (.getMessage e))))
    (catch RuntimeException e
      (log/error (str "Error parsing xml file '%s': %s\n" xml-file (.getMessage e))))))

(defn read-edn
  "Load edn from an io/reader source (filename or io/resource)."
  [edn-file]
  (try
    (with-open [r (io/reader edn-file)]
      (edn/read (java.io.PushbackReader. r)))
    (catch Exception e
      (log/error (str "Couldn't open '%s': %s\n" edn-file (.getMessage e))))
    (catch RuntimeException e
      (log/error (str "Error parsing edn file '%s': %s\n" edn-file (.getMessage e))))))

(defn read-file [file]
  (let [ext (get-extension-from-file file)]
    (cond
      (= "csv" ext) (csv->maps (read-csv file true))        ;; Currently eagerly read CSVs.
      (= "json" ext) (json->maps file)
      (= "xml" ext) (xml->maps file)
      (= "edn" ext) (read-edn file)
      :else (log/error "Invalid file format!"))))
