(ns fractl.datafmt.csv
  (:require [clojure.data.csv :as csv]
            [clojure.java.io :as io]))

(defn read-csv [file-path]
  (with-open [reader (io/reader file-path)]
    (doall
     (csv/read-csv reader))))
