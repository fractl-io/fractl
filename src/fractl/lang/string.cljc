(ns fractl.lang.string)

(defn string-in-range? [min max obj]
  (and (string? obj)
       (<= min (count obj) max)))
