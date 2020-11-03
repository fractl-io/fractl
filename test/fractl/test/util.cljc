(ns fractl.test.util
  (:require [clojure.test :refer [is]]))

(defmacro is-error [exp]
  `(is (try
         (do ~exp false)
         (catch Exception ex#
           (println (str "Expected exception in test: " (.getMessage ex#)))
           ex#))))
