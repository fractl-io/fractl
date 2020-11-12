(ns fractl.test.util
  (:require [clojure.test :refer [is]]
            [fractl.store :as store]))

(defmacro is-error [exp]
  `(is (try
         (do ~exp false)
         (catch Exception ex#
           (println (str "Expected exception in test: " (.getMessage ex#)))
           ex#))))

(defmacro defcomponent [component & body]
  `(do (fractl.lang/component ~component)
       ~@body
       (store/create-schema (store/open-default-store nil) ~component)
       ~component))

(def fresult (comp :result first))
