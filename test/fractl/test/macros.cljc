(ns fractl.test.macros
  "Macros for cljs."
  (:require [fractl.store :as store]))

(defmacro defcomponent [component & body]
  `(do (component ~component)
       ~@body
       (store/create-schema (store/get-default-store) ~component)
       ~component))

;; #?(:clj
;;    (defmacro is-error [exp]
;;      `(is (try
;;             (do ~exp false)
;;             (catch Exception ex#
;;               (println (str "Expected exception in test: " (.getMessage ex#)))
;;               ex#)))))

(defmacro is-error [exp]
  `(is (try
         (do ~exp false)
         (catch js/Error ex#
           (println (str "Expected exception in test: " ex#))
           ex#))))
