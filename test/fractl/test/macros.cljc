(ns fractl.test.macros
  "Macros for cljs."
  (:require [fractl.store :as store]))

(defmacro defcomponent [component & body]
  `(do (component ~component)
       ~@body
       (store/create-schema (store/open-default-store nil) ~component)
       ~component))
