(ns fractl.resolver.core
  (:require [fractl.util :as u]
            [fractl.lang.internal :as li]))

(def ^:private valid-resolver-keys #{:upsert :delete :get :query :eval})

(defn make-resolver [resolver-name fnmap]
  (when-not (every? identity (map #(some #{%} valid-resolver-keys) (keys fnmap)))
    (u/throw-ex (str "invalid resolver keys - " (keys fnmap))))
  (doseq [[k v] fnmap]
    (when-not (fn? v)
      (u/throw-ex (str "resolver key " k " must be mapped to a function"))))
  (assoc fnmap :name resolver-name))

(def resolver-name :name)
(def resolver-upsert :upsert)
(def resolver-delete :delete)
(def resolver-get :get)
(def resolver-query :query)
(def resolver-eval :eval)

(defn- wrap-result [method resolver arg]
  {:resolver (:name resolver)
   :method method
   :result ((method resolver) arg)})

(def call-resolver-upsert (partial wrap-result :upsert))
(def call-resolver-delete (partial wrap-result :delete))
(def call-resolver-get (partial wrap-result :get))
(def call-resolver-query (partial wrap-result :query))
(def call-resolver-eval (partial wrap-result :eval))
