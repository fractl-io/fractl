(ns fractl.resolver.registry
  (:require [fractl.util :as u]))

(def ^:private resolver-db (u/make-cell {}))

(defn resolver-for-path [path]
  (get @resolver-db path))

(def ^:private valid-resolver-keys #{:upsert :delete :get :query :eval})

(defn make-resolver [name fnmap]
  (when-not (= (set (keys fnmap)) valid-resolver-keys)
    (u/throw-ex (str "invalid resolver keys - " (keys fnmap))))
  (doseq [[k v] fnmap]
    (when-not (fn? v)
      (u/throw-ex (str "resolver key " k " must be mapped to a function"))))
  (assoc fnmap :name name))

(def resolver-name :name)
(def resolver-upsert :upsert)
(def resolver-delete :delete)
(def resolver-get :get)
(def resolver-query :query)
(def resolver-eval :eval)

(defn override-resolver [path resolver]
  (u/safe-set resolver-db (assoc @resolver-db path resolver)))

(defn compose-resolver [path resolver]
  (let [resolvers (get @resolver-db path [])]
    (u/safe-set resolver-db
                (assoc @resolver-db path
                       (conj resolvers resolver)))))

(def composed? (complement map?))
(def override? map?)
