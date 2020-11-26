(ns fractl.resolver.registry
  (:require [fractl.util :as u]))

(def ^:private resolver-db (u/make-cell {}))

(defn resolver-for-path [path]
  (get @resolver-db path))

(defn override-resolver [path resolver]
  (u/safe-set resolver-db (assoc @resolver-db path resolver)))

(defn compose-resolver [path resolver]
  (let [resolvers (get @resolver-db path [])]
    (u/safe-set resolver-db
                (assoc @resolver-db path
                       (conj resolvers resolver)))))

(def composed? (complement map?))
(def override? map?)
