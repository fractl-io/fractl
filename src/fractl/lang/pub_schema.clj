(ns fractl.lang.pub-schema
  (:require [fractl.util :as u]
            [fractl.global-state :as gs]
            [fractl.datafmt.json :as json])
  (:import [java.util HashMap]
           [redis.clients.jedis Jedis JedisPool]))

(defn publish-schema? [] (:publish-schema (gs/get-app-config)))

(defn- make-jedis-pool [config]
  (let [host (or (:host config) (u/getenv "REDIS_HOST" "localhost"))
        port (or (:port config) (read-string (u/getenv "REDIS_PORT" "6379")))
        username (or (:username config) (u/getenv "REDIS_USERNAME"))
        password (or (:password config) (u/getenv "REDIS_PASSWORD"))]
    (JedisPool. host port username password)))

(def ^:private get-connection (memoize (fn [] (when (publish-schema?) (make-jedis-pool nil)))))

(def ^:private get-app-uuid (memoize (fn [] (u/getenv "FRACTL_APP_UUID" (u/uuid-string)))))

(defn- ^HashMap instance-to-hmap [definition]
  (let [^HashMap hm (HashMap.)]
    (.put hm "operation" (str (:operation definition)))
    (.put hm "tag" (str (:tag definition)))
    (.put hm "type" (str (:type definition)))
    (.put hm "app-uuid" (get-app-uuid))
    (when-let [scm (:schema definition)]
      (.put hm "schema" (json/encode scm)))
    hm))

(defn publish-event [definition]
  (when-let [^JedisPool pool (get-connection)]
    (let [^Jedis j (.getResource pool)
          ^String k (u/getenv "REDIS_PUB_SCHEMA_STREAM" "fractl:schema")
          ^HashMap data (instance-to-hmap definition)]
      (.xadd j k data)
      definition)))
