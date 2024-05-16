(ns fractl.lang.pub-schema
  (:require [fractl.util :as u]
            [fractl.global-state :as gs]
            [fractl.lang :as ln]
            [fractl.datafmt.json :as json]
            [fractl.resolver.redis :as r])
  (:import [java.util HashMap]
           [redis.clients.jedis Jedis JedisPool]))

(defn- publish-schema? [] (:publish-schema (gs/get-app-config)))

(def ^:private get-connection (memoize (fn [] (when publish-schema? (r/make-jedis-pool)))))

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
  (when-let [^JedisPool conn (get-connection)]
    (let [^Jedis j (.getResource pool)
          ^String k (u/getenv "REDIS_PUB_SCHEMA_STREAM" "fractl:schema")
          ^HashMap data (instance-to-hmap definition)]
      (.xadd j k data)
      definition)))
