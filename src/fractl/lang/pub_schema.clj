(ns fractl.lang.pub-schema
  (:require [clojure.walk :as w]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.global-state :as gs]
            [fractl.datafmt.json :as json])
  (:import [redis.clients.jedis Jedis JedisPool]))

(defn publish-schema? [] (:publish-schema (gs/get-app-config)))

(defn- make-jedis-pool [config]
  (let [host (or (:host config) (u/getenv "REDIS_HOST" "localhost"))
        port (or (:port config) (read-string (u/getenv "REDIS_PORT" "6379")))
        username (or (:username config) (u/getenv "REDIS_USERNAME" ""))
        password (or (:password config) (u/getenv "REDIS_PASSWORD" ""))]
    (if (and (seq username) (seq password))
      (JedisPool. host port username password)
      (JedisPool. host port))))

(def ^:private get-connection (memoize (fn [] (when (publish-schema?) (make-jedis-pool nil)))))

(def ^:private get-app-uuid (memoize (fn [] (u/getenv "FRACTL_APP_UUID" (u/uuid-string)))))

(defn- preproc-definition [d]
  (w/prewalk #(if (fn? %) :fn %) d))

(defn publish-event [definition]
  (try
    (when-let [^JedisPool pool (get-connection)]
      (let [^Jedis j (.getResource pool)]
        (try
          (let [^String channel (u/getenv "REDIS_PUB_SCHEMA_CHANNEL" "fractl:schema")
                ^String data (json/encode (assoc (preproc-definition definition) :app-uuid (get-app-uuid)))]
            (.publish j channel data)
            definition)
          (finally
            (.close j)))))
    (catch Exception ex
      (log/error ex))))
