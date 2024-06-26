(ns fractl.lang.pub-schema
  (:require [clojure.walk :as w]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.global-state :as gs]
            [fractl.datafmt.json :as json]
            [fractl.inference.embeddings.core :as e]
            [fractl.inference.embeddings.pgvector :as pgvec])
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

(defn- preproc-definition [d]
  (w/prewalk #(if (fn? %) :fn %) d))

(def ^:private redis-pub-schema false)

(defn- publish-event-on-redis [definition]
  (try
    (when-let [^JedisPool pool (get-connection)]
      (let [^Jedis j (.getResource pool)]
        (try
          (let [^String channel (u/getenv "REDIS_PUB_SCHEMA_CHANNEL" "fractl:schema")
                ^String data (json/encode (assoc (preproc-definition definition) :app-uuid (u/get-app-uuid)))]
            (.publish j channel data)
            definition)
          (finally
            (.close j)))))
    (catch Exception ex
      (log/error ex))))

(defn publish-event [definition]
  (if redis-pub-schema
    (publish-event-on-redis definition)
    (e/embed-schema (pgvec/fetch-db) definition)))
