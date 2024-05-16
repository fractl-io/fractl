(ns fractl.resolver.redis
  (:require [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.datafmt.json :as json]
            [fractl.lang.internal :as li]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :refer [defmake]])
  (:import [redis.clients.jedis Jedis JedisPool]))

(defn- make-key
  ([instance]
   (make-key (cn/instance-type-kw instance) (cn/idval instance)))
  ([n id] (str n "-" id)))

(defn- redis-create [^JedisPool pool _ instance]
  (let [^Jedis j (.getResource pool)
        ^String k (make-key instance)
        ^String v (json/encode instance)]
    (.set j k v)
    instance))

(def ^:private redis-update redis-create)

(defn- redis-delete [^JedisPool pool _ instance]
  (let [^Jedis j (.getResource pool)
        ^String k (make-key instance)]
    (.getdel j k)
    instance))

(defn- redis-query [^JedisPool pool _ [entity-name {clause :where}]]
  (if (= := (first clause))
    (let [^Jedis j (.getResource pool)]
      (when-let [v (.get j ^String (make-key (li/make-path entity-name) (last clause)))]
        [(json/decode v)]))
    (u/throw-ex (str "query " clause " not supported in redis"))))

(def ^:private resolver-fns
  {:create {:handler redis-create}
   :update {:handler redis-update}
   :delete {:handler redis-delete}
   :query {:handler redis-query}})

(defn- make-jedis-pool [config]
  (let [host (or (:host config) (u/getenv "REDIS_HOST" "localhost"))
        port (or (:port config) (read-string (u/getenv "REDIS_PORT" "6379")))
        username (or (:username config) (u/getenv "REDIS_USERNAME"))
        password (or (:password config) (u/getenv "REDIS_PASSWORD"))]
    (JedisPool. host port username password)))

(defmake :redis
  (fn [resolver-name config]
    (let [conn (make-jedis-pool config)
          handlers (map (fn [[k res]]
                          [k {:handler (partial (:handler res) conn config)}])
                        resolver-fns)]
      (r/make-resolver resolver-name (into {} handlers)))))
