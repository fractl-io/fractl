(ns fractl.subs.kafka
  (:require [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.datafmt.json :as json]
            [fractl.subs.internal :as si])
  #?(:clj
     (:import [java.time Duration]
              [java.util Arrays Properties]
              [org.apache.kafka.clients.consumer
               ConsumerConfig ConsumerRecord KafkaConsumer
               ConsumerRebalanceListener]
              [org.apache.kafka.clients CommonClientConfigs]
              [org.apache.kafka.common.config SaslConfigs]
              [org.apache.kafka.common.serialization
               StringDeserializer StringSerializer]
              [org.apache.kafka.common.errors WakeupException]
              [fractl.subs HandleRebalance])))

(defn make-consumer [config]
  #?(:clj
     (let [^Properties props (Properties.)]
       (.setProperty props ConsumerConfig/BOOTSTRAP_SERVERS_CONFIG
                     (or (:servers config) "localhost:9092"))
       (.setProperty props ConsumerConfig/GROUP_ID_CONFIG
                     (or (:group-id config) "fractl-consumers"))
       (.setProperty props ConsumerConfig/AUTO_OFFSET_RESET_CONFIG (or (:auto-offset-rest config) "earliest"))
       (.setProperty props ConsumerConfig/ENABLE_AUTO_COMMIT_CONFIG "false")
       (.setProperty props ConsumerConfig/KEY_DESERIALIZER_CLASS_CONFIG "org.apache.kafka.common.serialization.StringDeserializer")
       (.setProperty props ConsumerConfig/VALUE_DESERIALIZER_CLASS_CONFIG "org.apache.kafka.common.serialization.StringDeserializer")
       (let [username (u/getenv "KAFKA_USERNAME" "")
             password (u/getenv "KAFKA_PASSWORD" "")]
         (when (and (seq username) (seq password))
           (.setProperty props CommonClientConfigs/SECURITY_PROTOCOL_CONFIG "SASL_SSL")
           (.setProperty props SaslConfigs/SASL_MECHANISM "PLAIN")
           (.setProperty props SaslConfigs/SASL_JAAS_CONFIG
                         (str "org.apache.kafka.common.security.plain.PlainLoginModule required username=\""  username
                              "\" password=\""  password  "\";"))))
       (let [^KafkaConsumer consumer (KafkaConsumer. props)
             ^ConsumerRebalanceListener rebalance-listener (HandleRebalance. consumer)]
         {:consumer consumer :rebalance-listener rebalance-listener :config config}))))

(defn- close-consumer [^KafkaConsumer consumer ^HandleRebalance rebalance-listener]
  #?(:clj
     (do
       (try
         (do (.commitSync consumer (.getCurrentOffsets rebalance-listener))
             (.resetCurrentOffsets rebalance-listener))
         (catch Exception ex
           (log/warn (str "commit failed: " (.getMessage ex)))))
       (try
         (.close consumer)
         (catch Exception ex
           (log/warn (str "shutdown failed - " (.getMessage ex))))))))

(defn- register-jvm-exit-handler [^KafkaConsumer consumer]
  #?(:clj
     (let [^Thread main-thrd (Thread/currentThread)]
       (.addShutdownHook
        (Runtime/getRuntime)
        (Thread. ^Runnable (fn []
                             (.wakeup consumer)
                             (try
                               (.join main-thrd)
                               (catch Exception ex
                                 (log/error ex)))))))))

(defn listen [client]
  #?(:clj
     (let [conn (si/connection client)
           ^KafkaConsumer consumer (:consumer conn)
           ^HandleRebalance rebalance-listener (:rebalance-listener conn)
           config (:config conn)
           topic (or (:topic config) "fractl-events")
           dur-ms (Duration/ofMillis (or (:poll-duration-millis config) 1000))]

       ;; Note: Call `shutdown` at call-site instead of depending on shut-down-hooks.
       ;; If we find shut-down-hooks are needed at some point, enable the following call
       ;; to `register-jvm-exit-handler`:
       ;;;; (register-jvm-exit-handler consumer)

       (.subscribe consumer (Arrays/asList (into-array String [topic])))
       (loop [run true]
         (when run
           (let [continue
                 (try
                   (do (doseq [^ConsumerRecord record (.poll consumer dur-ms)]
                         (si/process-notification client (json/decode (.value record)))
                         (.addOffset rebalance-listener record))
                       (.commitAsync consumer (.getCurrentOffsets rebalance-listener) nil)
                       (.resetCurrentOffsets rebalance-listener)
                       true)
                   (catch WakeupException ex
                     (close-consumer consumer rebalance-listener)
                     false)
                   (catch Exception ex
                     (log/error (str "event-consumer error: " (.getMessage ex)))
                     true))]
             (recur continue)))))))

(defn shutdown [client]
  #?(:clj
     (let [conn (si/connection client)
           ^KafkaConsumer consumer (:consumer conn)]
       (.start (Thread. ^Runnable (fn [] (.wakeup consumer))))
       client)))
