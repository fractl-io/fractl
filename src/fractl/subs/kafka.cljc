(ns fractl.subs.kafka
  (:require [fractl.util.logger :as log]
            [fractl.datafmt.json :as json]
            [fractl.subs.internal :as si])
  #?(:clj
     (:import [java.time Duration]
              [java.util Arrays Properties]
              [org.apache.kafka.clients.consumer
               ConsumerConfig ConsumerRecord ConsumerRecords
               KafkaConsumer]
              [org.apache.kafka.common.serialization
               StringDeserializer StringSerializer])))

(defn make-consumer [config]
  #?(:clj
     (let [^Properties props (Properties.)]
       (.setProperty props ConsumerConfig/BOOTSTRAP_SERVERS_CONFIG (or (:servers config) "localhost:9092"))
       (let [provider-config (:provider config)]
         (.setProperty props ConsumerConfig/GROUP_ID_CONFIG (or (:group-id provider-config) "fractl-consumers"))
         (.setProperty props ConsumerConfig/AUTO_OFFSET_RESET_CONFIG (or (:auto-offset-rest provider-config) "earliest")))
       (.setProperty props ConsumerConfig/KEY_DESERIALIZER_CLASS_CONFIG "org.apache.kafka.common.serialization.StringDeserializer")
       (.setProperty props ConsumerConfig/VALUE_DESERIALIZER_CLASS_CONFIG "org.apache.kafka.common.serialization.StringDeserializer")
       {:handle (KafkaConsumer. props) :config config})))

(defn run [conn]
  #?(:clj
     (let [^KafkaConsumer consumer (:handle conn)
           config (:config conn)
           topic (or (:topic config) "fractl-events")
           dur-ms (Duration/ofMillis (or (:poll-duration-millis config) 1000))]
       (.subscribe consumer (Arrays/asList (into-array String [topic])))
       (loop []
         (try
           (doseq [record (.poll consumer dur-ms)]
             (si/process-notification (json/decode (.value record))))
           (catch Exception ex
             (log/error (str "event-consumer error: " (.getMessage ex)))))
         (recur)))))
