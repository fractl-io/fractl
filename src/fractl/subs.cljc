(ns fractl.subs
  "Subsriptions for external events for handling out-of-bands change-notifications from
backend-systems. A resolver can express interest in these events by implementing the
:on-change-notification method."
  (:require [fractl.util.logger :as log]
            [fractl.component :as cn]
            [fractl.datafmt.json :as json]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg])
  #?(:clj
     (:import [java.time Duration]
              [java.util Arrays Properties]
              [org.apache.kafka.clients.consumer
               ConsumerConfig ConsumerRecord ConsumerRecords
               KafkaConsumer]
              [org.apache.kafka.common.serialization
               StringDeserializer StringSerializer])))

(defn make-consumer
  ([config]
   #?(:clj
      (let [^Properties props (Properties.)]
        (.setProperty props ConsumerConfig/BOOTSTRAP_SERVERS_CONFIG (or (:servers config) "localhost:9092"))
        (let [provider-config (:provider config)]
          (.setProperty props ConsumerConfig/GROUP_ID_CONFIG (or (:group-id provider-config) "fractl-consumers"))
          (.setProperty props ConsumerConfig/AUTO_OFFSET_RESET_CONFIG (or (:auto-offset-rest provider-config) "earliest")))
        (.setProperty props ConsumerConfig/KEY_DESERIALIZER_CLASS_CONFIG "org.apache.kafka.common.serialization.StringDeserializer")
        (.setProperty props ConsumerConfig/VALUE_DESERIALIZER_CLASS_CONFIG "org.apache.kafka.common.serialization.StringDeserializer")
        {:handle (KafkaConsumer. props) :config config})))
  ([] (make-consumer nil)))

(defn- notification-object? [obj]
  (and (map? obj)
       (map? (:instance obj))
       (some #{(:operation obj)} [:create :update :delete])
       true))

(defn- process-notification [obj]
  (if (notification-object? obj)
    (let [instance (cn/make-instance (:instance obj))
          old-instance (when-let [old (:old-instance obj)]
                         (cn/make-instance old))]
      (when-let [r (rg/resolver-for-path (cn/instance-type instance))]
        (r/call-resolver-on-change-notification r nil {:instance instance
                                                       :old-instance old-instance
                                                       :operation (:operation obj)})))
    (log/warn (str "not a notification object: " obj))))

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
             (process-notification (json/decode (.value record))))
           (catch Exception ex
             (log/error (str "event-consumer error: " (.getMessage ex)))))
         (recur)))))
