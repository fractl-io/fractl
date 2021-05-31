(ns fractl.util.logger
  #?(:clj  (:require [active.clojure.config :as config]
                     [active.clojure.logger.event :as event-logger]
                     [active.clojure.logger.config.timbre :as timbre-config])
     :cljs (:require [lambdaisland.glogi :as log]
                     [lambdaisland.glogi.console :as log-console])))
#?(:clj
   (do
     (def config-map
       {:timbre-config
        {:level :info
         :appenders
         ;; Store a fractl.log at project path, the max permitted  size is ~1MB\
         ;; Only 5 numbers of rotated logs are kept.
                {:rotor '(rotor {:path     "fractl.log"
                                 :max-size 1048576
                                 :backlog  5})
                 ;; print to terminal/console
                 ;:println '(println)
                 ;; Submit messages directly to a logstash instance.
                 ;:logstash '(logstash "localhost" 5044)
                 ;; Send direct streams to reimann instance.
                 ;:riemann  '(riemann {:host "localhost"
                 ;                     :port 5555})
                 }}})

     (def schema (config/schema "Configuration schema." timbre-config/timbre-config-section))
     (def config (config/make-configuration schema [] config-map))

     ;; Initialize the event-logger.
     (-> config
         (config/section-subconfig timbre-config/timbre-config-section)
         timbre-config/configure-events-logging
         event-logger/set-global-log-events-config!)

     (defn error [msg]
       (event-logger/log-event! :error msg))

     (defn debug [msg]
       (event-logger/log-event! :debug msg))

     (defn info [msg]
       (event-logger/log-event! :info msg))

     (defn warn [msg]
       (event-logger/log-event! :warn msg)))

   :cljs
   (do
     ;; Detect whether cljs-tools is running else, print to terminal.
     (log-console/install!)

     (log/set-levels
       {:glogi/root   :info                                 ;; Set a root logger level, this will be inherited by all loggers
        'fractl.lang  :trace                                ;; Some namespaces you might want detailed logging
        'fractl.store :error                                ;; or for others you only want to see errors.
        })

     (defn info
       "This is default info for fractl lang to use.
       If other form inputs are required no need to import this."
       [msg]
       (log/info :fractl {:message msg}))

     (defn error
       "This is default info for fractl lang to use.
       If other form inputs are required no need to import this."
       [msg]
       (log/error :fractl {:message msg}))

     (defn debug
       "This is default info for fractl lang to use.
       If other form inputs are required no need to import this."
       [msg]
       (log/debug :fractl {:message msg}))

     (defn warn
       "This is default info for fractl lang to use.
       If other form inputs are required no need to import this."
       [msg]
       (log/warn :fractl {:message msg}))))
