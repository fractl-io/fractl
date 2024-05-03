(ns fractl.evaluator.camel
  (:require [clojure.string :as s]
            [clojure.core.async :as async]
            [fractl.lang :as ln]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.evaluator :as ev]
            [fractl.lang.internal :as li])
  (:import [org.apache.camel CamelContext Component
            ProducerTemplate Processor Exchange Message]
           [org.apache.camel.impl DefaultCamelContext]
           [org.apache.camel.builder RouteBuilder]
           [org.apache.camel.support DefaultComponent]))

(defn- context-for-endpoint [endpoint ^Component c]
  (let [^CamelContext ctx (DefaultCamelContext.)
        n (subs endpoint 0 (s/index-of endpoint ":"))]
    (when c (.addComponent ctx n c))
    ctx))

(defn exec-route
  ([request is-blocking]
   (let [ep (:endpoint request)
         user-arg (:user-arg request)
         chan (when is-blocking (async/chan 1))
         ^CamelContext ctx (context-for-endpoint ep (:camel-component request))
         ^RouteBuilder rb (proxy [RouteBuilder] []
                            (configure []
                              (let [p (if user-arg
                                        (-> this
                                            (.from "direct:send")
                                            (.to ep))
                                        (-> this (.from ep)))]
                                (-> p
                                    (.convertBodyTo String)
                                    (.process (reify Processor
                                                (^void process [_ ^Exchange exchange]
                                                 (let [^Message msg (.getIn exchange)
                                                       body (.getBody msg)]
                                                   (if is-blocking
                                                     (async/>!! chan body)
                                                     ((:callback request) body))))))))))]
     (.addRoutes ctx rb)
     (.start ctx)
     (when user-arg
       (let [^ProducerTemplate t (.createProducerTemplate ctx)]
         (try
           (.requestBody t "direct:send" user-arg (or (:user-arg-type request) String))
           (catch Exception ex
             (log/error ex)))))
     #_(when-not (or (:is-service request) (:callback request))
         (.stop ctx))
     (when is-blocking
       (first (async/alts!! [chan (async/timeout 10000)])))))
  ([request] (exec-route request true)))

(defn exec-route-with-dataflow-callback [request]
  (let [cb (:callback request)]
    (exec-route
     (assoc request :callback #(ev/eval-all-dataflows (cb %)))
     false)))
