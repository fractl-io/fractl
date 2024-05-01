(ns fractl.evaluator.camel
  (:require [clojure.string :as s]
            [clojure.core.async :as async]
            [fractl.lang :as ln]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.lang.internal :as li])
  (:import [org.apache.camel CamelContext Component
            ProducerTemplate Processor Exchange Message]
           [org.apache.camel.impl DefaultCamelContext]
           [org.apache.camel.builder RouteBuilder]
           [org.apache.camel.support DefaultComponent]))

(defn- context-for-endpoint [endpoint ^Component c]
  (let [^CamelContext ctx (DefaultCamelContext.)
        n (subs endpoint 0 (s/index-of endpoint ":"))]
    (.addComponent ctx n c)
    ctx))

(defn exec-route
  ([request is-blocking]
   (let [ep (:endpoint request)
         user-arg (:user-arg request)
         has-arg (seq user-arg)
         chan (when is-blocking (async/chan 1))
         ^CamelContext ctx (context-for-endpoint ep (:camel-component request))
         ^RouteBuilder rb (proxy [RouteBuilder] []
                            (configure []
                              (let [p (if has-arg
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
     (when has-arg
       (let [^ProducerTemplate t (.createProducerTemplate ctx)]
         (.requestBody t "direct:send" user-arg String)))
     (when-not (or (:is-service request) (:callback request))
       (.stop ctx))
     (when is-blocking
       (async/<!! chan))))
  ([request] (exec-route request true)))
