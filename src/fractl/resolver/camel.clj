(ns fractl.resolver.camel
  (:require [clojure.string :as s]
            [fractl.lang :as ln]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [fractl.lang.internal :as li]
            [fractl.evaluator :as ev])
  (:import [org.apache.camel CamelContext ProducerTemplate Processor Exchange Message]
           [org.apache.camel.impl DefaultCamelContext]
           [org.apache.camel.builder RouteBuilder]
           [org.apache.camel.support DefaultComponent]
           [org.apache.camel.component.salesforce SalesforceComponent AuthenticationType]))

(defn- endpoint [event-name]
  (get-in (cn/fetch-meta event-name) [:trigger :endpoint]))

(defn- ^CamelContext context-for-endpoint [config endpoint]
  (if (s/starts-with? endpoint "salesforce")
    (let [^SalesforceComponent sfc (SalesforceComponent.)
          config (or (:salesforce config) config)
          inst-url (or (:instance-url config)
                       (u/getenv "SFDC_INSTANCE_URL"))]
      (.setInstanceUrl sfc inst-url)
      (.setClientId sfc (or (:client-id config)
                            (u/getenv "SFDC_CLIENT_ID")))
      (.setClientSecret sfc (or (:client-secret config)
                                (u/getenv "SFDC_CLIENT_SECRET")))
      (.setAuthenticationType sfc (AuthenticationType/valueOf "CLIENT_CREDENTIALS"))
      (.setLoginUrl sfc inst-url)
      (let [^CamelContext ctx (DefaultCamelContext.)]
        (.addComponent ctx "salesforce" sfc)
        ctx))
    (u/throw-ex (str "component not supported for endpoint - " endpoint))))

(defn camel-eval [config event-instance]
  (let [ep (endpoint (cn/instance-type event-instance))
        ^CamelContext ctx (context-for-endpoint config ep)
        ^RouteBuilder rb (proxy [RouteBuilder] []
                           (configure []
                             (-> this
                                 (.from "direct:send")
                                 (.to ep)
                                 (.convertBodyTo String)
                                 (.process (reify Processor
                                             (^void process [_ ^Exchange exchange]
                                              (let [^Message msg (.getIn exchange)
                                                    body (.getBody msg)]
                                                (ev/eval-all-dataflows (assoc event-instance :Response body)))))))))]
    (.addRoutes ctx rb)
    (.start ctx)
    (when-let [arg (:UserArg event-instance)]
      (let [^ProducerTemplate t (.createProducerTemplate ctx)]
        (.requestBody t "direct:send" arg String)))
    (.stop ctx)
    event-instance))

(defn- validate-attribute [event-schema attr-name]
  (if-let [t (cn/attribute-type event-schema attr-name)]
    (when-not (= :String (second (li/split-path t)))
      (u/throw-ex (str (name attr-name) " attribute must be of String type")))
    (u/throw-ex (str "no " (name attr-name) " attribute defined for event"))))

(defn- validate-event-schema [event-name]
  (let [scm (cn/fetch-event-schema event-name)]
    (doseq [a [:UserArg :Response]] (validate-attribute scm a))))

(defn- camel-on-set-path [_ [tag path]]
  (when-not (= tag :override)
    (u/throw-ex (str path " needs an override resolver")))
  (when-not (cn/event? path)
    (u/throw-ex (str path " is not an event")))
  (when-not (endpoint path)
    (u/throw-ex (str path " event does not define a trigger endpoint")))
  (validate-event-schema path)
  (when-not (cn/find-dataflows path)
    (u/throw-ex (str "no dataflow is defined for " path)))
  (let [[c n] (li/split-path path)
        trigger-event (li/make-path c (keyword (str "Trigger" (name n))))]
    (ln/dataflow
     trigger-event
     {path {:UserArg (li/make-ref trigger-event :UserArg)}})
    path))

(rg/defmake :camel
  (fn [resolver-name config]
    (r/make-resolver
     resolver-name
     {:eval (partial camel-eval config)
      :on-set-path (partial camel-on-set-path config)})))
