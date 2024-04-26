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

(ln/component :Camel)
(ln/record
 :Camel/EventTemplate
 {:UserArg :String
  :Response {:type :Any :optional true}})

(gen-class
 :name fractl.resolver.camel.MyBean
 :prefix "-"
 :main false
 ;; declare only new methods, not superclass methods
 :methods [[callback [Object] void]])

(defn -callback [_ obj]
  (println "Change event:" obj))

(defn- endpoint [event-name]
  (get-in (cn/fetch-meta event-name) [:trigger :endpoint]))

(defn- ^CamelContext context-for-endpoint [config endpoint]
  (if (s/starts-with? endpoint "salesforce")
    (let [^SalesforceComponent sfc (SalesforceComponent.)
          config (or (:salesforce config) config)
          inst-url (or (:instance-url config)
                       (u/getenv "SFDC_INSTANCE_URL"))]
      (.setClientId sfc (or (:client-id config)
                            (u/getenv "SFDC_CLIENT_ID")))
      (.setClientSecret sfc (or (:client-secret config)
                                (u/getenv "SFDC_CLIENT_SECRET")))
      (.setAuthenticationType sfc (AuthenticationType/valueOf "CLIENT_CREDENTIALS"))
      (.setInstanceUrl sfc inst-url)
      (.setLoginUrl sfc inst-url)
      (let [^CamelContext ctx (DefaultCamelContext.)]
        (.addComponent ctx "salesforce" sfc)
        ctx))
    (u/throw-ex (str "component not supported for endpoint - " endpoint))))

(defn camel-eval [config event-instance]
  (let [ep (endpoint (cn/instance-type event-instance))
        user-arg (:UserArg event-instance)
        has-arg (seq user-arg)
        ^CamelContext ctx (context-for-endpoint config ep)
        ^RouteBuilder rb (proxy [RouteBuilder] []
                           (configure []
                             #_(-> this
                                 (.from "salesforce:subscribe:/data/ChangeEvents?rawPayload=true")
	                         (.bean (fractl.resolver.camel.MyBean.), "callback"))
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
                                                  (ev/eval-all-dataflows (assoc event-instance :Response body))))))))))]
    (.addRoutes ctx rb)
    (.start ctx)
    (when has-arg
      (let [^ProducerTemplate t (.createProducerTemplate ctx)]
        (.requestBody t "direct:send" user-arg String)))
    ;;(.stop ctx)
    event-instance))

(defn- camel-on-set-path [_ [tag path]]
  (when-not (= tag :override)
    (u/throw-ex (str path " needs an override resolver")))
  (when-not (cn/event? path)
    (u/throw-ex (str path " is not an event")))
  (when-not (cn/inherits? :Camel/EventTemplate path)
    (u/throw-ex (str path " must inherit :Camel/EventTemplate")))
  (when-not (endpoint path)
    (u/throw-ex (str path " event does not define a trigger endpoint")))
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
