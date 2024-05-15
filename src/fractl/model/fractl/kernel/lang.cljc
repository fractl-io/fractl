(ns
 fractl.model.fractl.kernel.lang
 (:require
  [fractl.util :as u]
  [fractl.lang.kernel :as k]
  [fractl.lang.internal :as li]
  [fractl.lang.datetime :as dt]
  [fractl.resolver.registry :as r]
  [fractl.component :as cn])
 (:use
  [fractl.lang
   :only
   [dataflow
    entity
    view
    attribute
    rule
    relationship
    component
    event
    inference
    record]]))
(component
 :Fractl.Kernel.Lang
 {:clj-import
  '[(:require
     [fractl.util :as u]
     [fractl.lang.kernel :as k]
     [fractl.lang.internal :as li]
     [fractl.lang.datetime :as dt]
     [fractl.resolver.registry :as r]
     [fractl.component :as cn])]})
(attribute :Fractl.Kernel.Lang/String {:check k/kernel-string?})
(attribute
 :Fractl.Kernel.Lang/Keyword
 {:check (fn* [p1__282#] (or (keyword? p1__282#) (string? p1__282#)))})
(attribute :Fractl.Kernel.Lang/Path {:check k/path?})
(attribute :Fractl.Kernel.Lang/DateTime {:check k/date-time?})
(attribute :Fractl.Kernel.Lang/Date {:check k/date?})
(attribute :Fractl.Kernel.Lang/Time {:check k/time?})
(attribute :Fractl.Kernel.Lang/UUID {:check k/UUID?})
(attribute :Fractl.Kernel.Lang/Int {:check int?})
(attribute :Fractl.Kernel.Lang/Int64 {:check int?})
(attribute :Fractl.Kernel.Lang/BigInteger {:check integer?})
(attribute :Fractl.Kernel.Lang/Float {:check k/kernel-float?})
(attribute :Fractl.Kernel.Lang/Double {:check k/kernel-double?})
(attribute :Fractl.Kernel.Lang/Decimal {:check cn/decimal-value?})
(attribute :Fractl.Kernel.Lang/Boolean {:check boolean?})
(attribute :Fractl.Kernel.Lang/Record {:check cn/record-instance?})
(attribute :Fractl.Kernel.Lang/Entity {:check cn/entity-instance?})
(attribute :Fractl.Kernel.Lang/Event {:check cn/event-instance?})
(attribute :Fractl.Kernel.Lang/Any {:check k/any-obj?})
(attribute :Fractl.Kernel.Lang/Email {:check k/email?})
(attribute :Fractl.Kernel.Lang/Map {:check map?})
(attribute :Fractl.Kernel.Lang/Edn {:check k/edn?})
(attribute
 :Fractl.Kernel.Lang/Identity
 {:type :Fractl.Kernel.Lang/UUID, :default u/uuid-string, li/guid true})
(attribute
 :Fractl.Kernel.Lang/Now
 {:type :Fractl.Kernel.Lang/DateTime, :default dt/now})
(attribute
 (k/event-context-attribute-name)
 (k/event-context-attribute-schema))
(attribute
 :Fractl.Kernel.Lang/Password
 {:type :Fractl.Kernel.Lang/String, :secure-hash true})
(record
 :Fractl.Kernel.Lang/Future
 {:Result :Fractl.Kernel.Lang/Any,
  :TimeoutMillis {:type :Fractl.Kernel.Lang/Int, :default 2000}})
(entity
 :Fractl.Kernel.Lang/Policy
 {:Intercept {:type :Fractl.Kernel.Lang/Keyword, :indexed true},
  :Resource {:type :Fractl.Kernel.Lang/Path, :indexed true},
  :Spec :Fractl.Kernel.Lang/Edn,
  :InterceptStage
  {:oneof [:PreEval :PostEval :Default], :default :Default}})
(entity
 :Fractl.Kernel.Lang/Timer
 {:Expiry :Fractl.Kernel.Lang/Int,
  :ExpiryUnit
  {:oneof [:Seconds :Minutes :Hours :Days], :default :Seconds},
  :ExpiryEvent :Fractl.Kernel.Lang/Map,
  :TaskHandle {:type :Fractl.Kernel.Lang/Any, :optional true}})
(dataflow
 :Fractl.Kernel.Lang/LoadPolicies
 #:Fractl.Kernel.Lang{:Policy
                      {:Intercept?
                       :Fractl.Kernel.Lang/LoadPolicies.Intercept,
                       :Resource?
                       :Fractl.Kernel.Lang/LoadPolicies.Resource}})
(event :Fractl.Kernel.Lang/AppInit {:Data :Fractl.Kernel.Lang/Map})
(event :Fractl.Kernel.Lang/InitConfig {})
(record
 :Fractl.Kernel.Lang/InitConfigResult
 {:Data {:listof :Fractl.Kernel.Lang/Map}})
(record
 :Fractl.Kernel.Lang/DataSource
 {:Uri {:type :Fractl.Kernel.Lang/String, :optional true},
  :Entity :Fractl.Kernel.Lang/String,
  :AttributeMapping {:type :Fractl.Kernel.Lang/Map, :optional true}})
(event
 :Fractl.Kernel.Lang/DataSync
 {:Source :Fractl.Kernel.Lang/DataSource,
  :DestinationUri {:type :Fractl.Kernel.Lang/String, :optional true}})
(r/register-resolvers
 [{:name :meta,
   :type :meta,
   :compose? false,
   :config
   {:fractl-api
    {:component component,
     :entity entity,
     :event event,
     :record record,
     :dataflow dataflow}},
   :paths [:Fractl.Kernel.Lang/LoadModelFromMeta]}
  {:name :timer,
   :type :timer,
   :compose? false,
   :paths [:Fractl.Kernel.Lang/Timer]}
  (when
   (u/host-is-jvm?)
   {:name :data-sync,
    :type :data-sync,
    :compose? false,
    :paths [:Fractl.Kernel.Lang/DataSync]})])
(def
 Fractl_Kernel_Lang___COMPONENT_ID__
 "a2cb1971-bbdb-4242-8c5a-1c9285c9ccc5")
