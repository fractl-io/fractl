(ns fractl.resolver.store
  (:require [fractl.store :as store]
            [fractl.component :as cn]
            [fractl.model.fractl.kernel.store]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rr
             #?(:clj :refer :cljs :refer-macros)
             [defmake]]))

(defn- handle-create [store instance]
  (case (cn/instance-type-kw instance)
    :Fractl.Kernel.Store/Changeset
    (store/plan-changeset instance)
    instance))

(defmake :store-migration
  (fn [resolver-name config]
    (let [store (:store config)]
      (r/make-resolver
       resolver-name
       {:create {:handler (partial handle-create store)}}))))

(rr/register-resolvers
 [{:name :store-migration
   :type :store-migration
   :compose? false
   :paths [:Fractl.Kernel.Store/Changeset]}])
