(ns fractl.ui.policy-resolver
  (:require [fractl.resolver.core :as r]
            [fractl.util :as u]
            [fractl.lang.internal :as li]))

(def ^:private db (atom []))

(defn lookup-policies [intercept resource]
  (filter #(and (= (:Intercept %) intercept)
                (= (:Resource %) (if (keyword? resource)
                                   resource
                                   (li/make-path resource))))
          @db))

(defn- policy-upsert [instance]
  (let [r (:Resource instance)
        inst (assoc
              instance
              :Intercept (u/string-as-keyword (:Intercept instance))
              :Resource (cond
                          (keyword? r) r
                          (string? r) (keyword r)
                          :else (li/make-path r)))]
    (swap! db conj inst)
    instance))

(def ^:private resolver-fns
  {:upsert {:handler policy-upsert}})

(defn make
  "Create and return a policy resolver"
  [resolver-name]
  (r/make-resolver resolver-name resolver-fns))   
