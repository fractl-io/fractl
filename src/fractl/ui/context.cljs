(ns fractl.ui.context
  (:require [fractl.component :as cn]
            [fractl.lang.internal :as li]))

(def ^:private db (atom {}))

(defn attach-to-context!
  ([obj is-auth-response]
   (let [[c n :as k] (li/split-path (cn/instance-name obj))]
     (swap! db assoc n obj k obj)
     (when is-auth-response
       (swap! db assoc :auth obj))))
  ([obj]
   (attach-to-context! obj false)))

(defn reset-context! []
  (let [auth (:auth @db)]
    (reset! db {:auth auth})))

(defn hard-reset-context! []
  (reset! db {}))

(defn context-as-map
  ([keyword-names-only]
   (let [c (dissoc @db :auth)]
     (if keyword-names-only
       (into
        {}
        (filter (fn [[k _]]
                  (keyword? k))
                c))
       c)))
  ([] (context-as-map true)))

(defn lookup-ref [n path]
  (get-in @db (concat [n] path)))

(def ^:private active-inst-key :active-instance)

(defn set-active-instance! [obj]
  (swap! db assoc active-inst-key obj))

(defn lookup-active-instance []
  (get @db active-inst-key))
