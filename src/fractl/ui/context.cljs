(ns fractl.ui.context
  (:require [fractl.component :as cn]
            [fractl.lang.internal :as li]))

(def ^:private db (atom {}))

(defn attach-to-context!
  ([obj is-auth-response]
   (let [n (li/split-path (cn/instance-name obj))]
     (swap! db assoc n obj)
     (when is-auth-response
       (swap! db assoc :auth obj))))
  ([obj]
   (attach-to-context! obj false)))

(defn reset-context! []
  (let [auth (:auth @db)]
    (reset! db {:auth auth})))

(defn hard-reset-context! []
  (reset! db {}))

(defn context-as-map []
  (dissoc @db :auth))

(defn lookup-ref [n path]
  (get-in @db (concat [n] path)))
