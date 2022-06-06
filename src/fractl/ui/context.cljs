(ns fractl.ui.context
  (:require [fractl.component :as cn]
            [fractl.lang :as l]
            [fractl.lang.internal :as li]))

(def ^:private db (atom {}))

(defn- owner
  ([k obj]
   (when (l/kernel-auth-name? k)
     (l/auth-owner obj)))
  ([obj]
   (owner (li/split-path (cn/instance-type obj)) obj)))

(defn- assoc-to-context! [obj]
  (when obj
    (let [[c n :as k] (li/split-path (cn/instance-type obj))]
      (swap! db assoc n obj k obj))))

(defn attach-to-context!
  ([obj is-auth-response]
   (assoc-to-context! obj)
   (when is-auth-response
     (assoc-to-context! (owner obj))
     (swap! db assoc :auth obj)))
  ([obj]
   (attach-to-context! obj false)))

(defn reset-context! []
  (let [auth (:auth @db)]
    (reset! db {:auth auth})
    (assoc-to-context! auth)
    (assoc-to-context! (owner auth))))

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
