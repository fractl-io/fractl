(ns fractl.resolver.remote
  (:require [fractl.component :as cn]
            [fractl.util.http :as uh]
            [fractl.lang.internal :as li]
            [fractl.resolver.core :as r]))

(defn- event-name [event-type inst]
  (let [[component inst-name] (li/split-path (cn/instance-name inst))]
    (str (name component) "/" (name event-type) "_" inst-name)))

(defn- remote-request [event-type mkobj host options inst]
  (let [en (event-name event-type inst)
        url (str host uh/entity-event-prefix en)
        request-obj {en (if mkobj
                          (mkobj inst)
                          {:Instance inst})}]
    (uh/do-post url options request-obj)))

(defn- mk-lookup-obj [inst]
  {:Id (:Id inst)})

(def ^:private remote-upsert (partial remote-request :Upsert nil))
(def ^:private remote-delete (partial remote-request :Delete nil))
(def ^:private remote-get (partial remote-request :Lookup mk-lookup-obj))

(defn- remote-query [host options query]
  (uh/do-post (str host uh/query-prefix) options {:Query query}))

(defn- remote-eval [host options event-inst]
  (uh/do-post (str host uh/dynamic-eval-prefix) options event-inst))

(def ^:private resolver-fns
  {:upsert remote-upsert
   :delete remote-delete
   :get remote-get
   :query remote-query
   :eval remote-eval})

(defn- with-required-options [options]
  (let [hdrs (assoc (:headers options) "Content-Type" "application/json")]
    (merge {:timeout 1000}
           (assoc options :headers hdrs))))

(defn make [resolver-name config]
  (let [host (:host config)
        options (with-required-options (dissoc config :host))
        handlers (map (fn [[k f]]
                        [k (partial f host options)])
                      resolver-fns)]
    (r/make-resolver resolver-name (into {} handlers))))
