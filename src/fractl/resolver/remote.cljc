(ns fractl.resolver.remote
  (:require [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.util.http :as uh]
            [fractl.lang.internal :as li]
            [fractl.resolver.core :as r]))

(defn- event-name [event-type inst]
  (let [[component inst-name] (li/split-path (cn/instance-name inst))]
    (str (name component) "/" (name event-type) "_" (name inst-name))))

(defn- response-handler [callback response]
  ((or callback identity)
   (if (map? response)
     (let [status (:status response)]
       (if (< 199 status 299)
         #?(:clj
            ((uh/decoder format) (:body response))
            :cljs (:body response))
         (u/throw-ex (str "remote resolver error - " response))))
     response)))

(defn- do-post
  ([url options request-obj format]
   (uh/do-post
    url (dissoc options :callback)
    request-obj format (partial response-handler (:callback options))))
  ([url options request-obj]
   (do-post url options request-obj :transit+json)))

(defn- remote-request [event-type mkobj host options inst]
  (let [en (event-name event-type inst)
        url (str host uh/entity-event-prefix en)
        request-obj {en (if mkobj
                          (mkobj inst)
                          {:Instance inst})}]
    (do-post url options request-obj)))

(defn- mk-lookup-obj [inst]
  {:Id (:Id inst)})

(def ^:private remote-upsert (partial remote-request :Upsert nil))
(def ^:private remote-delete (partial remote-request :Delete nil))
(def ^:private remote-get (partial remote-request :Lookup mk-lookup-obj))

(defn- remote-query [host options query]
  (let [response (do-post
                  (str host uh/query-prefix)
                  options {:Query query})]
    (if (map? response)
      (first (:body response))
      response)))

(defn remote-eval [host options event-inst]
  (do-post (str host uh/dynamic-eval-prefix) options event-inst))

(def ^:private resolver-fns
  {:upsert {:handler remote-upsert}
   :delete {:handler remote-delete}
   :get {:handler remote-get}
   :query {:handler remote-query}
   :eval {:handler remote-eval}})

(defn- with-required-options [options]
  (merge {:timeout 1000} options))

(defn make [resolver-name config]
  (let [host (:host config)
        options (with-required-options (dissoc config :host))
        handlers (map (fn [[k res]]
                        [k {:handler (partial (:handler res) host options)}])
                      resolver-fns)]
    (r/make-resolver resolver-name (into {} handlers))))
