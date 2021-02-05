(ns fractl.util.http
  (:require #?(:clj [org.httpkit.client :as http]
               :cljs [cljs-http.client :as http])
            #?(:clj [cheshire.core :as json])
            [fractl.util.seq :as us]
            [fractl.datafmt.transit :as t]
            #?(:cljs [cljs.core.async :refer [<!]]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

(defn- json-parse-string [s]
  #?(:clj (json/parse-string s true)
     :cljs identity))

(def ^:private enc-dec
  {:json [json-parse-string #?(:clj json/generate-string
                               :cljs identity)]
   :transit+json [t/decode t/encode]})

(def content-types
  {"application/json" :json
   "application/transit+json" :transit+json})

(def ^:private datafmt-content-types (us/map-mirror content-types))

(defn encoder [data-fmt]
  (second (data-fmt enc-dec)))

(defn decoder [data-fmt]
  (first (data-fmt enc-dec)))

(def content-type (partial get datafmt-content-types))

(def entity-event-prefix "/_e/")
(def query-prefix "/_q/")
(def dynamic-eval-prefix "/_dynamic/")

(defn do-post
  ([url options request-obj format]
   (let [headers (assoc (:headers options) "Content-Type" (content-type format))
         options (assoc options :headers headers)
         body ((encoder format) request-obj)]
     #?(:clj @(http/post url (assoc options :body body))
        :cljs (go (let [response (<! (http/post url {:json-params body}))]
                    ((:cljs-response-handler options) response))))))
  ([url options request-obj]
   (do-post url options request-obj :json)))
