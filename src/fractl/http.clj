(ns fractl.http
  (:require [clojure.walk :as w]
            [clojure.string :as s]
            [org.httpkit.server :as h]
            [ring.middleware.cors :as cors]
            [cheshire.core :as json]
            [taoensso.timbre :as log]
            [fractl.util :as u]
            [fractl.util.seq :as us]
            [fractl.component :as cn]
            [fractl.datafmt.transit :as t]
            [fractl.lang.internal :as li])
  (:use [compojure.core :only [routes POST]]
        [compojure.route :only [not-found]]))

(defn- json-parse-string [s]
  (json/parse-string s true))

(def ^:private enc-dec
  {:json [json-parse-string json/generate-string]
   :transit+json [t/decode t/encode]})

(def ^:private content-types
  {"application/json" :json
   "application/transit+json" :transit+json})

(def ^:private datafmt-content-types (us/map-mirror content-types))

(defn- encoder [data-fmt]
  (second (data-fmt enc-dec)))

(defn- decoder [data-fmt]
  (first (data-fmt enc-dec)))

(def ^:private content-type (partial get datafmt-content-types))

(defn- response
  "Create a Ring response from a map object and an HTTP status code.
   The map object will be encoded as JSON in the response.
   Also see: https://github.com/ring-clojure/ring/wiki/Creating-responses"
  [json-obj status data-fmt]
  (let [r {:status status
           :headers {"Content-Type" (content-type data-fmt)}
           :body ((encoder data-fmt) json-obj)}]
    (log/debug {:response r})
    r))

(defn- bad-request
  ([s data-fmt]
   (response {:reason s} 400 data-fmt))
  ([s] (bad-request s :json)))

(defn- internal-error
  ([s data-fmt]
   (response {:reason s} 500 data-fmt))
  ([s] (internal-error s :json)))

(defn- ok [obj data-fmt]
  (response obj 200 data-fmt))

(defn- maybe-remove-read-only-attributes [obj]
  (if (cn/an-instance? obj)
    (cn/dissoc-write-only obj)
    obj))

(defn- remove-all-read-only-attributes [obj]
  (w/prewalk maybe-remove-read-only-attributes obj))

(defn- evaluate [evaluator event-instance data-fmt]
  (try
    (let [result (remove-all-read-only-attributes
                  (evaluator event-instance))]
      (ok result data-fmt))
    (catch Exception ex
      (log/error ex)
      (internal-error (.getMessage ex) data-fmt))))

(defn- event-from-request [request event-name data-fmt]
  (try
    (let [obj ((decoder data-fmt) (String. (.bytes (:body request))))
          obj-name (li/split-path (first (keys obj)))]
      (if (or (not event-name) (= obj-name event-name))
        [(cn/make-event-instance obj-name (first (vals obj))) nil]
        [nil (str "Type mismatch in request - " event-name " <> " obj-name)]))
    (catch Exception ex
      [nil (str "Failed to parse request - " (.getMessage ex))])))

(defn- request-content-type [request]
  (s/lower-case
   (get-in request [:headers "content-type"])))

(defn- find-data-format [request]
  (let [ct (request-content-type request)]
    (get content-types ct)))

(defn- process-dynamic-eval [evaluator event-name request]
  (if-let [data-fmt (find-data-format request)]
    (let [[obj err] (event-from-request request event-name data-fmt)]
      (if err
        (bad-request err data-fmt)
        (evaluate evaluator obj data-fmt)))
    (bad-request
     (str "unsupported content-type in request - "
          (request-content-type request)))))

(defn- process-request [evaluator request]
  (let [params (:params request)
        component (keyword (:component params))
        event (keyword (:event params))
        n [component event]]
    (if (cn/find-event-schema n)
      (process-dynamic-eval evaluator n request)
      (bad-request (str "Event not found - " n)))))

(defn- process-query [request]
  (internal-error "Remote query processing not implemented yet"))

(def entity-event-prefix "/_e/")
(def query-prefix "/_q/")
(def dynamic-eval-prefix "/_dynamic/")

(defn- make-routes [process-request process-dynamic-eval]
  (let [r (apply routes [(POST (str entity-event-prefix ":component/:event") [] process-request)
                         (POST query-prefix [] process-query)
                         (POST dynamic-eval-prefix [] process-dynamic-eval)
                         (not-found "<p>Resource not found.</p>")])]
    (cors/wrap-cors
     r :access-control-allow-origin [#".*"]
     :access-control-allow-headers ["Content-Type"]
     :access-control-allow-credentials true
     :access-control-allow-methods [:post])))

(defn run-server
  ([evaluator config]
   (h/run-server (make-routes (partial process-request evaluator)
                              (partial process-dynamic-eval evaluator nil))
                 (if (:thread config)
                   config
                   (assoc config :thread (+ 1 (u/n-cpu))))))
  ([evaluator]
   (run-server evaluator {:port 8080})))
