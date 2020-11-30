(ns fractl.http
  (:require [org.httpkit.server :as h]
            [ring.middleware.cors :as cors]
            [cheshire.core :as json]
            [taoensso.timbre :as log]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.lang.internal :as li])
  (:use [compojure.core :only [routes POST]]
        [compojure.route :only [not-found]]))

(defn- response
  "Create a Ring response from a map object and an HTTP status code.
   The map object will be encoded as JSON in the response.
   Also see: https://github.com/ring-clojure/ring/wiki/Creating-responses"
  [json-obj status]
  (let [r {:status status
           :headers {"Content-Type" "application/json"}
           :body (json/generate-string json-obj)}]
    (log/debug {:response r})
    r))

(defn- bad-request [s]
  (response {:reason s} 400))

(defn- internal-error [s]
  (response {:reason s} 500))

(defn- ok [obj]
  (response obj 200))

(defn- evaluate [evaluator event-instance]
  (try
    (ok (evaluator event-instance))
    (catch Exception ex
      (log/error ex)
      (internal-error (.getMessage ex)))))

(defn- event-from-request
  ([request event-name]
   (try
     (let [obj (json/parse-string (String. (.bytes (:body request))) true)
           obj-name (li/split-path (first (keys obj)))]
       (if (or (not event-name) (= obj-name event-name))
         [(cn/make-event-instance obj-name (first (vals obj))) nil]
         [nil (str "Type mismatch in request - " event-name " <> " obj-name)]))
     (catch Exception ex
       [nil (str "Failed to parse request - " (.getMessage ex))])))
  ([request] (event-from-request request nil)))

(defn- process-dynamic-eval [evaluator event-name request]
  (let [[obj err] (event-from-request request event-name)]
    (if err
      (bad-request err)
      (evaluate evaluator obj))))

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
