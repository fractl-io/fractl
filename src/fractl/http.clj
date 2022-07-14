(ns fractl.http
  (:require [clojure.walk :as w]
            [clojure.string :as s]
            [org.httpkit.server :as h]
            [ring.middleware.cors :as cors]
            [ring.util.codec :as codec]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.util.http :as uh]
            [fractl.component :as cn]
            [fractl.lang.internal :as li])
  (:use [compojure.core :only [routes POST GET]]
        [compojure.route :only [not-found]]))

(def entity-event-prefix "/_e/")
(def query-prefix "/_q/")
(def dynamic-eval-prefix "/_dynamic/")
(def callback-prefix "/_callback/")

(defn- response
  "Create a Ring response from a map object and an HTTP status code.
   The map object will be encoded as JSON in the response.
   Also see: https://github.com/ring-clojure/ring/wiki/Creating-responses"
  [json-obj status data-fmt]
  (let [r {:status status
           :headers {"Content-Type" (uh/content-type data-fmt)}
           :body ((uh/encoder data-fmt) json-obj)}]
    r))

(defn- bad-request
  ([s data-fmt]
   (response {:reason s} 400 data-fmt))
  ([s] (bad-request s :json)))

(defn- internal-error
  ([s data-fmt]
   (response {:reason s} 500 data-fmt))
  ([s] (internal-error s :json)))

(defn- ok
  ([obj data-fmt]
   (response obj 200 data-fmt))
  ([obj]
   (ok obj :json)))

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
      (log/exception ex)
      (internal-error (.getMessage ex) data-fmt))))

(defn- event-from-request [request event-name data-fmt]
  (try
    (let [body (:body request)
          obj (if (map? body)
                body
                ((uh/decoder data-fmt)
                 (String.
                  (.bytes body)
                  java.nio.charset.StandardCharsets/UTF_8)))
          obj-name (or (cn/instance-type obj)
                       (li/split-path
                        (u/string-as-keyword
                         (first (keys obj)))))]
      (if (or (not event-name) (= obj-name event-name))
        [(if (cn/an-instance? obj) obj (cn/make-event-instance obj-name (first (vals obj)))) nil]
        [nil (str "Type mismatch in request - " event-name " <> " obj-name)]))
    (catch Exception ex
      (log/exception ex)
      [nil (str "Failed to parse request - " (.getMessage ex))])))

(defn- request-content-type [request]
  (s/lower-case
   (or (get-in request [:headers "content-type"])
       "application/json")))

(defn- find-data-format [request]
  (let [ct (request-content-type request)]
    (uh/content-types ct)))

(defn- process-dynamic-eval [evaluator event-name request]
  (if-let [data-fmt (find-data-format request)]
    (let [[obj err] (event-from-request request event-name data-fmt)]
      (if err
        (bad-request err data-fmt)
        (evaluate evaluator obj data-fmt)))
    (bad-request
     (str "unsupported content-type in request - "
          (request-content-type request)))))

(defn- paths-info [component]
  (mapv (fn [n] {(subs (str n) 1)
                 {"post" {"parameters" (cn/event-schema n)}}})
        (cn/event-names component)))

(defn- schemas-info [component]
  (mapv (fn [n] {n (cn/entity-schema n)})
        (cn/entity-names component)))

(defn- process-meta-request [request]
  (let [c (keyword (get-in request [:params :component]))]
    (ok {:paths (paths-info c) :schemas (schemas-info c)})))

(defn process-request [evaluator request]
  (let [params (:params request)
        component (keyword (:component params))
        event (keyword (:event params))
        n [component event]]
    (if (cn/find-event-schema n)
      (process-dynamic-eval evaluator n request)
      (bad-request (str "Event not found - " n)))))

(defn- like-pattern? [x]
  ;; For patterns that include the `_` wildcard,
  ;; the caller should provide an explicit where clause:
  ;;  {:from :EntityName
  ;;   :where [:like :AttributeName "pattern%"]}
  (and (string? x)
       (s/includes? x "%")))

(defn- filter-as-where-clause [[k v]]
  (let [n (u/string-as-keyword k)]
    (cond
      (vector? v) [(u/string-as-keyword (first v))
                   n (second v)]
      (like-pattern? v) [:like n v]
      :else [:= n v])))

(defn- preprocess-query [q]
  (if-let [fls (:filters q)]
    (let [or-cond (:or fls)
          f (or or-cond fls)]
      (assoc
       (dissoc q :filters)
       :where
       (let [r (mapv filter-as-where-clause f)]
         (if (= 1 (count r))
           (first r)
           `[~(if or-cond :or :and) ~@r]))))
    q))

(defn do-query [query-fn request-obj data-fmt]
  (if-let [q (preprocess-query (:Query request-obj))]
    (let [result (query-fn (li/split-path (:from q)) q)]
      (ok (first result) data-fmt))
    (bad-request (str "not a valid query request - " request-obj))))

(defn- process-query [_ query-fn request]
  (try
    (if-let [data-fmt (find-data-format request)]
      (do-query
       query-fn
       ((uh/decoder data-fmt) (String. (.bytes (:body request))))
       data-fmt)
      (bad-request
       (str "unsupported content-type in request - "
            (request-content-type request))))
    (catch Exception ex
      (log/exception ex)
      (internal-error (str "Failed to process query request - " (.getMessage ex))))))

(defn- make-routes [process-request process-query process-dynamic-eval]
  (let [r (routes
           (POST (str entity-event-prefix ":component/:event") [] process-request)
           (POST query-prefix [] process-query)
           (POST dynamic-eval-prefix [] process-dynamic-eval)
           (GET "/meta/:component" [] process-meta-request)
           (not-found "<p>Resource not found</p>"))]
    (cors/wrap-cors
     r
     :access-control-allow-origin [#".*"]
     :access-control-allow-headers ["Content-Type"]
     :access-control-allow-credentials true
     :access-control-allow-methods [:post])))

(defn run-server
  ([[evaluator query-fn] config]
   (h/run-server
    (make-routes
     (partial process-request evaluator)
     (partial process-query evaluator query-fn)
     (partial process-dynamic-eval evaluator nil))
    (if (:thread config)
      config
      (assoc config :thread (+ 1 (u/n-cpu))))))
  ([eval-context]
   (run-server eval-context {:port 8080})))
