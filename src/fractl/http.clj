(ns fractl.http
  (:require [org.httpkit.server :as h])
  (:use [compojure.core :only [defroutes POST]]
        [compojure.route :only [not-found]]))

(defn- evaluate [request]
  ;; TODO: parse request, evaluate event instance and return dataflow results.
  ;; All request/response encoding has to be JSON.
  )

;; TODO: instead of listening on "/_e", listen on "/:component/:event-name"
(defroutes routes
  (POST "/_e" evaluate)
  (not-found "<p>Resource not found.</p>")) ;; return 404

(defn run-server
  ([config]
   (h/run-server routes config))
  ([]
   (run-server {:port 8080})))
