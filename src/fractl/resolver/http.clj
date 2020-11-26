(ns fractl.resolver.http
  (:require [org.httpkit.server :as h])
  (:use [compojure.core :only [defroutes POST]]
        [compojure.route :only [not-found]]))

(defroutes routes
  (POST "/r" resolve)
  (not-found "<p>Resource not found.</p>")) ;; return 404

(defn run-server
  ([config]
   (h/run-server routes config))
  ([] (run-server {:port 8080})))
