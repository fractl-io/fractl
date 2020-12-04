(ns fractl.util.http
  (:require #?(:clj [org.httpkit.client :as http]
               :cljs [cljs-http.client :as http])
            #?(:clj [cheshire.core :as json])
            #?(:cljs [cljs.core.async :refer [<!]]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

(def entity-event-prefix "/_e/")
(def query-prefix "/_q/")
(def dynamic-eval-prefix "/_dynamic/")

(defn do-post [url options request-obj]
  (let [body (#?(:clj json/generate-string :cljs identity) request-obj)]
    #?(:clj @(http/post url (assoc options :body body))
       :cljs (go (let [response (<! (http/post url {:json-params body}))]
                   ((:cljs-response-handler options) response))))))