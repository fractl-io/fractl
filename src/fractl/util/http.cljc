(ns fractl.util.http
  (:require #?(:clj [org.httpkit.client :as http]
               :cljs [cljs-http.client :as http])
            [clojure.string :as s]
            [fractl.util :as u]
            [fractl.util.seq :as us]
            [fractl.datafmt.json :as json]
            [fractl.datafmt.transit :as t]
            #?(:cljs [cljs.core.async :refer [<!]]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

(def ^:private enc-dec
  {:json [json/decode json/encode]
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
(def login-prefix "/_login/")
(def logout-prefix "/_logout/")
(def signup-prefix "/_signup/")
(def get-user-prefix "/_get-user/")
(def update-user-prefix "/_update-user/")
(def forgot-password-prefix "/_forgot-password/")
(def confirm-forgot-password-prefix "/_confirm-forgot-password/")
(def change-password-prefix "/_change-password/")
(def query-prefix "/_q/")
(def dynamic-eval-prefix "/_dynamic/")
(def callback-prefix "/_callback/")

(defn- response-handler [format callback response]
  ((or callback identity)
   (if (map? response)
     (let [status (:status response)]
       (if (< 199 status 299)
         #?(:clj
            ((decoder format) (:body response))
            :cljs (:body response))
         (u/throw-ex (str "remote resolver error - " response))))
     response)))

(defn- fetch-auth-token [options]
  (if-let [t (:auth-token options)]
    [t (dissoc options :auth-token)]
    [nil options]))

#?(:cljs
   (defn make-http-request [format body token]
     (merge {format body}
            (when token {:with-credentials? false
                         :oauth-token token}))))

(defn do-post
  ([url options request-obj format response-handler]
   (let [[token options] (fetch-auth-token options)
         body ((encoder format) request-obj)]
     #?(:clj
        (let [headers (apply
                       assoc
                       (:headers options)
                       "Content-Type" (content-type format)
                       (when token
                         ["Authorization" (str "Bearer " token)]))
              options (assoc options :headers headers)]
          (response-handler @(http/post url (assoc options :body body))))
        :cljs (go
                (let [k (if (= format :transit+json) :transit-params :json-params)]
                  (response-handler
                   (<! (http/post url (make-http-request k body token)))))))))
  ([url options request-obj]
   (do-post url options request-obj :json identity)))

(defn POST
  ([url options request-obj format]
   (do-post
    url (dissoc options :callback)
    request-obj format (partial response-handler format (:callback options))))
  ([url options request-obj]
   (POST url options request-obj :transit+json)))

(defn normalize-post-options [arg]
  (if (fn? arg) {:callback arg} arg))
