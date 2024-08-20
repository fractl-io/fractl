(ns fractl.resolver.rest
  (:require #?(:clj [fractl.util.logger :as log]
               :cljs [fractl.util.jslogger :as log])
            [fractl.util.http :as http]
            [fractl.datafmt.json :as json]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry
             #?(:clj :refer :cljs :refer-macros)
             [defmake]]))

(defn- maybe-throw [resp]
  (if (:error resp)
    (throw (ex-info "Rest resolver error" resp))
    resp))

(defn- response-handler [response]
  (log/info (str "rest-resolver response: " (json/decode (:body response))))
  (maybe-throw
   (if (map? response)
     (if-let [status (:status response)]
       (if (< 199 status 299)
         {:status status :response (json/decode (:body response))}
         {:status status :error (or (:error response) (:body response))})
       {:error (or (:error response) (:body response))})
     (do (log/warn (str "rest-resolver: invalid HTTP response - " response))
         {:error "invalid HTTP response"}))))

(defn- maybe-encode-body [body]
  (if (string? body)
    body
    (json/encode body)))

(defn rest-request [method inst]
  (log/info (str "rest-resolver called " method " " (:Url inst) "\n "
                 (:Body inst)))
  (response-handler
   (http/do-request
    method (:Url inst) (:Headers inst)
    (maybe-encode-body (:Body inst)))))

(def ^:private resolver-fns
  {:create {:handler (partial rest-request :post)}
   :update {:handler (partial rest-request :put)}
   :delete {:handler (partial rest-request :delete)}})

(defmake :rest
  (fn [resolver-name config]
    (r/make-resolver resolver-name resolver-fns)))
