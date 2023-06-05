(ns fractl.util.http
  (:require #?(:clj  [org.httpkit.client :as http]
               :cljs [cljs-http.client :as http])
            [clojure.string :as s]
            [fractl.util :as u]
            [fractl.util.seq :as us]
            [fractl.datafmt.json :as json]
            [fractl.datafmt.transit :as t]
            [fractl.global-state :as gs]
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
(def refresh-token-prefix "/_refresh-token/")
(def query-prefix "/_q/")
(def dynamic-eval-prefix "/_dynamic/")
(def callback-prefix "/_callback/")

(defn- remote-resolver-error [response]
  (u/throw-ex (str "remote resolver error - " (or (:error response) response))))

(defn- response-handler [format callback response]
  ((or callback identity)
   (if (map? response)
     (if-let [status (:status response)]
       (if (< 199 status 299)
         #?(:clj
            ((decoder format) (:body response))
            :cljs (:body response))
         (remote-resolver-error response))
       (remote-resolver-error response))
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

#?(:clj
   (do
     (defn- get-env-var [var-name]
       (let [var-value (System/getenv var-name)]
         (if (nil? var-value)
           (throw (Exception. (str "Environment variable \"" var-name "\" not found.")))
           var-value)))

     (defn get-aws-config []
       (let [aws-config {:region       (get-env-var "AWS_REGION")
                         :access-key   (get-env-var "AWS_ACCESS_KEY")
                         :secret-key   (get-env-var "AWS_SECRET_KEY")
                         :client-id    (get-env-var "AWS_COGNITO_CLIENT_ID")
                         :user-pool-id (get-env-var "AWS_COGNITO_USER_POOL_ID")
                         :whitelist?   (or (get-in (gs/get-app-config) [:authentication :whitelist?])
                                           false)}]
         ;;TODO: Need to revisit this and add a layer to check for domains
         ;;      that are whitelisted.
         #_(if (true? (:whitelist? aws-config))
             (assoc aws-config
               :s3-bucket (get-env-var "AWS_S3_BUCKET")
               :whitelist-file-key (get-env-var "WHITELIST_FILE_KEY"))
             aws-config)
         aws-config))))

(defn- fully-qualified-name [base-component n]
  (let [[c en] (s/split n #"\$")]
    (if (and c en)
      (keyword (str c "/" en))
      (keyword (str base-component "/" n)))))

(defn- uri-as-path [fqn parts]
  (loop [ss (partition-all 2 parts), path []]
    (if (seq ss)
      (let [[p v] (first ss)]
        (if (seq (rest ss))
          (recur
           (rest ss)
           (conj path {:parent (fqn p)
                       :id v}))
          {:path path :entity p :id v})))))

(defn parse-rest-uri [uri]
  (let [parts (s/split uri #"/")
        c (count parts)]
    (when (>= c 2)
      (let [f (first parts) r (rest parts)
            fqn (partial fully-qualified-name f)]
        (if (<= c 3)
          {:component (keyword f)
           :entity (fqn (first r))
           :id (when (seq (rest r)) (last r))}
          (assoc
           (uri-as-path fqn r)
           :component (keyword f)))))))
