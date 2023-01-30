(ns fractl.http
  (:require [amazonica.aws.s3 :as s3]
            [clojure.walk :as w]
            [clojure.string :as s]
            [org.httpkit.server :as h]
            [ring.middleware.cors :as cors]
            [ring.util.codec :as codec]
            [buddy.auth :as buddy]
            [buddy.auth.backends :as buddy-back]
            [buddy.auth.middleware :as buddy-midd
             :refer [wrap-authentication]]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.util.http :as uh]
            [fractl.auth.core :as auth]
            [fractl.component :as cn]
            [fractl.lang.internal :as li])
  (:use [compojure.core :only [routes POST GET]]
        [compojure.route :only [not-found]]))

(defn- response
  "Create a Ring response from a map object and an HTTP status code.
   The map object will be encoded as JSON in the response.
   Also see: https://github.com/ring-clojure/ring/wiki/Creating-responses"
  [json-obj status data-fmt]
  {:status status
   :headers {"Content-Type" (uh/content-type data-fmt)}
   :body ((uh/encoder data-fmt) json-obj)})

(defn- unauthorized
  ([msg data-fmt]
   (response {:reason msg} 401 data-fmt))
  ([data-fmt]
   (unauthorized "not authorized to access this resource" data-fmt)))

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

(defn- create-event [event-name]
  {cn/type-tag-key :event
   cn/instance-type (keyword event-name)})

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
      result)
    (catch Exception ex
      (log/exception ex)
      (internal-error (.getMessage ex) data-fmt))))

(defn- event-from-request [request event-name data-fmt auth-config]
  (try
    (let [user (when auth-config
                 (auth/session-user
                  (assoc auth-config :request request)))
          body (:body request)
          obj (if (map? body)
                body
                ((uh/decoder data-fmt)
                 (String.
                  (.bytes body)
                  java.nio.charset.StandardCharsets/UTF_8)))
          obj-name (li/split-path
                    (or (cn/instance-type obj)
                        (u/string-as-keyword
                         (first (keys obj)))))]
      (if (or (not event-name) (= obj-name event-name))
        [(cn/assoc-event-context-values
          {:User (:email user)
           :Sub (:sub user)
           :UserDetails user}
          (if (cn/an-instance? obj)
            obj
            (cn/make-event-instance obj-name (first (vals obj)))))
         nil]
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

(defn- process-dynamic-eval
  ([evaluator [auth-config maybe-unauth] event-name request]
   (or (maybe-unauth request)
       (if-let [data-fmt (find-data-format request)]
         (let [[obj err] (event-from-request request event-name data-fmt auth-config)]
           (if err
             (bad-request err data-fmt)
             (ok (evaluate evaluator obj data-fmt) data-fmt)))
         (bad-request
          (str "unsupported content-type in request - "
               (request-content-type request))))))
  ([evaluator auth-info request]
   (process-dynamic-eval evaluator auth-info nil request)))

(defn- paths-info [component]
  (mapv (fn [n] {(subs (str n) 1)
                 {"post" {"parameters" (cn/event-schema n)}}})
        (cn/event-names component)))

(defn- schemas-info [component]
  (mapv (fn [n] {n (cn/entity-schema n)})
        (cn/entity-names component)))

(defn- process-meta-request [[_ maybe-unauth] request]
  (or (maybe-unauth request)
      (let [c (keyword (get-in request [:params :component]))]
        (ok {:paths (paths-info c) :schemas (schemas-info c)}))))

(defn process-request [evaluator auth request]
  (let [params (:params request)
        component (keyword (:component params))
        event (keyword (:event params))
        n [component event]]
    (if (cn/find-event-schema n)
      (process-dynamic-eval evaluator auth n request)
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

(defn- process-query [_ [_ maybe-unauth] query-fn request]
  (or (maybe-unauth request)
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
          (internal-error (str "Failed to process query request - " (.getMessage ex)))))))

(def ^:private post-signup-event-name :Kernel.Identity/PostSignUp)

(defn- eval-ok-result [eval-result]
  (if (vector? eval-result)
    (eval-ok-result (first eval-result))
    (when (and (map? eval-result) (= :ok (:status eval-result)))
      (:result eval-result))))

(defn- process-signup [evaluator call-post-signup [auth-config _] request]
  (if-not auth-config
    (internal-error "cannot process sign-up - authentication not enabled")
    (if-let [data-fmt (find-data-format request)]
      (let [[evobj err] (event-from-request request nil data-fmt nil)]
        (cond
          err
          (do (log/warn (str "bad sign-up request - " err))
              (bad-request err data-fmt))

          (not (cn/instance-of? :Kernel.Identity/SignUp evobj))
          (bad-request (str "not a signup event - " evobj) data-fmt)

          :else
          (try
            (let [result (evaluate evaluator evobj data-fmt)
                  r (eval-ok-result result)
                  user (if (map? r) r (first r))
                  post-signup-result
                  (when call-post-signup
                    (evaluate
                     evaluator
                     (assoc (create-event post-signup-event-name) :SignupResult result)
                     data-fmt))]
              (if user
                (ok (or post-signup-result {:status :ok :result (dissoc user :Password)}) data-fmt)
                (bad-request (or post-signup-result result) data-fmt)))
            (catch Exception ex
              (log/warn ex)
              (unauthorized (str "Sign up failed. " (.getMessage ex))
                            data-fmt)))))
      (bad-request
       (str "unsupported content-type in request - "
            (request-content-type request))))))

(defn- whitelisted? [email {:keys [access-key secret-key region whitelist? s3-bucket whitelist-file-key] :as _auth-info}]
  (if-not whitelist?
    true
    (let [whitelisted-emails (try
                               (read-string
                                (s3/get-object-as-string
                                 {:access-key access-key
                                  :secret-key secret-key
                                  :endpoint region}
                                 s3-bucket whitelist-file-key))
                               (catch Exception ex
                                 (log/warn ex)
                                 #{}))]
      (contains? whitelisted-emails email))))


(defn- process-login [evaluator [auth-config _ :as _auth-info] request]
  (if-not auth-config
    (internal-error "cannot process login - authentication not enabled")
    (if-let [data-fmt (find-data-format request)]
      (let [[evobj err] (event-from-request request nil data-fmt nil)]
        (if err
          (do (log/warn (str "bad login request - " err))
              (bad-request err data-fmt))
          (if-not (whitelisted? (:Username evobj) auth-config)
            (unauthorized "Your email is not whitelisted yet." data-fmt)
            (try
              (let [result (auth/user-login
                            (assoc
                             auth-config
                             :event evobj
                             :eval evaluator))]
                (ok {:result result} data-fmt))
              (catch Exception ex
                (log/warn ex)
                (unauthorized (str "Login failed. "
                                   (.getMessage ex)) data-fmt))))))
      (bad-request
       (str "unsupported content-type in request - "
            (request-content-type request))))))

(defn- process-forgot-password [auth-config request]
  (if-not auth-config
    (internal-error "cannot process forgot-password - authentication not enabled")
    (if-let [data-fmt (find-data-format request)]
      (let [[evobj err] (event-from-request request nil data-fmt nil)]
        (cond
          err
          (do (log/warn (str "bad forgot-request request - " err))
              (bad-request err data-fmt))

          (not (cn/instance-of? :Kernel.Identity/ForgotPassword evobj))
          (bad-request (str "not a forgot-password event - " evobj) data-fmt)

          :else
          (try
            (let [result (auth/forgot-password
                          (assoc
                           auth-config
                           :event evobj))]
              (ok {:result result} data-fmt))
            (catch Exception ex
              (log/warn ex)
              (unauthorized (str "Forgot Password failed. "
                                 (.getMessage ex)) data-fmt)))))
      (bad-request
       (str "unsupported content-type in request - "
            (request-content-type request))))))

(defn- process-confirm-forgot-password [auth-config request]
  (if-not auth-config
    (internal-error "cannot process confirm-forgot-password - authentication not enabled")
    (if-let [data-fmt (find-data-format request)]
      (let [[evobj err] (event-from-request request nil data-fmt nil)]
        (cond
          err
          (do (log/warn (str "bad confirm-forgot-request request - " err))
              (bad-request err data-fmt))

          (not (cn/instance-of? :Kernel.Identity/ConfirmForgotPassword evobj))
          (bad-request (str "not a confirm-forgot-password event - " evobj) data-fmt)

          :else
          (try
            (let [result (auth/confirm-forgot-password
                          (assoc
                           auth-config
                           :event evobj))]
              (ok {:result result} data-fmt))
            (catch Exception ex
              (log/warn ex)
              (unauthorized (str "Confirm Forgot Password failed. "
                                 (.getMessage ex)) data-fmt)))))
      (bad-request
       (str "unsupported content-type in request - "
            (request-content-type request))))))

(defn- process-change-password [auth-config request]
  (if-not auth-config
    (internal-error "cannot process change-password - authentication not enabled")
    (if-let [data-fmt (find-data-format request)]
      (let [[evobj err] (event-from-request request nil data-fmt nil)]
        (cond
          err
          (do (log/warn (str "bad change-password-request request - " err))
              (bad-request err data-fmt))

          (not (cn/instance-of? :Kernel.Identity/ChangePassword evobj))
          (bad-request (str "not a change-password event - " evobj) data-fmt)

          :else
          (try
            (let [result (auth/change-password
                          (assoc
                           auth-config
                           :event evobj))]
              (ok {:result result} data-fmt))
            (catch Exception ex
              (log/warn ex)
              (unauthorized (str "Change Password failed. "
                                 (.getMessage ex)) data-fmt)))))
      (bad-request
       (str "unsupported content-type in request - "
            (request-content-type request))))))

(defn- process-logout [auth-config request]
  (if-let [data-fmt (find-data-format request)]
    (if auth-config
      (try
        (let [sub (auth/session-sub
                   (assoc auth-config :request request))
              result (auth/user-logout
                      (assoc
                       auth-config
                       :sub sub))]
          (ok {:result result} data-fmt))
        (catch Exception ex
          (log/warn ex)
          (unauthorized "logout failed" data-fmt)))
      (ok {:result :bye} data-fmt))
    (bad-request
     (str "unsupported content-type in request - "
          (request-content-type request)))))

(defn- process-get-user [auth-config request]
  (if-let [data-fmt (find-data-format request)]
    (if auth-config
      (try
        (let [user (auth/session-user
                    (assoc auth-config :request request))
              result (auth/get-user
                      (assoc auth-config :user user))]
          (ok {:result result} data-fmt))
        (catch Exception ex
          (log/warn ex)
          (unauthorized "get-user failed" data-fmt)))
      (unauthorized "get-user failed" data-fmt))
    (bad-request
     (str "unsupported content-type in request - "
          (request-content-type request)))))

(defn- process-update-user [auth-config request]
  (if-not auth-config
    (internal-error "cannot process update-user - authentication not enabled")
    (if-let [data-fmt (find-data-format request)]
      (let [[evobj err] (event-from-request request [:Kernel.Identity :UpdateUser] data-fmt nil)]
        (cond
          err
          (do (log/warn (str "bad update-user request - " err))
              (bad-request err data-fmt))

          (not (cn/instance-of? :Kernel.Identity/UpdateUser evobj))
          (bad-request (str "not a UpdateUser event - " evobj) data-fmt)

          :else
          (try
            (let [user (auth/session-user
                        (assoc auth-config :request request))
                  result (auth/upsert-user
                          (assoc
                           auth-config
                           :event evobj
                           :user user))]
              (ok {:result result} data-fmt))
            (catch Exception ex
              (log/warn ex)
              (unauthorized "update-user failed" data-fmt)))))
      (bad-request
       (str "unsupported content-type in request - " (request-content-type request))))))

(defn- process-refresh-token [auth-config request]
  (if-not auth-config
    (internal-error "cannot process refresh-token - authentication not enabled")
    (if-let [data-fmt (find-data-format request)]
      (let [[evobj err] (event-from-request request [:Kernel.Identity :RefreshToken] data-fmt nil)]
        (cond
          err
          (do (log/warn (str "bad refresh-token request - " err))
              (bad-request err data-fmt))

          (not (cn/instance-of? :Kernel.Identity/RefreshToken evobj))
          (bad-request (str "not a RefreshToken event - " evobj) data-fmt)

          :else
          (try
            (let [user (auth/session-user
                        (assoc auth-config :request request))
                  result (auth/refresh-token
                          (assoc
                           auth-config
                           :event evobj
                           :user user))]
              (ok {:result result} data-fmt))
            (catch Exception ex
              (log/warn ex)
              (unauthorized "refresh-token failed" data-fmt)))))
      (bad-request
       (str "unsupported content-type in request - " (request-content-type request))))))

(defn- process-root-get [_]
  (ok {:result :fractl}))

(defn- make-routes [config auth-config handlers]
  (let [r (routes
           (POST uh/login-prefix [] (:login handlers))
           (POST uh/logout-prefix [] (:logout handlers))
           (POST uh/signup-prefix [] (:signup handlers))
           (POST uh/get-user-prefix [] (:get-user handlers))
           (POST uh/update-user-prefix [] (:update-user handlers))
           (POST uh/forgot-password-prefix [] (:forgot-password handlers))
           (POST uh/confirm-forgot-password-prefix [] (:confirm-forgot-password handlers))
           (POST uh/change-password-prefix [] (:change-password handlers))
           (POST uh/refresh-token-prefix [] (:refresh-token handlers))
           (POST (str uh/entity-event-prefix ":component/:event") []
             (:request handlers))
           (POST uh/query-prefix [] (:query handlers))
           (POST uh/dynamic-eval-prefix [] (:eval handlers))
           (GET "/meta/:component" [] (:meta handlers))
           (GET "/" [] process-root-get)
           (not-found "<p>Resource not found</p>"))
        r-with-auth (if auth-config
                      (wrap-authentication
                       r (buddy-back/token
                          {:authfn (auth/make-authfn auth-config)
                           :token-name "Bearer"}))
                      r)]
    (cors/wrap-cors
     r-with-auth
     :access-control-allow-origin (or (:cors-allow-origin config)
                                      [#".*"])
     :access-control-allow-credentials true
     :access-control-allow-methods [:post])))

(defn- handle-request-auth [request]
  (try
    (when-not (buddy/authenticated? request)
      (log/info (str "unauthorized request - " request))
      (unauthorized (find-data-format request)))
    (catch Exception ex
      (log/warn ex)
      (bad-request "invalid auth data" (find-data-format request)))))

(defn- auth-service-supported? [auth]
  (some #{(:service auth)} [:keycloak :cognito :dataflow]))

(defn make-auth-handler [config]
  (let [auth (:authentication config)
        auth-check (if auth handle-request-auth (constantly false))]
    [auth auth-check]))

(defn run-server
  ([[evaluator query-fn] config]
   (let [[auth _ :as auth-info] (make-auth-handler config)]
     (if (or (not auth) (auth-service-supported? auth))
       (h/run-server
        (make-routes
         config auth
         {:login (partial process-login evaluator auth-info)
          :logout (partial process-logout auth)
          :signup (partial
                   process-signup evaluator
                   (:call-post-sign-up-event config) auth-info)
          :get-user (partial process-get-user auth)
          :update-user (partial process-update-user auth)
          :forgot-password (partial process-forgot-password auth)
          :confirm-forgot-password (partial process-confirm-forgot-password auth)
          :change-password (partial process-change-password auth)
          :refresh-token (partial process-refresh-token auth)
          :request (partial process-request evaluator auth-info)
          :query (partial process-query evaluator auth-info query-fn)
          :eval (partial process-dynamic-eval evaluator auth-info nil)
          :meta (partial process-meta-request auth-info)})
        (if (:thread config)
          config
          (assoc config :thread (+ 1 (u/n-cpu)))))
       (u/throw-ex (str "authentication service not supported - " (:service auth))))))
  ([eval-context]
   (run-server eval-context {:port 8080})))
