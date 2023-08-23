(ns fractl.http
  (:require [amazonica.aws.s3 :as s3]
            [buddy.auth :as buddy]
            [buddy.auth.backends :as buddy-back]
            [buddy.auth.middleware :as buddy-midd
             :refer [wrap-authentication]]
            [clojure.string :as s]
            [clojure.walk :as w]
            [fractl.auth.core :as auth]
            [fractl.component :as cn]
            [fractl.compiler :as compiler]
            [fractl.lang :as ln]
            [fractl.lang.internal :as li]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.util.http :as uh]
            [fractl.util.logger :as log]
            [fractl.gpt.core :as gpt]
            [org.httpkit.server :as h]
            [ring.util.codec :as codec]
            [ring.middleware.cors :as cors])
  (:use [compojure.core :only [routes POST PUT DELETE GET]]
        [compojure.route :only [not-found]]))

(defn- response
  "Create a Ring response from a map object and an HTTP status code.
   The map object will be encoded as JSON in the response.
   Also see: https://github.com/ring-clojure/ring/wiki/Creating-responses"
  [json-obj status data-fmt]
  {:status status
   :headers {"Content-Type" (uh/content-type data-fmt)
             "Access-Control-Allow-Origin" "*"
             "Access-Control-Allow-Methods" "GET,POST,PUT,DELETE"
             "Access-Control-Allow-Headers" "X-Requested-With,Content-Type,Cache-Control,Origin,Accept,Authorization"}
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

(defn- cleanup-inst [obj]
  (cond
    (cn/an-instance? obj) (dissoc obj li/meta-attr)
    (vector? obj) (mapv cleanup-inst obj)
    :else obj))

(defn- cleanup-result [rs]
  (if-let [result (:result rs)]
    (assoc rs :result (if (map? result)
                        (cleanup-inst result)
                        (mapv cleanup-inst result)))
    rs))

(defn- cleanup-results [rs]
  (if (map? rs)
    (cleanup-result rs)
    (mapv cleanup-result rs)))

(defn- ok
  ([obj data-fmt]
   (response (cleanup-results obj) 200 data-fmt))
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

(defn- assoc-event-context [request auth-config event-instance]
  (if auth-config
    (let [user (auth/session-user (assoc auth-config :request request))
          event-instance (if (cn/an-instance? event-instance)
                           event-instance
                           (cn/make-instance event-instance))]
      (cn/assoc-event-context-values
       {:User (:email user)
        :Sub (:sub user)
        :UserDetails user}
       event-instance))
    event-instance))

(defn- event-from-request [request event-name data-fmt auth-config]
  (try
    (let [body (:body request)
          obj (if (map? body)
                body
                ((uh/decoder data-fmt)
                 (String.
                  (.bytes body)
                  java.nio.charset.StandardCharsets/UTF_8)))
          obj-name (li/split-path
                    (or (cn/instance-type obj)
                        (u/string-as-keyword
                         (first (keys obj)))))
          event-instance (if (cn/an-instance? obj)
                           obj
                           (cn/make-event-instance obj-name (first (vals obj))))]
      (if (or (not event-name) (= obj-name event-name))
        [(assoc-event-context request auth-config event-instance) nil]
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
         (if (cn/an-internal-event? event-name)
           (bad-request (str "cannot invoke internal event - " event-name) data-fmt)
           (let [[obj err] (event-from-request request event-name data-fmt auth-config)]
             (if err
               (bad-request err data-fmt)
               (ok (evaluate evaluator obj data-fmt) data-fmt))))
         (bad-request
          (str "unsupported content-type in request - "
               (request-content-type request))))))
  ([evaluator auth-info request]
   (process-dynamic-eval evaluator auth-info nil request)))

(defn process-request [evaluator auth request]
  (let [params (:params request)
        component (keyword (:component params))
        event (keyword (:event params))
        n [component event]]
    (if (cn/find-event-schema n)
      (process-dynamic-eval evaluator auth n request)
      (bad-request (str "Event not found - " n)))))

(defn- paths-info [component]
  (mapv (fn [n] {(subs (str n) 1)
                 {"post" {"parameters" (cn/event-schema n)}}})
        (cn/event-names component)))

(defn- schemas-info [component]
  (mapv (fn [n] {n (cn/entity-schema n)})
        (cn/entity-names component)))

(defn- request-object [request]
  (if-let [data-fmt (find-data-format request)]
    [(when-let [body (:body request)]
       ((uh/decoder data-fmt) (String. (.bytes body)))) data-fmt nil]
    [nil nil (bad-request (str "unsupported content-type in request - " (request-content-type request)))]))

(defn- process-meta-request [[_ maybe-unauth] request]
  (or (maybe-unauth request)
      (let [c (keyword (get-in request [:params :component]))]
        (ok {:paths (paths-info c) :schemas (schemas-info c)}))))

(defn- process-gpt-chat [[_ maybe-unauth] request]
  (or (maybe-unauth request)
      (let [[obj _ err-response] (request-object request)]
        (or err-response
            (let [resp (atom nil)]
              (gpt/non-interactive-generate
               (fn [choice f]
                 (reset!
                  resp
                  {:choice choice
                   :chat-history (f "<insert-next-request-here>")}))
               obj)
              (ok @resp))))))

(defn- parse-rest-uri [request]
  (let [s (:* (:params request))
        [uri suffix] (if (s/ends-with? s "/__tree")
                       [(subs s 0 (s/index-of s "/__all")) :tree]
                       [s nil])]
    (assoc (uh/parse-rest-uri uri) :suffix suffix)))

(defn- maybe-path-attribute [path]
  (when path
    {li/path-attr path}))

(defn- process-generic-request [handler evaluator [auth-config maybe-unauth] request]
  (or (maybe-unauth request)
      (if-let [parsed-path (parse-rest-uri request)]
        (let [query-params (when-let [s (:query-string request)]
                             (w/keywordize-keys (codec/form-decode s)))
              [obj data-fmt err-response] (request-object request)
              parsed-path (assoc parsed-path :query-params query-params :data-fmt data-fmt)]
          (or err-response (let [[event-gen resp] (handler parsed-path obj)]
                             (if resp
                               resp
                               (let [[evt post-fn] (if (fn? event-gen) (event-gen) [event-gen nil])
                                     evt (assoc-event-context request auth-config evt)
                                     result (try
                                              (ok (evaluate evaluator evt data-fmt) data-fmt)
                                              (finally
                                                (when post-fn (post-fn))))]
                                 result)))))
        (bad-request (str "invalid request uri - " (:* (:params request)))))))

(defn- temp-event-name [component]
  (li/make-path component (li/unq-name)))

(defn- multi-post-request? [obj]
  (>= (count (keys obj)) 2))

(defn- maybe-generate-multi-post-event [obj component]
  (when (multi-post-request? obj)
    (let [event-name (temp-event-name component)]
      (and (apply ln/dataflow event-name (compiler/parse-relationship-tree obj))
           event-name))))

(def process-post-request
  (partial
   process-generic-request
   (fn [{entity-name :entity component :component path :path} obj]
     (if (cn/event? entity-name)
       [obj nil]
       (if-let [evt (maybe-generate-multi-post-event obj component)]
         [(fn [] [{evt {}} #(cn/remove-event evt)]) nil]
         [{(cn/crud-event-name component entity-name :Create)
           (merge
            {:Instance obj}
            (when path
              {li/path-attr (li/as-partial-path path)}))}
          nil])))))

(def process-put-request
  (partial
   process-generic-request
   (fn [{entity-name :entity id :id component :component path :path} obj]
     (if-not (or id path)
       [nil (bad-request (str "id or path required to update " entity-name))]
       [{(cn/crud-event-name component entity-name :Update)
         (merge
          (when-not path
            (let [id-attr (cn/identity-attribute-name entity-name)]
              {id-attr (cn/parse-attribute-value entity-name id-attr id)}))
          {:Data (li/record-attributes obj)}
          (maybe-path-attribute path))}
        nil]))))

(defn- generate-filter-query-event [component entity-name query-params]
  (let [event-name (temp-event-name component)]
    (and (apply ln/dataflow
                event-name [{(li/name-as-query-pattern entity-name)
                             {:where
                              `[:and ~@(mapv
                                        (fn [[k v]]
                                          [(if (= k li/path-attr) :like :=)
                                           k (cn/parse-attribute-value entity-name k v)])
                                        query-params)]}}])
         event-name)))

(defn- make-lookup-event [component entity-name id path]
  {(cn/crud-event-name component entity-name :Lookup)
   (merge
    (when-not path
      (let [id-attr (cn/identity-attribute-name entity-name)]
        {id-attr (cn/parse-attribute-value entity-name id-attr id)}))
    (maybe-path-attribute path))})

(defn- make-lookupall-event [component entity-name path]
  {(cn/crud-event-name component entity-name :LookupAll)
   (or (when path (maybe-path-attribute path)) {})})

(defn- merge-child-uris [evaluator evt-context component entity-name
                         parent-insts children data-fmt]
  (mapv (fn [r]
          (reduce
           (fn [parent-inst [relname _ child-entity]]
             (let [[c n] (li/split-path child-entity)
                   path (cn/full-path-from-references parent-inst relname child-entity)
                   evt (evt-context (make-lookupall-event c child-entity path))
                   result (evaluate evaluator evt data-fmt)
                   rels (li/rel-tag parent-inst)]
               (assoc parent-inst li/rel-tag (assoc rels relname result))))
           r children))
        parent-insts))

(defn- get-tree [evaluator [auth-config maybe-unauth] request
                 component entity-name id path data-fmt]
  (if-not id
    (bad-request (str "identity of " entity-name " required for tree lookup"))
    (or (maybe-unauth request)
        (let [evt-context (partial assoc-event-context request auth-config)
              evt (evt-context (make-lookup-event component entity-name id path))
              result (evaluate evaluator evt data-fmt)]
          (if (seq result)
            (if-let [children (seq (cn/contained-children entity-name))]
              (ok (merge-child-uris
                   evaluator evt-context component entity-name
                   result children data-fmt) data-fmt)
              (ok result data-fmt))
            (ok result data-fmt))))))

(defn process-get-request [evaluator auth-info request]
  (process-generic-request
   (fn [{entity-name :entity id :id component :component path
         :path suffix :suffix query-params :query-params data-fmt :data-fmt
         :as p} obj]
     (cond
       query-params
       [(fn []
          (let [evt (generate-filter-query-event
                     component entity-name
                     (merge query-params (when path (maybe-path-attribute (str path "%")))))]
            [{evt {}} #(cn/remove-event evt)]))
        nil]

       (= suffix :tree)
       [nil (get-tree evaluator auth-info request component
                      entity-name id path data-fmt)]

       :else
       [(if id
          (make-lookup-event component entity-name id path)
          (make-lookupall-event component entity-name (when path (str path "%"))))
        nil]))
   evaluator auth-info request))

(def process-delete-request
  (partial
   process-generic-request
   (fn [{entity-name :entity id :id component :component path :path} _]
     (if-not (or id path)
       [nil (bad-request (str "id or path required to delete " entity-name))]
       [{(cn/crud-event-name component entity-name :Delete)
         (merge
          (when-not path
            (let [id-attr (cn/identity-attribute-name entity-name)]
              {id-attr (cn/parse-attribute-value entity-name id-attr id)}))
          (maybe-path-attribute path))}
        nil]))))

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
  (let [request-obj (su/keys-as-keywords request-obj)]
    (if-let [q (preprocess-query (:Query request-obj))]
      (let [result (query-fn (li/split-path (:from q)) q)]
        (ok (first result) data-fmt))
      (bad-request (str "not a valid query request - " request-obj)))))

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

(def ^:private post-signup-event-name :Fractl.Kernel.Identity/PostSignUp)

(defn- eval-ok-result [eval-result]
  (if (vector? eval-result)
    (eval-ok-result (first eval-result))
    (when (and (map? eval-result) (= :ok (:status eval-result)))
      (:result eval-result))))

(defn- eval-result [eval-res]
  (if (vector? eval-res)
    (eval-result (first eval-res))
    eval-res))

;; TODO: Add layer of domain filtering on top of cognito.
;; Additionally: Is this a right place for this?
#_(defn- whitelisted-email? [email]
  (let [{:keys [access-key secret-key region whitelist?] :as _aws-config} (uh/get-aws-config)]
    (if (true? whitelist?)
      (let [[s3-bucket whitelist-file-key] (uh/get-aws-config)
            whitelisted-emails (read-string
                                 (s3/get-object-as-string
                                   {:access-key access-key
                                    :secret-key secret-key
                                    :endpoint region}
                                   s3-bucket whitelist-file-key))]
        (contains? whitelisted-emails email))
      nil)))

;; TODO: Add layer of domain filtering on top of cognito.
#_(defn- whitelisted-domain? [email domains]
  (let [domain (last (s/split email #"@"))]
    (contains? (set domains) domain)))

(defn- whitelisted? [email {:keys [whitelist? email-domains] :as _auth-info}]
  true
  ;; TODO: Add layer of domain filtering on top of cognito.
  #_(cond
    (and (not (nil? email-domains)) (true? whitelist?))
    (or (whitelisted-email? email)
        (whitelisted-domain? email email-domains))

    (not (nil? email-domains))
    (whitelisted-domain? email email-domains)

    (true? whitelist?)
    (whitelisted-email? email)

    :else
    true))

(defn- process-signup [evaluator call-post-signup [auth-config _] request]
  (if-not auth-config
    (internal-error "cannot process sign-up - authentication not enabled")
    (if-let [data-fmt (find-data-format request)]
      (let [[evobj err] (event-from-request request nil data-fmt nil)]
        (cond
          err
          (do (log/warn (str "bad sign-up request - " err))
              (bad-request err data-fmt))

          (not (cn/instance-of? :Fractl.Kernel.Identity/SignUp evobj))
          (bad-request (str "not a signup event - " evobj) data-fmt)

          :else
          (if-not (whitelisted? (:Email (:User evobj)) auth-config)
            (unauthorized "Your email is not whitelisted yet." data-fmt)
            (try
              (let [result (evaluate evaluator evobj data-fmt)
                    r (eval-ok-result result)]
                (when (not r) (throw (Exception. (:message (eval-result result)))))
                (let [user (if (map? r) r (first r))
                      post-signup-result
                      (when call-post-signup
                        (evaluate
                         evaluator
                         (assoc
                          (create-event post-signup-event-name)
                          :SignupResult result :SignupRequest evobj)
                         data-fmt))]
                  (if user
                    (ok (or post-signup-result {:status :ok :result (dissoc user :Password)}) data-fmt)
                    (bad-request (or post-signup-result result) data-fmt))))
              (catch Exception ex
                (log/warn ex)
                (unauthorized (str "Sign up failed. " (.getMessage ex))
                              data-fmt))))))
      (bad-request
       (str "unsupported content-type in request - "
            (request-content-type request))))))

(defn- process-login [evaluator [auth-config _ :as _auth-info] request]
  (if-not auth-config
    (internal-error "cannot process login - authentication not enabled")
    (if-let [data-fmt (find-data-format request)]
      (let [[evobj err] (event-from-request request nil data-fmt nil)]
        (if err
          (do (log/warn (str "bad login request - " err))
              (bad-request err data-fmt))
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
                                 (.getMessage ex)) data-fmt)))))
      (bad-request
       (str "unsupported content-type in request - "
            (request-content-type request))))))

(defn- process-confirm-sign-up [auth-config request]
  (if-not auth-config
    (internal-error "cannot process process-confirm-sign-up - authentication not enabled")
    (if-let [data-fmt (find-data-format request)]
      (let [[evobj err] (event-from-request request nil data-fmt nil)]
        (cond
          err
          (do (log/warn (str "bad confirm-sign-up request - " err))
              (bad-request err data-fmt))

          (not (cn/instance-of? :Fractl.Kernel.Identity/ConfirmSignUp evobj))
          (bad-request (str "not a confirm-sign-up event - " evobj) data-fmt)

          :else
          (try
            (let [result (auth/confirm-sign-up
                          (assoc
                           auth-config
                           :event evobj))]
              (ok {:result result} data-fmt))
            (catch Exception ex
              (log/warn ex)
              (unauthorized (str "Verify user failed. "
                                 (.getMessage ex)) data-fmt)))))
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

          (not (cn/instance-of? :Fractl.Kernel.Identity/ForgotPassword evobj))
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

          (not (cn/instance-of? :Fractl.Kernel.Identity/ConfirmForgotPassword evobj))
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

          (not (cn/instance-of? :Fractl.Kernel.Identity/ChangePassword evobj))
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
          (unauthorized (str "logout failed. " (ex-message ex)) data-fmt)))
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
          (unauthorized (str "get-user failed" (ex-message ex)) data-fmt)))
      (unauthorized "get-user failed" data-fmt))
    (bad-request
     (str "unsupported content-type in request - "
          (request-content-type request)))))

(defn- process-update-user [auth-config request]
  (if-not auth-config
    (internal-error "cannot process update-user - authentication not enabled")
    (if-let [data-fmt (find-data-format request)]
      (let [[evobj err] (event-from-request request [:Fractl.Kernel.Identity :UpdateUser] data-fmt nil)]
        (cond
          err
          (do (log/warn (str "bad update-user request - " err))
              (bad-request err data-fmt))

          (not (cn/instance-of? :Fractl.Kernel.Identity/UpdateUser evobj))
          (bad-request (str "not a UpdateUser event - " evobj) data-fmt)

          :else
          (try
            (let [user (auth/session-user
                        (assoc auth-config :request request))
                  result (auth/upsert-user
                          (assoc
                           auth-config
                           :instance evobj
                           :user user))]
              (ok {:result result} data-fmt))
            (catch Exception ex
              (log/warn ex)
              (unauthorized (str "update-user failed. " (ex-message ex)) data-fmt)))))
      (bad-request
       (str "unsupported content-type in request - " (request-content-type request))))))

(defn- process-refresh-token [auth-config request]
  (if-not auth-config
    (internal-error "cannot process refresh-token - authentication not enabled")
    (if-let [data-fmt (find-data-format request)]
      (let [[evobj err] (event-from-request request [:Fractl.Kernel.Identity :RefreshToken] data-fmt nil)]
        (cond
          err
          (do (log/warn (str "bad refresh-token request - " err))
              (bad-request err data-fmt))

          (not (cn/instance-of? :Fractl.Kernel.Identity/RefreshToken evobj))
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
              (unauthorized (str "refresh-token failed. " (ex-message ex)) data-fmt)))))
      (bad-request
       (str "unsupported content-type in request - " (request-content-type request))))))

(defn- process-root-get [_]
  (ok {:result :fractl}))

(defn- make-routes [config auth-config handlers]
  (let [r (routes
           (POST uh/login-prefix [] (:login handlers))
           (POST uh/logout-prefix [] (:logout handlers))
           (POST uh/signup-prefix [] (:signup handlers))
           (POST uh/confirm-sign-up-prefix [] (:confirm-sign-up handlers))
           (POST uh/get-user-prefix [] (:get-user handlers))
           (POST uh/update-user-prefix [] (:update-user handlers))
           (POST uh/forgot-password-prefix [] (:forgot-password handlers))
           (POST uh/confirm-forgot-password-prefix [] (:confirm-forgot-password handlers))
           (POST uh/change-password-prefix [] (:change-password handlers))
           (POST uh/refresh-token-prefix [] (:refresh-token handlers))
           (PUT (str uh/entity-event-prefix "*") [] (:put-request handlers))
           (POST (str uh/entity-event-prefix "*") [] (:post-request handlers))
           (GET (str uh/entity-event-prefix "*") [] (:get-request handlers))
           (DELETE (str uh/entity-event-prefix "*") [] (:delete-request handlers))
           (POST uh/query-prefix [] (:query handlers))
           (POST uh/dynamic-eval-prefix [] (:eval handlers))
           (POST uh/gpt-prefix [] (:gpt handlers))
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
     :access-control-allow-methods [:post :put :delete :get])))

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
          :confirm-sign-up (partial process-confirm-sign-up auth)
          :get-user (partial process-get-user auth)
          :update-user (partial process-update-user auth)
          :forgot-password (partial process-forgot-password auth)
          :confirm-forgot-password (partial process-confirm-forgot-password auth)
          :change-password (partial process-change-password auth)
          :refresh-token (partial process-refresh-token auth)
          :put-request (partial process-put-request evaluator auth-info)
          :post-request (partial process-post-request evaluator auth-info)
          :get-request (partial process-get-request evaluator auth-info)
          :delete-request (partial process-delete-request evaluator auth-info)
          :query (partial process-query evaluator auth-info query-fn)
          :eval (partial process-dynamic-eval evaluator auth-info nil)
          :gpt (partial process-gpt-chat auth-info)
          :meta (partial process-meta-request auth-info)})
        (if (:thread config)
          config
          (assoc config :thread (+ 1 (u/n-cpu)))))
       (u/throw-ex (str "authentication service not supported - " (:service auth))))))
  ([eval-context]
   (run-server eval-context {:port 8080})))
