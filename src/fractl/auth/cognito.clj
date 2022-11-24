(ns fractl.auth.cognito
  (:require [amazonica.aws.cognitoidp :as aws]
            [amazonica.core :refer [ex->map]]
            [fractl.auth.core :as auth]
            [fractl.evaluator :as e]))

(def ^:private tag :cognito)

(defmethod auth/make-client tag [{:keys [access-key-env-var secret-key-env-var region] :as _config}]
  {:access-key (System/getenv access-key-env-var)
   :secret-key (System/getenv secret-key-env-var)
   :endpoint region})

(defmethod auth/make-authfn tag [_]
  (fn [_req token]
    token))

(defmethod auth/user-login tag [{:keys [client-id-env-var event] :as req}]
  (try
    (aws/initiate-auth (auth/make-client req)
                       :auth-flow "USER_PASSWORD_AUTH"
                       :client-id (System/getenv client-id-env-var)
                       :auth-parameters {"USERNAME" (:Username event)
                                         "PASSWORD" (:Password event)})
    (catch Exception e
      (let [exception-details (ex->map e)]
        ;; how to change top level status. it's still 200. this goes inside `:body`
        {:status (:status-code exception-details)
         :body exception-details}))))

(defn- user-exists? [username user-pool-id config]
  (try
    (aws/admin-get-user (auth/make-client config)
                        :username username
                        :user-pool-id user-pool-id)
    true
    (catch Exception e
      (let [exception-details (ex->map e)
            {:keys [error-code status-code]} exception-details]
        (if (and (= status-code 400) (= error-code "UserNotFoundException"))
          false
          (throw e))))))

(defmethod auth/upsert-user tag [{:keys [client-id-env-var user-pool-id-env-var event] :as req}]
  (let [username (:Username event)
        password (:Password event)]
    (if (user-exists? username (System/getenv user-pool-id-env-var) req)
      nil ;;TODO: update-user
      nil))) ;; TODO: create user

(defmethod auth/session-user tag [req]
  nil)

(defmethod auth/session-sub tag [req]
  nil)

(defmethod auth/user-logout tag [])

(defmethod auth/delete-user tag [req]
  nil)
