(ns fractl.util.auth
  (:require [fractl.store :as s]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.lang.datetime :as dt]
            [cheshire.core :as json]
            [babashka.curl :as curl]))

(defn- save-oauth-tokens [store response auth-req]
  ;; save received oauth tokens to the disk
  (let [access-token (get response "access_token")
        id-token (get response "id_token")
        token-type (get response "token_type")
        expires-in (get response "expires_in")
        owner (cn/id-attr auth-req)
        auth-response {:Kernel/AuthResponse
                       {:AccessToken access-token
                        :IdToken id-token
                        :TokenType token-type
                        :ExpirySeconds expires-in
                        :Issued (dt/as-string (dt/now-raw))
                        :Owner owner}}]
    (s/upsert-instance store ":Kernel/AuthResponse"
                       (cn/make-instance auth-response))))

(defn complete-oauth-flow
  "Complete oauth authorization flow by
  exchanging the auth code for oauth tokens"
  [auth-id auth-code]
  #?(:clj
     (let [default-store (s/get-default-store)
           entity-name ":Kernel/OAuthAnyRequest"
           auth-req (s/lookup-by-id default-store entity-name auth-id)]
       (if (some? auth-req)
         (let [oauth-token-url (str "https://" (:AuthDomain auth-req) "/oauth/token")
               client-id (:ClientID auth-req)
               client-secret (:ClientSecret auth-req)
               callback-url (:CallbackURL auth-req)
               params {"grant_type" "authorization_code"
                       "client_id" client-id
                       "client_secret" client-secret
                       "code" auth-code
                       "redirect_uri" callback-url}
               response (curl/post oauth-token-url
                                   {:form-params params})]
           (if (= (:status response) 200)
             (save-oauth-tokens default-store
                                (json/parse-string (:body response))
                                auth-req)))))))


                           
     
