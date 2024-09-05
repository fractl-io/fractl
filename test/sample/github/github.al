(component
 :Github
 {:clj-import '[(:require [agentlang.component :as cn]
                          [agentlang.util :as u]
                          [agentlang.global-state :as gs]
                          [agentlang.resolver.core :as r]
                          [agentlang.resolver.registry :as rg]
                          [agentlang.auth.oauth2 :as auth]
                          [agentlang.datafmt.json :as json])]})

(entity :Issue {:Data :Any})

(defn- github-query [api [[_ entity-name] {clause :where} :as param]]
  (when (= entity-name :Issue)
    (let [response (auth/http-get api "https://api.github.com/issues")
          status (:status response)
          body (:body response)]
      (case status
        200 (mapv #(cn/make-instance {:Github/Issue {:Data %}}) (json/decode body))
        404 []
        (u/throw-ex (str "Github API failed with status " status " - " body))))))

(defn- init-api []
  (let [config (:github (gs/get-app-config))
        auth (auth/initialize
              auth/git-hub
              {:client-id (:client-id config)
               :client-secret (:client-secret config)
               :callback (:callback "http://localhost:8000/agentlang-test/callback")})]
    (when-not (auth/oauth2? auth)
      (u/throw-ex "Failed to initialize api"))
    (println (str "please go to " (auth/authorization-url auth) " to authorize the client."))
    (print "once authorized, please enter the code here: ")
    (flush)
    (let [code (read-line)]
      (print "enter the secret: ")
      (flush)
      (let [secret (read-line)
            api (auth/enable-access auth code secret)]
        (when-not (auth/access-enabled? api)
          (u/throw-ex "failed to get access token"))
        api))))

(rg/register-resolver-type
 :github
 (fn [_ _]
   (r/make-resolver
    :github
    {:query (partial github-query (init-api))})))

(rg/register-resolver
 {:name :github :type :github
  :paths [:Github/Issue]})
