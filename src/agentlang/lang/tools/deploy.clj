(ns agentlang.lang.tools.deploy
  "Deploy a model to the agentlang-platform"
  (:require [agentlang.util :as u]
            [agentlang.util.http :as uh]))

(defn- login [service-info]
  (if-let [r (uh/POST
              (str (:host service-info) uh/login-prefix)
              nil {:Agentlang.Kernel.Identity/UserLogin
                   {:Username (:user service-info)
                    :Password (:password service-info)}})]
    (if-let [token (get-in r [:result :authentication-result :access-token])]
      (assoc service-info :auth-token token)
      (u/throw-ex (str "login failed - " r)))
    (u/throw-ex (str "unable to login user - " (:user service-info)))))

(defn- fetch-token [service-info]
  (if (:auth-token service-info)
    service-info
    (login service-info)))

(def ^:private deploy-api "/api/AgentlangDeployment.Core/DeployToCluster")

(defn- deploy-model [service-info model-name]
  (if-let [r (uh/POST
              (str (:host service-info) deploy-api)
              {:auth-token (:auth-token service-info)
               ;; TODO: remove these long timeouts once
               ;; https://github.com/fractl-io/fractl-deployment-service/issues/11
               ;; is done.
               :keepalive 3600000
               :timeout 3600000} ; 1 hour in ms
              {:AgentlangDeployment.Core/DeployToCluster
               {:Model model-name}})]
    (assoc service-info :result r)
    (u/throw-ex (str "failed to deploy " model-name))))

(defn deploy [service-info model-name]
  (deploy-model (fetch-token service-info) model-name))
