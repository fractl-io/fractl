(ns fractl.lang.tools.deploy
  "Deploy a model to the fractl-platform"
  (:require [fractl.util :as u]
            [fractl.util.http :as uh]))

(defn- login [service-info]
  (if-let [r (uh/POST
              (str (:host service-info) uh/login-prefix)
              nil {:Kernel.Identity/UserLogin
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

(def ^:private deploy-api "/_e/FractlDeployment.Core/DeployToCluster")

(defn- deploy-model [service-info model-name repourl]
  (if-let [r (uh/POST
              (str (:host service-info) deploy-api)
              {:auth-token (:auth-token service-info)}
              {:FractlDeployment.Core/DeployToCluster
               {:Model model-name :SourceRepository repourl}})]
    (assoc service-info :result r)
    (u/throw-ex (str "failed to deploy " model-name))))

(defn deploy
  ([service-info model-name repourl]
   (deploy-model (fetch-token service-info) model-name repourl))
  ([service-info model-name]
   (deploy
    service-info model-name
    (when-let [repo-root (:repo-root service-info)]
      (str repo-root "/" model-name ".git")))))
