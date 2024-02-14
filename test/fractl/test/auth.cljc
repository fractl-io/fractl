(ns fractl.test.auth
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.util :as u]
            [fractl.auth.oauth2 :as auth]))

;; This test should not be enabled on CI.
;; To run locally, set the FRACTL_AUTH_TEST_CLIENT_ID and
;; FRACTL_AUTH_TEST_CLIENT_SECRET environment variables.
(deftest basic
  #?(:clj
     (let [client-id (u/getenv "FRACTL_AUTH_TEST_CLIENT_ID" "")
           client-secret (u/getenv "FRACTL_AUTH_TEST_CLIENT_SECRET" "")]
       (when (and (seq client-id) (seq client-secret))
         (let [api-obj (auth/initialize
                        auth/git-hub
                        {:client-id client-id
                         :client-secret client-secret
                         :callback "http://localhost:8000/fractl-test/callback"})]
           (is (auth/oauth2? api-obj))
           (println (str "please go to " (auth/authorization-url api-obj) " to authorize the client"))
           (print "once authorized, please enter the code here: ")
           (flush)
           (let [code (read-line)]
             (print "enter the secret: ")
             (flush)
             (let [secret (read-line)
                   token (auth/get-access-token api-obj code secret)]
               (println "trying to list issues using the access-token " (:token token))
               (let [response (auth/http-get api-obj token "https://api.github.com/issues")
                     status (:status response)]
                 (is (or (= status 200) (= status 404))))))
           (is (auth/release api-obj)))))))
