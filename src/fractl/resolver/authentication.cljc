(ns fractl.resolver.authentication
  (:require [fractl.util :as u]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry
             #?(:clj :refer :cljs :refer-macros)
             [defmake]]
            [fractl.auth.core :as auth]))

(defmake :authentication
  (fn [resolver-name config]
    (if-let [client (auth/make-client config)]
      (r/make-resolver
       resolver-name
       {:upsert {:handler (partial auth/call-upsert-user client config)}
        :delete {:handler (partial auth/call-delete-user client config)}})
      (u/throw-ex (str "failed to create auth-client for " resolver-name)))))
