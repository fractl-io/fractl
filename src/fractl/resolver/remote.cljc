(ns fractl.resolver.remote
  (:require [fractl.util.remote :as ur]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry
             #?(:clj :refer :cljs :refer-macros)
             [defmake]]))

(def ^:private resolver-fns
  {:upsert {:handler ur/remote-upsert}
   :delete {:handler ur/remote-delete}
   :get {:handler ur/remote-get}
   :query {:handler ur/remote-query}
   :eval {:handler ur/remote-eval}})

(defn- with-required-options [options]
  (merge {:timeout 1000} options))

(defmake :remote
  (fn [resolver-name config]
    (let [host (:host config)
          options (with-required-options (dissoc config :host))
          handlers (map (fn [[k res]]
                          [k {:handler (partial (:handler res) host options)}])
                        resolver-fns)]
      (r/make-resolver resolver-name (into {} handlers)))))
