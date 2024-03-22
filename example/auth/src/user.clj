(ns user
  (:require
   [shadow.cljs.devtools.api :as shadow]
   [ring.middleware.resource :refer [wrap-resource]]))

(defn cljs []
  (shadow/repl :app))
