(ns fractl.lang.pub-schema
  (:require [clojure.walk :as w]
            [fractl.util :as u]
            [fractl.global-state :as gs]
            [fractl.inference.embeddings :as e]))

(defn publish-schema? [] (:publish-schema (gs/get-app-config)))

(defn- preproc-definition [d]
  (let [d (w/prewalk #(if (fn? %) :fn %) d)]
    (assoc d :app-uuid (u/get-app-uuid))))

(defn publish-event [definition]
  (let [definition (preproc-definition definition)]
    (e/embed-schema definition)))
