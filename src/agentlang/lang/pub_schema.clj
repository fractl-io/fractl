(ns agentlang.lang.pub-schema
  (:require [clojure.walk :as w]
            [agentlang.util :as u]
            [agentlang.global-state :as gs]
            [agentlang.inference.embeddings :as e]))

(defn publish-schema? []
  #_(:publish-schema (gs/get-app-config))
  ;; NOTE: while running the app as a standalone jar
  ;; the model is loaded before the config. So another
  ;; option, like a system.property, has to be used.
  ;; As we are using a local queue, publishing the schema
  ;; itself will not throw an error, so setting this to
  ;; always return `true`.
  true)

(defn- preproc-definition [d]
  (let [d (w/prewalk #(if (fn? %) :fn %) d)]
    (assoc d :app-uuid (u/get-app-uuid))))

(defn publish-event [definition]
  (let [definition (preproc-definition definition)]
    (e/embed-schema definition)))