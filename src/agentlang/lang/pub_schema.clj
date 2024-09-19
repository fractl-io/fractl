(ns agentlang.lang.pub-schema
  (:require [clojure.walk :as w]
            [agentlang.util :as u]
            [agentlang.global-state :as gs]
            [agentlang.inference.embeddings :as e]))

(defn publish-schema? []
  #_(:publish-schema (gs/get-app-config))
  ;; Publish schema is dsiabled as tools are built directly from app-model.
  false)

(defn- preproc-definition [d]
  (let [d (w/prewalk #(if (fn? %) :fn %) d)]
    (assoc d :app-uuid (u/get-app-uuid))))

(defn publish-event [definition]
  (let [definition (preproc-definition definition)]
    (e/embed-schema definition)))
