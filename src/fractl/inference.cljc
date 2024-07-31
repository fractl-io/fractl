(ns fractl.inference
  (:require [clojure.edn :as edn]
            [fractl.lang :as ln]
            [fractl.lang.internal :as li]
            [fractl.env :as env]
            [fractl.evaluator :as ev]
            [fractl.component :as cn]
            [fractl.util :as u]
            #?(:clj [fractl.util.logger :as log]
               :cljs [fractl.util.jslogger :as log])
            [fractl.util.http :as uh]
            [fractl.inference.service.agent-registry :as ar]
            [fractl.inference.service.core :as inference]))

(defn as-vec [x]
  (if (vector? x)
    x
    [x]))

(defn- can-eval? [r]
  (not (string? r)))

(defn- eval-patterns [fractl-patterns context-event]
  (if (can-eval? fractl-patterns)
    (let [env (env/bind-instance env/EMPTY context-event)]
      (loop [pats (as-vec fractl-patterns), env env, result nil]
        (if-let [p (first pats)]
          (let [r (ev/evaluate-pattern env p)]
            (if (u/safe-ok-result r)
              (recur (rest pats) (:env r) r)
              (do (log/error (str "inferred-pattern " p " failed with result " r))
                  r)))
          (do (log/info (str "inference succeeded with result " result))
              result))))
    fractl-patterns))

(defn- cleanup-agent [inst]
  (dissoc inst :Context))

(defn handle-generic-agent [instance]
  (if-let [handler (ar/fetch-agent-handler (:Type instance))]
    (cleanup-agent (assoc instance :Response (handler instance)))
    (u/throw-ex (str "No handler for agent type " (:Type instance)))))

(defn run-inference-for-event
  ([event agent-instance]
   (log/info (str "Processing response for inference " (cn/instance-type event)
                  " - " (u/pretty-str agent-instance)))
   (let [agent-instance (handle-generic-agent agent-instance)
         r0 (or (:Response agent-instance) agent-instance)
         r1 (if (string? r0) (edn/read-string r0) r0)
         result (if-let [f (:ResponseHandler agent-instance)] (f r1) r1)
         is-review-mode (get-in event [:EventContext :evaluate-inferred-patterns])]
     (if-let [patterns (:patterns result)]
       (if is-review-mode
         patterns
         (eval-patterns patterns event))
       (if-let [errmsg (:errormsg result)]
         (u/throw-ex errmsg)
         result))))
  ([agent-instance]
   (run-inference-for-event (:Context agent-instance) agent-instance)))
