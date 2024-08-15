(ns agentlang.inference
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [agentlang.lang :as ln]
            [agentlang.lang.internal :as li]
            [agentlang.env :as env]
            [agentlang.evaluator :as ev]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.util.http :as uh]
            [agentlang.inference.service.agent-registry :as ar]
            [agentlang.inference.service.core :as inference]))

(defn as-vec [x]
  (if (vector? x)
    x
    [x]))

(defn- can-eval? [r]
  (not (string? r)))

(defn- eval-patterns [agentlang-patterns context-event]
  (if (can-eval? agentlang-patterns)
    (let [env (env/bind-instance env/EMPTY context-event)]
      (loop [pats (as-vec agentlang-patterns), env env, result nil]
        (if-let [p (first pats)]
          (let [r (ev/evaluate-pattern env p)]
            (if (u/safe-ok-result r)
              (recur (rest pats) (:env r) r)
              (do (log/error (str "inferred-pattern " p " failed with result " r))
                  r)))
          (do (log/info (str "inference succeeded with result " result))
              result))))
    agentlang-patterns))

(defn run-inference-for-event
  ([event instructions agent-instance]
   (log/info (str "Processing response for inference " (cn/instance-type event)
                  " - " (u/pretty-str agent-instance)))
   (let [agent-instance
         (ar/handle-generic-agent (assoc agent-instance :UserInstruction
                                         (s/trim
                                          (str (or (:UserInstruction agent-instance) "")
                                               "\n"
                                               (or instructions "")
                                               "\n"
                                               (or (get-in agent-instance [:Context :UserInstruction]) "")))))
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
