(ns fractl.lang.tools.nrepl.core
  (:require
    [fractl.component :as cn]
    [fractl.lang :as ln]
    [fractl.lang.tools.repl :as repl]
    [fractl.lang.tools.replcmds :as replcmds]
    [fractl.util.runtime :as ur]))

(defonce nrepl-eval-init (atom nil))

(defn initialize-nrepl-environment [model-name store evaluator]
  "Initializes the REPL environment for a given model name."
  (let [model-name (or model-name (repl/infer-model-name))
        current-cn (cn/get-current-component)
        decl-names (cn/declared-names current-cn)]
    (when decl-names
      (repl/set-declared-names! current-cn decl-names))
    (use '[fractl.lang])
    (use '[fractl.lang.tools.replcmds])
    (ln/component repl/repl-component)
    (let [cn (if (= model-name :fractl)
               repl/repl-component
               current-cn)]
      (replcmds/switch cn))
    (partial repl/repl-eval store (atom nil) evaluator)))

(defn init-repl-eval-func [model-name options]
  (ur/force-call-after-load-model
    model-name
    (fn []
      (let [model-info (ur/read-model-and-config options)
            [[ev store] _] (ur/prepare-repl-runtime model-info)]
        (reset! nrepl-eval-init (initialize-nrepl-environment model-name store ev))))))
