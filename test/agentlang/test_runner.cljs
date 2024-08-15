(ns agentlang.test-runner
  "This ns should be used to run tests with `figwheel-main` test runner."
  (:require [cljs-test-display.core]
            [figwheel.main.testing :refer-macros [run-tests run-tests-async]]
            [agentlang.test.basic]
            [agentlang.test.resolver]
            [agentlang.test.query]))

(defn -main [& args]
  (run-tests-async 100000)
  ;; return a message to the figwheel process that tells it to wait
  [:figwheel.main.async-result/wait 100000])

