(ns fractl.test-runner
  "This ns should be used to run tests with `figwheel-main` test runner."
  (:require [cljs-test-display.core]
            [figwheel.main.testing :refer-macros [run-tests run-tests-async]]
            [fractl.test.basic]
            [fractl.test.features01]
            [fractl.test.features02]
            [fractl.test.features03]
            [fractl.test.fixes01]
            [fractl.test.fixes02]
            [fractl.test.fixes03]
            [fractl.test.policy]
            [fractl.test.query]
            [fractl.test.rbac]
            [fractl.test.reagent]
            [fractl.test.resolver]
            [fractl.test.timer]
            [fractl.test.util]))

(defn -main [& args]
  (run-tests-async 100000)
  ;; return a message to the figwheel process that tells it to wait
  [:figwheel.main.async-result/wait 100000])

