(ns fractl.test-runner
  "This ns should be used to run tests with `figwheel-main` test runner."
  (:require [cljs-test-display.core]
            [figwheel.main.testing :refer-macros [run-tests run-tests-async]]
            [fractl.test.basic]))

(defn -main [& args]
  (run-tests-async 'fractl.test.basic)
  ;; return a message to the figwheel process that tells it to wait
  [:figwheel.main.async-result/wait 100000])

