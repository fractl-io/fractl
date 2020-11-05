(ns fractl.test.test-runner
  "This ns should be used to run tests with `figwheel-main` test runner."
  (:require [cljs-test-display.core]
            [figwheel.main.testing :refer-macros [run-tests run-tests-async]]
            ;; [fractl.test.basic]
            [fractl.test.core-test]))

(run-tests 'fractl.test.core-test)


(defn -main [& args]
  ;; this needs to be the last statement in the main function so that it can
  ;; return the value `[:figwheel.main.async-result/wait 10000]`
  (run-tests))
