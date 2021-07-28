(ns fractl.deploy.util
  (:require [clojure.string :as s]
            [fractl.util :as u]
            [fractl.util.logger :as log])
  (:use [clojure.java.shell :only [sh]]))
  
(defn run-shell-command
  ([cmd ok-exit-code print-out]
   (let [cmd-str (s/join " " cmd)]
     (log/info cmd-str)
     (let [r (apply sh cmd)
           status (:exit r)]
       (log/info (:out r))
       (when print-out
         (println (:out r)))
       (if (= ok-exit-code status)
         true
         (u/throw-ex
          (str "`" cmd-str "` - command failed with exit code - " status))))))
  ([cmd ok-exit-code]
   (run-shell-command cmd ok-exit-code false))
  ([cmd]
   (run-shell-command cmd 0)))

(defn run-shell-command-ignore-error [cmd ok-exit-code print-out]
  (try
    (run-shell-command cmd ok-exit-code print-out)
    (catch Exception ex
      (log/error ex))))
