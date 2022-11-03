(ns banking.core
  (:require [fractl.core :as f]
            [banking.model.model]))

(defn -main [& args] (apply f/-main args))
