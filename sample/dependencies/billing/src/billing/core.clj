(ns billing.core
  (:require [fractl.core :as f]
            [banking.core]
            [billing.model.model]))

(defn -main [& args] (apply f/-main args))
