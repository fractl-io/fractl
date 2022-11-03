(ns billing.core
  (:require [fractl.core :as f]
            [banking.core]
            [billing.model.billing.core]))

(defn -main [& args] (apply f/-main args))
