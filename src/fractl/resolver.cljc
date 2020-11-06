(ns fractl.resolver
  "Pattern resolvers, aka evaluators"
  (:require [fractl.util :as u]
            [fractl.lang.opcode :as opc]
            [fractl.resolver.protocol :as p]
            [fractl.resolver.root :as r]))

(defn- dispatch-an-opcode [env opcode resolver]
  (if-let [f ((opc/op opcode) p/dispatch-table)]
    (f resolver env (opc/arg opcode))
    (u/throw-ex (str "no dispatcher for opcode - " (opc/op opcode)))))

(defn dispatch [env {opcode :opcode resolver :resolver :as x}]
  (if (map? opcode)
    (dispatch-an-opcode env opcode resolver)
    (loop [opcs opcode, env env, result nil]
      (if-let [opc (first opcs)]
        (let [r (dispatch-an-opcode env opc resolver)]
          (recur (rest opcs) (:env r) r))
        result))))

;; The database of registered resolvers.
(def ^:private db (ref {}))

(defn resolver-for-path [path eval-event-dataflows]
  (or (get db path)
      (r/get-default-resolver eval-event-dataflows)))
