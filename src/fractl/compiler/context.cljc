(ns fractl.compiler.context
  "Context information attached to a compilation session."
  (:require [fractl.util :as u]))

(defn make []
  (u/make-cell {}))

(defn put-record!
  "A record/entity/event encounterd during the compilation
  process. This can be used to validate references downstream."  
  [ctx rec-name schema]
  (u/safe-set ctx (assoc @ctx rec-name schema)))

(defn fetch-record [ctx rec-name]
  (get @ctx rec-name))

(defn bind-variable! [ctx k v]
  (u/safe-set ctx (assoc @ctx k v)))

(defn fetch-variable [ctx k]
  (find @ctx k))

(defn lookup-record [ctx instance path]
  (if-let [r (fetch-record ctx path)]
    r
    (u/throw-ex (str "instance not found - " path))))

(defn lookup-variable [ctx k]
  (if-let [r (fetch-variable ctx k)]
    r
    (u/throw-ex (str "unbound variable - " k))))

(defn bind-resolver! [ctx r]
  (bind-variable! ctx :resolver r))

(defn fetch-resolver [ctx]
  (second (fetch-variable ctx :resolver)))
