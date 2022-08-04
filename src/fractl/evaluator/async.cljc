(ns fractl.evaluator.async
  #?(:clj
     (:import [java.util.concurrent ExecutorService Executors])))

(def ^:private service
  #?(:clj (Executors/newCachedThreadPool)))

(defn async-invoke [f]
  #?(:clj
     (.submit ^ExecutorService service
              ^Callable f)
     :cljs
     ;; Concurrency not yet implemented in cljs.
     (f)))
