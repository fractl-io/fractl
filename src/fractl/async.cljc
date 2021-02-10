(ns fractl.async
  #?(:clj (:import [java.util.concurrent ExecutorService Executors])))

(def ^:private service
  #?(:clj (Executors/newCachedThreadPool)))

(defn async-invoke [f]
  #?(:clj
     (.submit ^ExecutorService service
              ^Callable f)))
