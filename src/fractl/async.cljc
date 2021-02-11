(ns fractl.async
  #?(:clj
     (:import [java.util.concurrent ExecutorService Executors])
     :cljs
     (:require [promesa.core :as p]
               [promesa.exec :as exec])))

(def ^:private service
  #?(:clj (Executors/newCachedThreadPool)))

(defn async-invoke [f]
  #?(:clj
     (.submit ^ExecutorService service
              ^Callable f)
     :cljs
     (p/create
      (fn [resolve reject]
        (resolve (f)))
      exec/default-executor)))
