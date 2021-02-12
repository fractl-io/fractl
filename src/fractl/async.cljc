(ns fractl.async
  #?(:clj
     (:import [java.util.concurrent ExecutorService Executors])
     :cljs
     (:require [cljs.core.async :refer [chan >!]]))
  #?(:cljs
     (:require-macros [cljs.core.async.macros :refer [go]])))

(def ^:private service
  #?(:clj (Executors/newCachedThreadPool)))

(defn async-invoke [f]
  #?(:clj
     (.submit ^ExecutorService service
              ^Callable f)
     :cljs
     (let [c (chan)]
       (go (>! c (f)))
       c)))
