(ns fractl.util.common)

(defn throw-ex-info
  ([msg errobj]
   (throw (ex-info msg errobj)))
  ([msg] (throw-ex-info msg nil)))

(defn throw-ex
  [msg]
  #?(:clj
     (throw (Exception. msg))
     :cljs
     (let [e (js/Error. msg)]
       (println msg)
       (.log js/console (.-stack e))
       (throw e))))
