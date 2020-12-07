(ns fractl.test.util
  (:require #?(:clj [clojure.test :refer [is]]
               :cljs [cljs.test :refer-macros [is]])
            [fractl.store :as store]
            [fractl.evaluator :as e]))

(defn- report-expected-ex [ex]
  (println (str "Expected exception in test: "
                #?(:clj (.getMessage ex)
                   :cljs ex)))
  ex)

(defn is-error [f]
  (is (try
        (do (f) false)
        #?(:clj  (catch Exception ex
                   (report-expected-ex ex))
           :cljs (catch js/Error e
                   (report-expected-ex e))))))

(defmacro defcomponent [component & body]
  `(do (fractl.lang/component ~component)
       ~@body
       ~component))

(defn fresult [r]
  (:result (first r)))

(def store (store/open-default-store nil))

(def make-df-eval e/evaluator)
