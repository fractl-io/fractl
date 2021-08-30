(ns fractl.test.util
  (:require [fractl.evaluator :as e]
            #?(:clj  [clojure.test :refer [is]]
               :cljs [cljs.test :refer-macros [is]])
            [fractl.store :as store]))

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

(defn nth-result [r n]
  (:result (nth r n)))

(defn embedded-results [r]
  (fresult (first (second r))))

(defn uuid-string []
  #?(:clj
     (str (java.util.UUID/randomUUID))
     :cljs
     (str (random-uuid))))

(defn first-result [evt]
  (first
   (fresult
    (e/eval-all-dataflows evt))))

(defn sleep [msec f]
  #?(:clj
     (do
       (try
         (Thread/sleep msec)
         (catch Exception ex
           nil))
       (f))
     :cljs
     (js/setTimeout f msec)))

;; To test postgres in CI, set to true
(def test-with-postgres false)

(store/open-default-store
 #?(:clj (when test-with-postgres
           {:type     :postgres
            :host     (System/getenv "POSTGRES_HOST")
            :dbname   "postgres"
            :username "postgres"
            :password (System/getenv "POSTGRES_PASSWORD")})
    :cljs {:type :alasql}))
