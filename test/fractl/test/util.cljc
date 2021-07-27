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

(defn embedded-results [r]
  (fresult (first (second (first r)))))

(defn uuid-string []
  #?(:clj
     (str (java.util.UUID/randomUUID))
     :cljs
     (str (random-uuid))))

(defn first-result [evt]
  (ffirst
   (fresult
    (e/eval-all-dataflows evt))))

#?(:clj
   (def postgres-config
     {:type     :postgres
      :host     (System/getenv "POSTGRES_HOST")
      :dbname   "postgres"
      :username "postgres"
      :password (System/getenv "POSTGRES_PASSWORD")}))

(store/open-default-store
  #?(:clj {:type :h2}
     :cljs {:type :reagent}))

#_(store/open-default-store
  ;; To test postgres in CI, uncomment the following,
  #?(:clj {:type     :postgres
           :host     (System/getenv "POSTGRES_HOST")
           :dbname   "postgres"
           :username "postgres"
           :password (System/getenv "POSTGRES_PASSWORD")}
     :cljs {:type :alasql}))
