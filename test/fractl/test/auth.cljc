(ns fractl.test.auth
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.auth.oauth2 :as oauth2]))

(deftest basic
  #?(:clj (is (oauth2/google))))
