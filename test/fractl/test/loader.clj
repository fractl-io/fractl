(ns fractl.test.loader
  "Loader specific tests. Only work for Clojure."
  (:require [clojure.test :refer [deftest is]]
            [fractl.lang.loader :as loader]))

(deftest test-load-script
  (is :Sample.Simple (loader/load-script nil "sample/simple.fractl"))
  (is :Platform.Identity (loader/load-script nil "platform/identity.fractl"))
  (is :Platform.AppCatalog (loader/load-script nil "platform/app_catalog.fractl"))
  (is (nil? (loader/load-script nil "platform/model.fractl"))))

(deftest test-read-expressions
  (is :Sample.Simple/LoggingPolicy (first (loader/read-expressions "sample/simple.fractl")))
  (is (some #{:Sample.Simple/E3} (loader/read-expressions "sample/simple.fractl")))
  (is [:Platform.Identity :Platform.AppCatalog] (:components (first (loader/read-expressions "platform/model.fractl"))))
  (is (some #{:Platform.Identity/User} (loader/read-expressions "platform/identity.fractl"))))
