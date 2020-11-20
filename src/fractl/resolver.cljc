(ns fractl.resolver
  (:require [fractl.resolver.registry :as rg]))

(def lookup-resolver rg/resolver-for-path)
