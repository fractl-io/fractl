(ns fractl.resolvers
  ;; load resolvers required by kernel
  (:require [fractl.resolver.meta]
            [fractl.resolver.timer]
            [fractl.resolver.policy]
            #?(:clj [fractl.resolver.data-sync])))
