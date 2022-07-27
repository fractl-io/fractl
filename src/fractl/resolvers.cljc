(ns fractl.resolvers
  ;; load resolvers required by kernel
  (:require [fractl.resolver.meta]
            [fractl.resolver.timer]
            [fractl.resolver.policy]
            #?(:clj [fractl.resolver.data-sync])
            ;; resolvers to be moved to external repositories
            #?(:clj [fractl.resolver.git])
            #?(:clj [fractl.resolver.email])
            #?(:clj [fractl.resolver.sms])
            #?(:clj [fractl.resolver.aws])
            #?(:clj [fractl.resolver.sns])))
