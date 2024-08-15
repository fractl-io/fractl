(ns agentlang.resolvers
  ;; load resolvers required by kernel
  (:require [agentlang.resolver.meta]
            [agentlang.resolver.timer]
            [agentlang.resolver.policy]
            #?(:clj [agentlang.resolver.data-sync])))
