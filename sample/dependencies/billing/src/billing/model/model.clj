(ns billing.model.model
  (:require [billing.model.billing.core]))

{:name :Billing
 :components [:Billing.Core]
 :external-dependencies [:Banking]}
