(ns banking.model.banking.core
  (:use [fractl.lang]))

(component :Banking.Core)

(entity
 :Banking.Core/Account
 {:No {:type :Kernel/String
       :identity true}
  :Balance :Kernel/Decimal})

(defn set-balance [transaction-type transaction-amount current-balance]
  ((if (= "dr" transaction-type) - +)
   current-balance transaction-amount))

(event
 :Banking.Core/Transaction
 {:Account :Kernel/String
  :Type :Kernel/String
  :Amount :Kernel/Decimal})

(dataflow
 :Banking.Core/Transaction
 {:Banking.Core/Account
  {:No? :Banking.Core/Transaction.Account
   :Balance '(banking.model.banking.core/set-balance
              :Banking.Core/Transaction.Type
              :Banking.Core/Transaction.Amount
              :Balance)}})

(dataflow
 :Kernel/AppInit
 {:Banking.Core/Account
  {:No "0001"
   :Balance 1500M}})
