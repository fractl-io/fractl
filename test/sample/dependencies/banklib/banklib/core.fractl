(component :Banklib.Core)

(entity
 :Account
 {:No {:type :String
       :guid true}
  :Balance :Decimal})

(defn set-balance [transaction-type transaction-amount current-balance]
  ((if (= "dr" transaction-type) - +)
   current-balance transaction-amount))

(event
 :Transaction
 {:Account :String
  :Type :String
  :Amount :Decimal})

(dataflow
 :Transaction
 {:Account
  {:No? :Transaction.Account
   :Balance '(set-balance
              :Transaction.Type
              :Transaction.Amount
              :Balance)}})

(dataflow
 :AppInit
 {:Account
  {:No "0001"
   :Balance 1500M}})
