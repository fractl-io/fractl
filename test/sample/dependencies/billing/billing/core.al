(component
 :Billing.Core
 ;; clj-import only for testing build
 {:refer [:Banklib.Core]
  :clj-import '[(:require [agentlang.util :as u])
                (:import [java.io File])]})

(entity
 :Billing.Core/Product
 {:Name {:type :String
         :guid true}
  :Price :Decimal
  :Stock :Int})

(defn update-stock [current-stock purchase-count]
  (let [new-stock (- current-stock purchase-count)]
    (if (>= new-stock 0)
      new-stock
      (throw (Exception. "not enough stock")))))

(defn amount-to-debit [product-price order-count]
  (* product-price order-count))

(record
 :Billing.Core/PurchaseResult
 {:Result :Any})

(dataflow
 :Billing.Core/Purchase
 {:Billing.Core/Product
  {:Name? :Billing.Core/Purchase.Product
   :Stock '(update-stock :Stock :Billing.Core/Purchase.Count)}
  :as :Prod}
 {:Banklib.Core/Transaction
  {:Account "0001"
   :Type "dr"
   :Amount '(amount-to-debit :Prod.Price :Billing.Core/Purchase.Count)}
  :as :Acc}
 {:Billing.Core/PurchaseResult
  {:Result [:Prod :Acc]}})

(dataflow
 :AppInit
 {:Banklib.Core/Account
  {:No "0001"
   :Balance 1500M}}
 {:Billing.Core/Product
  {:Name "k01"
   :Price 14M
   :Stock 100}}
 {:Billing.Core/Product
  {:Name "k02"
   :Price 20M
   :Stock 10}})
