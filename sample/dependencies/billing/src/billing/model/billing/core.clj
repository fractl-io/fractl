(ns billing.model.billing.core
  (:use [fractl.lang]))

(component :Billing.Core)

(entity
 :Billing.Core/Product
 {:Name {:type :Kernel/String
         :identity true}
  :Price :Kernel/Decimal
  :Stock :Kernel/Int})

(defn update-stock [current-stock purchase-count]
  (let [new-stock (- current-stock purchase-count)]
    (if (>= new-stock 0)
      new-stock
      (throw (Exception. "not enough stock")))))

(defn amount-to-debit [product-price order-count]
  (* product-price order-count))

(record
 :Billing.Core/PurchaseResult
 {:Result :Kernel/Any})

(dataflow
 :Billing.Core/Purchase
 {:Billing.Core/Product
  {:Name? :Billing.Core/Purchase.Product
   :Stock '(billing.model.billing.core/update-stock :Stock :Billing.Core/Purchase.Count)}
  :as :Prod}
 {:Banking.Core/Transaction
  {:Account "0001"
   :Type "dr"
   :Amount '(billing.model.billing.core/amount-to-debit :Prod.Price :Billing.Core/Purchase.Count)}
  :as :Acc}
 {:Billing.Core/PurchaseResult
  {:Result [:Prod :Acc]}})

(dataflow
 :Kernel/AppInit
 {:Banking.Core/Account
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
