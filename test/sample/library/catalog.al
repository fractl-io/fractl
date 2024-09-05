(ns sample.library.catalog
  "Manage a list of books in the library"
  (:use [agentlang.lang]
        [agentlang.lang.datetime]
        [agentlang.lang.string]
        [agentlang.lang.b64])
  (:require [agentlang.component :as cn]))

(component :Test.Sample.Library.Catalog)

(def valid-name? (partial string-in-range? 3 120))
(def app-name? valid-name?)
(def app-artifact? (partial string-in-range? 1 10485760)) ; max size - 10MB

(entity {:Test.Sample.Library.Catalog/Book
         {:Name {:check app-name?}
          :ISBN {:type :String :guid true}
          :Publisher {:ref :Test.Sample.Library.Identity/User.Email
                      :indexed true}
          :PublishDate {:type :DateTime
                        :immutable true
                        :default now}
          :LastUpdated {:type :DateTime
                        :default now}
          :IsCheckedout {:type :Boolean
                         :default false}
          :LastCheckout {:type :UUID
                         :optional true}}})

(event {:ListBooks {:Publisher :UUID}})

(dataflow :ListBooks {:Book {:Publisher? :ListBooks.Publisher}})

(event {:ListAllBooks {}})

(dataflow :ListAllBooks :Book?)
