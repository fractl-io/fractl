(ns fractl.client.library.catalog
  "Manage a list of books in the library"
  (:require [fractl.client.library.identity]
            [fractl.lang.string :as s]
            [fractl.lang.datetime :as dt]
            [fractl.lang :refer [entity event dataflow component]]))

(component :Library.Catalog)

(def valid-name? (partial s/string-in-range? 3 120))
(def app-name? valid-name?)
(def app-artifact? (partial s/string-in-range? 1 10485760)) ; max size - 10MB

(entity {:Book
         {:Name {:check app-name?}
          :Publisher {:ref :Library.Identity/User.Id
                      :indexed true}
          :PublishDate {:type :Kernel/DateTime
                        :immutable true
                        :default dt/now}
          :LastUpdated {:type :Kernel/DateTime
                        :default dt/now}}})

(event {:ListBooks
        {:Publisher :Kernel/UUID}})

(dataflow :ListBooks
          {:Book {:Publisher? :Library.Catalog/ListBooks.Publisher}})

(event {:ListAllBooks
        {}})

(dataflow :ListAllBooks
          :Book?)

