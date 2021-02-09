(ns fractl.client.library.identity
  "Simple user-identity management for the library"
  (:require [fractl.lang.string :as s]
            [fractl.lang.datetime :as dt]
            [fractl.lang :refer [entity component]]))

(component :Library.Identity)

(def user-name? (partial s/string-in-range? 2 50))
(def password? (partial s/string-in-range? 3 20))

(entity {:User
         {:UserName {:type :Kernel/String
                     :check user-name?
                     :unique true}
          :Password {:type :Kernel/Password
                     :check password?}
          :Email {:type :Kernel/Email
                  :unique true}
          :DateCreated {:type :Kernel/DateTime
                        :default dt/now}}})
