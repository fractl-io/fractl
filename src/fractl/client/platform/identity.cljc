(ns fractl.client.platform.identity
  "Simple user-identity management for the app-catalog"
  (:require [fractl.lang :refer [entity component]]
            [fractl.lang.string :as str]
            [fractl.lang.datetime :as dt]))

(component :Platform.Identity)

(def user-name? (partial str/string-in-range? 2 50))
(def password? (partial str/string-in-range? 3 20))

(entity {:User
         {:UserName {:type :Kernel/String
                     :check user-name?
                     :unique true}
          :Password {:type :Kernel/String
                     :check password?}
          :Email {:type :Kernel/Email
                  :unique true}
          :DateCreated {:type :Kernel/DateTime
                        :default dt/now}}})
