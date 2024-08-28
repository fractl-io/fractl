(ns sample.library.identity
  "Simple user-identity management for the library"
  (:require [clojure.string :as s])
  (:use [agentlang.lang]
        [agentlang.lang.datetime]
        [agentlang.lang.string]))

(component :Test.Sample.Library.Identity)

(def user-name? (partial string-in-range? 2 50))

(defn password? [s]
  (or (agentlang.util.hash/crypto-hash? s)
      (string-in-range? 3 20 s)))

(entity {:User
         {:UserName    {:type   :String
                        :check  user-name?
                        :unique true}
          :Designation {:oneof ["incharge" "general" "intern"]}
          :Password    {:type  :Password
                        :check password?}
          :Email       {:type   :Email
                        :guid true}
          :DateCreated {:type    :DateTime
                        :default now}}})

(entity {:Member
         {:Name     {:type  :String
                     :check user-name?}
          :UserName {:type   :String
                     :check  user-name?
                     :unique true}
          :Password {:type  :Password
                     :check password?}
          :Email    {:type   :Email
                     :guid true}
          :DOB      :String
          :DateCreated {:type    :DateTime
                        :default now}
          ;; Membership Types obtained from here.
          ;; https://www.londonlibrary.co.uk/join/types-of-membership
          :Designation {:oneof ["life" "individual" "family"
                                "remote" "oversees" "supported" "associate" "temporary"]}}})
