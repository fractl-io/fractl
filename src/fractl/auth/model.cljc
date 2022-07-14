(ns fractl.auth.model
  (:require [fractl.lang
             :refer [component event entity dataflow]]))

(component :Kernel.Identity)

(entity
 :Kernel.Identity/User
 {:Name {:type :Kernel/String
         :indexed true
         :unique true}
  :Password {:type :Kernel/Password
             :optional true} ; may use social-login
  :FirstName {:type :Kernel/String
              :optional true}
  :LastName {:type :Kernel/String
             :optional true}
  :Email {:type :Kernel/Email
          :optional true}})

(def login-event-name :Kernel.Identity/UserLogin)

(event
 login-event-name
 {:Username :Kernel/String
  :Password :Kernel/Password})

(def login-username :Username)
(def login-password :Password)

(dataflow
 :Kernel.Identity/FindUser
 {:Kernel.Identity/User
  {:Name? :Kernel.Identity/FindUser.Name}})
