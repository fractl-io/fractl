(ns fractl.auth.model
  (:require [fractl.lang
             :refer [component event entity dataflow]]
            [fractl.util.auth :as au]))

(component :Kernel.Identity)

(entity
 :Kernel.Identity/User
 {:Name {:type :Kernel/String
         :identity true}
  :Password {:type :Kernel/Password
             :optional true} ; may use social-login
  :FirstName {:type :Kernel/String
              :optional true}
  :LastName {:type :Kernel/String
             :optional true}
  :Email {:type :Kernel/Email
          :optional true}})

(event
 :Kernel.Identity/SignUp
 {:User :Kernel.Identity/User})

(event
 au/login-event-name
 {au/login-username :Kernel/String
  au/login-password :Kernel/Password})

(event
 :Kernel.Identity/FindUser
 {:Name :Kernel/String})

(dataflow
 :Kernel.Identity/FindUser
 {:Kernel.Identity/User
  {:Name? :Kernel.Identity/FindUser.Name}})
