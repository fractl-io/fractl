(ns fractl.auth.model
  (:require [fractl.lang
             :refer [component event entity dataflow]]
            [fractl.util.auth :as au]))

(component :Kernel.Identity)

(entity
 :Kernel.Identity/User
 {:Name {:type :Kernel/String
         :optional true}
  :Password {:type :Kernel/Password
             :optional true} ; may use social-login
  :FirstName {:type :Kernel/String
              :optional true}
  :LastName {:type :Kernel/String
             :optional true}
  :Email {:type :Kernel/Email
          :identity true}})

(event
 :Kernel.Identity/SignUp
 {:User :Kernel.Identity/User})

(entity
 :Kernel.Identity/UserExtra
 {:User :Kernel.Identity/User
  :OtherDetails :Kernel/Map})

(event
 :Kernel.Identity/UpdateUser
 {:UserDetails :Kernel.Identity/UserExtra})

(event
 :Kernel.Identity/ForgotPassword
 {:Username :Kernel/Email})

(event
 :Kernel.Identity/ConfirmForgotPassword
 {:Username :Kernel/Email
  :ConfirmationCode :Kernel/String
  :Password :Kernel/String})

(event
 :Kernel.Identity/ChangePassword
 {:AccessToken :Kernel/String
  :CurrentPassword :Kernel/String
  :NewPassword :Kernel/String})

(event
 au/login-event-name
 {au/login-username :Kernel/String
  au/login-password :Kernel/Password})

(event
 :Kernel.Identity/FindUser
 {:Email :Kernel/Email})

(dataflow
 :Kernel.Identity/FindUser
 {:Kernel.Identity/User
  {:Email? :Kernel.Identity/FindUser.Email}})
