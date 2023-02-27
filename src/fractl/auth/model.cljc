(ns fractl.auth.model
  (:require [fractl.lang
             :refer [component event entity dataflow]]
            [fractl.util.auth :as au]))

(component :Kernel.Identity)

(entity
 :Kernel.Identity/User
 {:Name {:type :Kernel.Lang/String
         :optional true}
  :Password {:type :Kernel.Lang/Password
             :optional true} ; may use social-login
  :FirstName {:type :Kernel.Lang/String
              :optional true}
  :LastName {:type :Kernel.Lang/String
             :optional true}
  :Email {:type :Kernel.Lang/Email
          :identity true}
  :UserData {:type :Kernel.Lang/Map :optional true}})

(event
 :Kernel.Identity/SignUp
 {:User :Kernel.Identity/User})

(event
 :Kernel.Identity/PostSignUp
 {:SignupResult :Kernel.Lang/Any
  :UserDetails :Kernel.Lang/Any})

(dataflow
 :Kernel.Identity/SignUp
 :Kernel.Identity/SignUp.User)

(entity
 :Kernel.Identity/UserExtra
 {:User :Kernel.Identity/User
  :OtherDetails :Kernel.Lang/Map})

(event
 :Kernel.Identity/UpdateUser
 {:UserDetails :Kernel.Identity/UserExtra})

(event
 :Kernel.Identity/ForgotPassword
 {:Username :Kernel.Lang/Email})

(event
 :Kernel.Identity/ConfirmForgotPassword
 {:Username :Kernel.Lang/Email
  :ConfirmationCode :Kernel.Lang/String
  :Password :Kernel.Lang/String})

(event
 :Kernel.Identity/ChangePassword
 {:AccessToken :Kernel.Lang/String
  :CurrentPassword :Kernel.Lang/String
  :NewPassword :Kernel.Lang/String})

(event
 :Kernel.Identity/RefreshToken
 {:RefreshToken :Kernel.Lang/String})

(event
 au/login-event-name
 {au/login-username :Kernel.Lang/String
  au/login-password :Kernel.Lang/Password})

(event
 :Kernel.Identity/FindUser
 {:Email :Kernel.Lang/Email})

(dataflow
 :Kernel.Identity/FindUser
 {:Kernel.Identity/User
  {:Email? :Kernel.Identity/FindUser.Email}})
