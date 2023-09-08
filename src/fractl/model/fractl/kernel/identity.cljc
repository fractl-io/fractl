(ns
 fractl.model.fractl.kernel.identity
 (:use
  [fractl.model.fractl.kernel.lang
   :only
   [Fractl_Kernel_Lang___COMPONENT_ID__]]
  [fractl.lang
   :only
   [dataflow entity attribute relationship component event record]]))
(component :Fractl.Kernel.Identity {:refer [:Fractl.Kernel.Lang]})
(entity
 :Fractl.Kernel.Identity/User
 {:Name {:type :String, :optional true},
  :Password {:type :Password, :optional true},
  :FirstName {:type :String, :optional true},
  :LastName {:type :String, :optional true},
  :Email {:type :Email, :identity true},
  :UserData {:type :Map, :optional true}})
(event
 :Fractl.Kernel.Identity/SignUp
 {:User :Fractl.Kernel.Identity/User})
(event
 :Fractl.Kernel.Identity/PostSignUp
 {:SignupRequest :Fractl.Kernel.Identity/SignUp, :SignupResult :Any})
(dataflow
 :Fractl.Kernel.Identity/SignUp
 :Fractl.Kernel.Identity/SignUp.User)
(entity
 :Fractl.Kernel.Identity/UserExtra
 {:User :Fractl.Kernel.Identity/User, :OtherDetails :Map})
(event
 :Fractl.Kernel.Identity/UpdateUser
 {:UserDetails :Fractl.Kernel.Identity/UserExtra})
(event :Fractl.Kernel.Identity/ForgotPassword {:Username :Email})
(event
 :Fractl.Kernel.Identity/ConfirmForgotPassword
 {:Username :Email, :ConfirmationCode :String, :Password :String})
(event
 :Fractl.Kernel.Identity/ConfirmSignUp
 {:Username :Email, :ConfirmationCode :String})
(event
 :Fractl.Kernel.Identity/ChangePassword
 {:AccessToken :String, :CurrentPassword :String, :NewPassword :String})
(event :Fractl.Kernel.Identity/RefreshToken {:RefreshToken :String})
(event
 :Fractl.Kernel.Identity/UserLogin
 {:Username :String, :Password :Password})
(event :Fractl.Kernel.Identity/FindUser {:Email :Email})
(dataflow
 :Fractl.Kernel.Identity/FindUser
 #:Fractl.Kernel.Identity{:User
                          {:Email?
                           :Fractl.Kernel.Identity/FindUser.Email}})
(def
 Fractl_Kernel_Identity___COMPONENT_ID__
 "01343459-581a-413c-9f39-cac175cbe3de")
