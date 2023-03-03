(ns
 fractl.model.fractl.kernel.identity
 (:use
  [fractl.model.fractl.kernel.lang
   :only
   [Fractl.Kernel.Lang___COMPONENT-ID__]]
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
 {:SignupResult :Any, :UserData :Any})
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
 Fractl.Kernel.Identity___COMPONENT-ID__
 "1882be98-0905-4a9d-a9f5-420e3cbf17a8")
