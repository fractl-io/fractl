(ns
 fractl.model.fractl.kernel.identity
 (:require [fractl.lang.internal :as li] [fractl.util :as u])
 (:use
  [fractl.model.fractl.kernel.lang
   :only
   [Fractl_Kernel_Lang___COMPONENT_ID__]]
  [fractl.lang
   :only
   [dataflow
    entity
    view
    attribute
    rule
    relationship
    component
    event
    inference
    record]]))
(component
 :Fractl.Kernel.Identity
 {:refer [:Fractl.Kernel.Lang],
  :clj-import
  '[(:require [fractl.lang.internal :as li] [fractl.util :as u])]})
(entity
 :Fractl.Kernel.Identity/User
 {:Name {:type :String, :optional true},
  :Password {:type :Password, :optional true},
  :FirstName {:type :String, :optional true},
  :LastName {:type :String, :optional true},
  :Email {:type :Email, li/guid true},
  :UserData {:type :Any, :optional true},
  :AppId {:type :UUID, :default u/uuid-string, :indexed true}})
(event
 :Fractl.Kernel.Identity/SignUp
 {:User :Fractl.Kernel.Identity/User})
(event
 :Fractl.Kernel.Identity/PostSignUp
 {:SignupRequest :Fractl.Kernel.Identity/SignUp, :SignupResult :Any})
(dataflow
 :Fractl.Kernel.Identity/SignUp
 {:Fractl.Kernel.Identity/User {},
  :from :Fractl.Kernel.Identity/SignUp.User})
(entity
 :Fractl.Kernel.Identity/UserExtra
 {:User :Fractl.Kernel.Identity/User, :OtherDetails :Map})
(entity
 :Fractl.Kernel.Identity/UserSession
 {:User {:type :String, :guid true}, :LoggedIn :Boolean})
(entity
 :Fractl.Kernel.Identity/SessionCookie
 {:Id {:type :String, :guid true}, :UserData :Any})
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
(event
 :Fractl.Kernel.Identity/ResendConfirmationCode
 {:Username :Email})
(dataflow
 [:after :delete :Fractl.Kernel.Identity/User]
 [:delete
  :Fractl.Kernel.Rbac/InstancePrivilegeAssignment
  {:Assignee :Instance.Email}]
 [:delete :Fractl.Kernel.Rbac/InstancePrivilegeAssignment :purge]
 [:delete
  :Fractl.Kernel.Rbac/OwnershipAssignment
  {:Assignee :Instance.Email}]
 [:delete :Fractl.Kernel.Rbac/OwnershipAssignment :purge]
 [:delete
  :Fractl.Kernel.Rbac/RoleAssignment
  {:Assignee :Instance.Email}]
 [:delete :Fractl.Kernel.Rbac/RoleAssignment :purge])
(entity
 :Fractl.Kernel.Identity/AuditTrailEntry
 {:Id {:type :UUID, :default u/uuid-string, :indexed true},
  :EntityName :String,
  :EntityId :String,
  :Action {:oneof ["create" "update" "delete"]},
  :Timestamp :Int,
  :User :String,
  :SessionToken {:type :String, :optional true}})
(def
 Fractl_Kernel_Identity___COMPONENT_ID__
 "f7ace7b2-6336-4b01-9f76-0fa9d985608a")
