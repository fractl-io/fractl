(ns sample.library.ledger
  "Manage a list of books in the library"
  (:use [agentlang.lang]
        [agentlang.lang.datetime]
        [agentlang.lang.string]
        [agentlang.lang.b64])
  (:require [agentlang.component :as cn]))

(component :Test.Sample.Library.Ledger)

(entity {:Test.Sample.Library.Ledger/CheckoutLog
         {:Member       {:ref     :Test.Sample.Library.Identity/Member.Email
                         :indexed true}
          :Designation  :String
          :Book         {:ref     :Test.Sample.Library.Catalog/Book.ISBN
                         :indexed true}
          :IsCheckedout {:type    :Boolean
                         :default true}
          :CheckoutDate {:type    :DateTime
                         :default now}
          :ReturnDate   {:type     :DateTime
                         :optional true}
          :meta         {:unique [:Member :Book]}}})

(entity {:Authentication
         {:Owner         :Any
          :Issued        {:type :DateTime :optional true}
          :ExpirySeconds {:type :Int :default 300}}})

(dataflow :Test.Sample.Library.Ledger/MemberLogin
          {:Test.Sample.Library.Identity/Member {:UserName? :Test.Sample.Library.Ledger/MemberLogin.UserName}}
          [:match :Test.Sample.Library.Identity/Member.Password
           :Test.Sample.Library.Ledger/MemberLogin.Password  {:Authentication {:Owner :Test.Sample.Library.Identity/Member}}])

(dataflow :Test.Sample.Library.Ledger/UserLogin
          {:Test.Sample.Library.Identity/User {:UserName? :Test.Sample.Library.Ledger/UserLogin.UserName}}
          [:match :Test.Sample.Library.Identity/User.Password
           :Test.Sample.Library.Ledger/UserLogin.Password  {:Authentication {:Owner :Test.Sample.Library.Identity/User}}])

(event {:Test.Sample.Library.Ledger/CheckoutBook
        {:Member   :Email
         :Designation :String
         :Book     :String
         :Backend  :String
         :Receiver :String
         :Subject  :String
         :Text     :String
         :To       :String}})

(dataflow :Test.Sample.Library.Ledger/CheckoutBook
          {:Test.Sample.Library.Ledger/CheckoutLog {:Member       :Test.Sample.Library.Ledger/CheckoutBook.Member
                                               :Designation  :Test.Sample.Library.Ledger/CheckoutBook.Designation
                                               :Book         :Test.Sample.Library.Ledger/CheckoutBook.Book
                                               :IsCheckedout true}}
          #_{:Email/Push {:Backend  :Test.Sample.Library.Ledger/CheckoutBook.Backend
                          :Receiver :Test.Sample.Library.Ledger/CheckoutBook.Receiver
                          :Subject  :Test.Sample.Library.Ledger/CheckoutBook.Subject
                          :Text     :Test.Sample.Library.Ledger/CheckoutBook.Text}}
          #_{:Sms/Push {:To   :Test.Sample.Library.Ledger/CheckoutBook.To
                        :Body :Test.Sample.Library.Ledger/CheckoutBook.Text}}
          {:Test.Sample.Library.Ledger/CheckoutLog {:Member? :Test.Sample.Library.Ledger/CheckoutBook.Member
                                               :Book?   :Test.Sample.Library.Ledger/CheckoutBook.Book}})

(event {:Test.Sample.Library.Ledger/CheckinBook
        {:Member   :Email
         :Designation :String
         :Book     :String
         :Backend  :String
         :Receiver :String
         :Subject  :String
         :Text     :String
         :To       :String}})

(dataflow :Test.Sample.Library.Ledger/CheckinBook
          {:Test.Sample.Library.Ledger/CheckoutLog {:Member       :Test.Sample.Library.Ledger/CheckinBook.Member
                                               :Designation   :Test.Sample.Library.Ledger/CheckinBook.Designation
                                               :Book         :Test.Sample.Library.Ledger/CheckinBook.Book
                                               :IsCheckedout false}}
          #_{:Email/Push {:Backend  :Test.Sample.Library.Ledger/CheckoutBook.Backend
                        :Receiver :Test.Sample.Library.Ledger/CheckoutBook.Receiver
                        :Subject  :Test.Sample.Library.Ledger/CheckoutBook.Subject
                        :Text     :Test.Sample.Library.Ledger/CheckoutBook.Text}}
          #_{:Sms/Push {:To   :Test.Sample.Library.Ledger/CheckoutBook.To
                      :Body :Test.Sample.Library.Ledger/CheckoutBook.Text}}
          {:CheckoutLog {:Member? :Test.Sample.Library.Ledger/CheckinBook.Member
                         :Book?   :Test.Sample.Library.Ledger/CheckinBook.Book}})

(event {:CheckedoutBooks
        {:Member :UUID}})

(dataflow :CheckedoutBooks
          {:CheckoutLog {:Member? :Test.Sample.Library.Ledger/CheckedoutBooks.Member}})

(event {:CheckedoutBy
        {:Book :UUID}})

(dataflow :CheckedoutBy
          {:CheckoutLog {:Book? :Test.Sample.Library.Ledger/CheckedoutBy.Book}}
          [:match :CheckoutLog.IsCheckedout
           true :CheckoutLog])

(event {:AllCheckouts {}})

(dataflow :AllCheckouts
          :CheckoutLog?)

(dataflow :ServicePolicy
          {:Agentlang.Kernel.Lang/Policy
           {:Intercept "RBAC"
            :Resource  ["Test.Sample.Library.Identity/Upsert_Member"]
            :Spec      [:q#
                        [:when
                         [:in ["life" "individual" "family"
                               "remote" "oversees" "supported" "associate"]
                          :EventContext.Auth.Owner.Designation]]]}})

(dataflow :UserCreationPolicy
          {:Agentlang.Kernel.Lang/Policy
           {:Intercept "RBAC"
            :Resource  ["Test.Sample.Library.Identity/Upsert_User"]
            :Spec      [:q#
                        [:when
                         [:= "incharge" :EventContext.Auth.Owner.Designation]]]}})

(dataflow :CheckoutPolicy
          {:Agentlang.Kernel.Lang/Policy
           {:Intercept "RBAC"
            :Resource  ["Test.Sample.Library.Ledger/CheckoutBook"]
            :Spec      [:q#
                        [:when
                         [:in ["life" "individual" "family"
                               "remote" "oversees" "supported" "associate"]
                          :EventContext.Auth.Owner.Designation]]]}})

(dataflow :CheckinPolicy
          {:Agentlang.Kernel.Lang/Policy
           {:Intercept "RBAC"
            :Resource  ["Test.Sample.Library.Ledger/CheckinBook"]
            :Spec      [:q#
                        [:when
                         [:in ["life" "individual" "family"
                               "remote" "oversees" "supported" "associate"]
                          :EventContext.Auth.Owner.Designation]]]}})

(dataflow :RBACPolicyLogging
          {:Agentlang.Kernel.Lang/Policy
           {:Intercept "Logging"
            :Resource  ["Test.Sample.Library.Ledger/CheckoutBook"
                        "Test.Sample.Library.Ledger/CheckinBook"
                        "Test.Sample.Library.Identity/Upsert_Member"
                        "Test.Sample.Library.Identity/Upsert_User"]
            :Spec      [:q# {:Disable        :INFO
                             :PagerThreshold {:WARN  {:count            5
                                                      :duration-minutes 10}
                                              :ERROR {:count            3
                                                      :duration-minutes 5}}}]}})


