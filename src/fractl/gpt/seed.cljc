(ns fractl.gpt.seed)

(def full-conversation
  [{:role "system" :content "You are the fractl assistant."}
   {:role "user", :content "Let's create an e-commerce application similar to Etsy called Marketplace."}
   {:role "assistant", :content
    "(entity :Marketplace.User {:Id :Identity, :Username {:type :String :unique true}, :Email :Email, :Password :Password}) 
    (entity :Marketplace.Shop {:Id :Identity, :Name :String, :Description :Text, :Owner {:ref :Marketplace.User.Id}, :CreationDate :DateTime})
    (entity :Marketplace.Product {:Id :Identity, :Title :String, :Description :Text, :Price :Decimal, :Shop {:ref :Marketplace.Shop.Id}, :Category {:ref :Marketplace.ProductCategory.Id}, :CreatedDate :DateTime})
    (entity :Marketplace.ProductCategory {:Id :Identity, :Name :String, :ParentCategory {:ref :Marketplace.ProductCategory.Id :optional true}})
    (relationship :Marketplace.ProductReview {:meta {:between [:Marketplace.User :Marketplace.Product], :Rating :Int, :Review :Text, :ReviewDate :DateTime}})
    (relationship :Marketplace.ShopMembership {:meta {:between [:Marketplace.User :Marketplace.Shop], :JoinDate :DateTime}})
    (relationship :Marketplace.Order {:meta {:between [:Marketplace.User :Marketplace.Product], :OrderDate :DateTime, :Quantity :Int, :TotalPrice :Decimal}})
    (relationship :Marketplace.Payment {:meta {:between [:Marketplace.Order :Marketplace.User], :PaymentDate :DateTime, :Amount :Decimal}})"}
   {:role "user" :content "In the above data-model you used ref, but, ref is marked invalid and should be replaced by relationship like contains, between"}
   {:role "assistant" :content "I apologize for the oversight. You're correct, in the Fractl data model, the use of :ref is marked as invalid, and relationships like `:contains` and `:between` should be used instead. Here's the corrected data model for the `Marketplace` e-commerce application using only relationships:
    (entity :Marketplace.User {:Id :Identity, :Username {:type :String :unique true}, :Email :Email, :Password :Password}) 
    (entity :Marketplace.Shop {:Id :Identity, :Name :String, :Description :Text, :Owner :Identity, :CreationDate :DateTime})
    (entity :Marketplace.Product {:Id :Identity, :Title :String, :Description :Text, :Price :Decimal, :Shop :Identity, :Category :Identity, :CreatedDate :DateTime})
    (entity :Marketplace.ProductCategory {:Id :Identity, :Name :String, :ParentCategory :Identity :optional true})
    (relationship :Marketplace.ProductReview {:meta {:between [:Marketplace.User :Marketplace.Product], :Rating :Int, :Review :Text, :ReviewDate :DateTime}})
    (relationship :Marketplace.ShopMembership {:meta {:between [:Marketplace.User :Marketplace.Shop], :JoinDate :DateTime}})
    (relationship :Marketplace.Order {:meta {:between [:Marketplace.User :Marketplace.Product], :OrderDate :DateTime, :Quantity :Int, :TotalPrice :Decimal}})
    (relationship :Marketplace.Payment {:meta {:between [:Marketplace.Order :Marketplace.User], :PaymentDate :DateTime, :Amount :Decimal}})"}
   {:role "user" :content "In the above example, you didn't have component and model description"}
   {:role "assistant" :content "My apologies for missing the component and model description. Here's the updated data model for the `MarketPlace` e-commerce application with the missing component and a brief model description
    (component :MarketPlace.Core)

  (entity :MarketPlace.User {:Id :Identity, :Username :String, :Email :Email, :Password :Password})
  (entity :MarketPlace.Product {:Id :Identity, :Title :String, :Description :Text, :Price :Decimal, :Quantity :Int, :Seller :Identity})
  (entity :MarketPlace.Order {:Id :Identity, :User :Identity, :OrderDate :DateTime, :TotalAmount :Decimal})
  (entity :MarketPlace.OrderItem {:Id :Identity, :Order :Identity, :Product :Identity, :Quantity :Int, :Price :Decimal})
  (entity :MarketPlace.ShippingAddress {:Id :Identity, :User :Identity, :Name :String, :Street :String, :City :String, :State :String, :ZipCode :String, :Country :String})
  (entity :MarketPlace.Review {:Id :Identity, :Product :Identity, :User :Identity, :Rating :Int, :Comment :Text})

  (relationship :MarketPlace.UserOrders {:meta {:between [:MarketPlace.User :MarketPlace.Order]}})
  (relationship :MarketPlace.OrderItems {:meta {:between [:MarketPlace.Order :MarketPlace.OrderItem]}})
  (relationship :MarketPlace.ProductReviews {:meta {:between [:MarketPlace.Product :MarketPlace.Review]}})
  (relationship :MarketPlace.UserShippingAddresses {:meta {:between [:MarketPlace.User :MarketPlace.ShippingAddress]}})"}
   {:role "user" :content "Okay, this is fine, but, when generating data-model entity, relationship name, you don't need to give full component name"}
   {:role "assistant" :content "I understand your point. You're correct; it's not necessary to include the full component name when defining entities and relationships in the data model. Here's the updated data model for the MarketPlace e-commerce application without the full component name:
    (component :MarketPlace.Core)

  (entity :User {:Id :Identity, :Username :String, :Email :Email, :Password :Password})
  (entity :Product {:Id :Identity, :Title :String, :Description :Text, :Price :Decimal, :Quantity :Int, :Seller :Identity})
  (entity :Order {:Id :Identity, :User :Identity, :OrderDate :DateTime, :TotalAmount :Decimal})
  (entity :OrderItem {:Id :Identity, :Order :Identity, :Product :Identity, :Quantity :Int, :Price :Decimal})
  (entity :ShippingAddress {:Id :Identity, :User :Identity, :Name :String, :Street :String, :City :String, :State :String, :ZipCode :String, :Country :String})
  (entity :Review {:Id :Identity, :Product :Identity, :User :Identity, :Rating :Int, :Comment :Text})

  (relationship :UserOrders {:meta {:between [:User :Order]}})
  (relationship :OrderItems {:meta {:between [:Order :OrderItem]}})
  (relationship :ProductReviews {:meta {:between [:Product :Review]}})
  (relationship :UserShippingAddresses {:meta {:between [:User :ShippingAddress]}})"}
   {:role "user" :content "Create a full functionality data model for loan app"}
   {:role "assistant" :content "Creating a full functionality data model for a Loan application:
    (component :LoanApp.Core)

; User entity represents the users of the application
(entity :User {:Id :Identity, :Name :String, :Email :Email, :Password :Password})

; LoanApplication entity represents the loan applications submitted by users
(entity :LoanApplication {:Id :Identity, :Applicant :Identity, :LoanAmount :Decimal, :InterestRate :Decimal, :LoanTerm :Int, :Status :String, :ApplicationDate :DateTime})

; LoanOffer entity represents the loan offers made by lenders in response to applications
(entity :LoanOffer {:Id :Identity, :Lender :Identity, :Loan :Identity, :LoanAmount :Decimal, :InterestRate :Decimal, :LoanTerm :Int, :Status :String, :ExpiryDate :DateTime})

; Loan entity represents the loans granted to users
(entity :Loan {:Id :Identity, :Lender :Identity, :Borrower :Identity, :LoanAmount :Decimal, :InterestRate :Decimal, :LoanTerm :Int, :Status :String, :ApprovalDate :DateTime})

; Repayment entity represents individual loan repayments
(entity :Repayment {:Id :Identity, :Loan :Identity, :InstallmentAmount :Decimal, :DueDate :DateTime, :Status :String})

; Define relationships between entities

; User to Loan relationship, representing loans associated with a user
(relationship :UserLoans {:meta {:between [:User :Loan]}})

; User to LoanApplication relationship, representing loan applications submitted by a user
(relationship :UserLoanApplications {:meta {:between [:User :LoanApplication]}})

; LoanApplication to LoanOffer relationship, representing offers made in response to an application
(relationship :LoanApplicationLoanOffers {:meta {:between [:LoanApplication :LoanOffer]}})

; LoanOffer to Loan relationship, representing the loans resulting from accepted offers
(relationship :LoanOfferLoan {:meta {:between [:LoanOffer :Loan]}})

; Loan to Repayment relationship, representing repayments for a loan
(relationship :LoanRepayments {:meta {:between [:Loan :Repayment]}})"}
   {:role "user" :content "Generate above example using contains relationship"}
   {:role "assistant" :content "I apologize for any confusion earlier. Here's the data model for a Loan application using the `:contains` relationship:
(component :LoanApp.Core)

(entity :User {:Id :Identity, :Name :String, :Email :Email, :Password :Password})
(entity :LoanApplication {:Id :Identity, :Applicant :Identity, :LoanAmount :Decimal, :InterestRate :Decimal, :LoanTerm :Int, :Status :String, :ApplicationDate :DateTime})
(entity :LoanOffer {:Id :Identity, :Lender :Identity, :Loan :Identity, :LoanAmount :Decimal, :InterestRate :Decimal, :LoanTerm :Int, :Status :String, :ExpiryDate :DateTime})
(entity :Loan {:Id :Identity, :Lender :Identity, :Borrower :Identity, :LoanAmount :Decimal, :InterestRate :Decimal, :LoanTerm :Int, :Status :String, :ApprovalDate :DateTime})
(entity :Repayment {:Id :Identity, :Loan :Identity, :InstallmentAmount :Decimal, :DueDate :DateTime, :Status :String})

(relationship :UserLoans {:meta {:contains [:User :Loan]}})
(relationship :UserLoanApplications {:meta {:contains [:User :LoanApplication]}})
(relationship :LoanApplicationLoanOffers {:meta {:contains [:LoanApplication :LoanOffer]}})
(relationship :LoanOfferLoan {:meta {:contains [:LoanOffer :Loan]}})
(relationship :LoanRepayments {:meta {:contains [:Loan :Repayment]}})"}])
