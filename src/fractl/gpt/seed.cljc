(ns fractl.gpt.seed)

(def full-conversation
  [{:role "system" :content "You are the fractl assistant."}
   {:role "user", :content "Let's create an e-commerce application similar to Etsy. Name it Marketplace."}
   {:role "assistant", :content
    "(component :Marketplace.Core)

(record :Marketplace.Core/Person
  {:Email {:type :Email :guid true} ; guid means globally-unique-identity
   :FullName :String
   :Address :String
   :PhoneNumber :String})

(entity :Marketplace.Core/Buyer
  {:meta {:inherits :Marketplace.Core/Person}})

(entity :Marketplace.Core/Seller
  {:meta {:inherits :Marketplace.Core/Person}})

(entity :Marketplace.Core/Item
  {:SerialNumber {:type :String :guid true}
   :Name {:type :String :id true}
   :Description :String
   :Price :Decimal
   :Category {:type :String :default \"NA\"}
   :QuantityAvailable :Int
   :Images {:setof :String}
   :CreationDate :Now
   :ActiveStatus {:type :Boolean :default true}})

(relationship :Marketplace.Core/Listing
  {:meta {:contains [:Marketplace.Core/Seller :Marketplace.Core/Item]}})

(entity :Marketplace.Core/Review
  {:Id :Identity ; :Identity is shortcut for {:type :UUID :default fractl.util/uuid-string :guid true}
   :Reviewer {:type :Email :id true}
   :Rating :Int
   :Comment {:type :String :optional true}
   :Date :Now})

(relationship :Marketplace.Core/BuyerReview
  {:meta {:between [:Marketplace.Core/Buyer :Marketplace.Core/Review]}})

(entity :Marketplace.Core/Order
  {:Id :Identity
   :SerialNumber {:type :Int :id true}
   :Quantity :Int
   :TotalPrice :Decimal
   :OrderDate :Now
   :Status {:oneof [\"pending\" \"processing\" \"fulfilled\"]}})

(relationship :Marketplace.Core/ItemOrders
  {:meta {:between [:Marketplace.Core/Item :Marketplace.Core/Order]}})"}
   {:role "user" :content "Looks good. Now please create the data-model for social-networking platform"}
   {:role "assistant" :content
    "(component :SocialNetwork.Core)

(entity :SocialNetwork.Core/Profile
  {:Email {:type :Email :guid true}
   :PhoneNumber :String
   :Name :String
   :Bio :String
   :ProfilePicture :String
   :Location :String
   :Birthdate :Date
   :JoinDate :Now})

(entity :SocialNetwork.Core/Post
  {:Name {:type :String
          :id true} ; :id is unique in the context of a contains-parent, in this case :SocialNetwork.Core/Profile
   :Content :String
   :Images {:setof :String}
   :Created :Now
   :Likes :Int})

(relationship :SocialNetwork.Core/Posts
  {:meta {:contains [:SocialNetwork.Core/Profile :SocialNetwork.Core/Post]}})

(relationship :SocialNetwork.Core/Friends
  {:meta {:between [:SocialNetwork.Core/Profile :SocialNetwork.Core/Profile :as [:From :To]]}
   :FriendsSince :Now})"}
   {:role "user" :content "Good. Please generate a small library management application."}
   {:role "assistant" :content
    "(component :Library.Core)

(entity :Library.Core/Book
  {:Id :Identity
   :Title :String
   :Author :String
   :Genre :String
   :PublicationDate :Date
   :Availability :Boolean})

(entity :Library.Core/Member
  {:Id :Identity
   :Name :String
   :Email {:type :Email :guid true}
   :PhoneNumber :String
   :Address :String
   :JoinDate :Now})

(entity :Library.Core/Loan
  {:Id {:type :UUID
        :default fractl.util/uuid-string
        :id true}
   :BookId {:ref :Library.Core/Book.Id}
   :MemberId {:ref :Library.Core/Member.Id}
   :LoanDate :Now
   :ReturnDate :DateTime
   :Returned :Boolean})

(relationship :Library.Core/Loans
  {:meta {:contains [:Library.Core/Book]}
   :LoanDate :Npw})"}
   {:role "user" :content "Error in `:ref`. Instead of `:ref`, use relationships. Error in `relationship :Library.Core/Loans` - both `:contains` and `:between` relationships must have two entity-nodes. Also attributes like :LoanDate is not allowed for `:contains` relationships. Attributes are allowed only for `:between` relationships."}
   {:role "assistant" :content
    "(component :Library.Core)

(entity :Library.Core/Book
  {:Id :Identity
   :Title :String
   :Author :String
   :Genre :String
   :PublicationDate :Date
   :Availability :Boolean})

(entity :Library.Core/Member
  {:Id :Identity
   :Name :String
   :Email {:type :Email :guid true}
   :PhoneNumber :String
   :Address :String
   :JoinDate :Now})

(relationship
 :Library.Core/Loan
 {:meta {:between [:Library.Core/Member :Library.Core/Book]}
  :LoanDate :Now
  :ReturnDate :DateTime
  :Returned :Boolean})"}])
