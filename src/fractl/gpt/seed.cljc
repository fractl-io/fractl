(ns fractl.gpt.seed)

(def full-conversation
  [{:role "system" :content "You are the fractl assistant."}
   {:role "user", :content "Let's create an e-commerce application similar to Etsy. Name it Marketplace."}
   {:role "assistant", :content
    "(component :Marketplace.Core)

(def phone-number? (partial re-matches #\"^(\\+\\d{1,2}\\s)?\\(?\\d{3}\\)?[\\s.-]\\d{3}[\\s.-]\\d{4}$\"))
;; examples of phone-number? matches: 123-456-7890, (123) 456-7890, 123 456 7890, 123.456.7890, +91 (123) 456-7890

(record :Marketplace.Core/Person
  {:Email {:type :Email :guid true} ; guid means globally-unique-identity
   :FullName :String
   :Address :String
   :PhoneNumber {:check phone-number?}})

(entity :Marketplace.Core/Buyer
  {:meta {:inherits :Marketplace.Core/Person}})

(entity :Marketplace.Core/Seller
  {:meta {:inherits :Marketplace.Core/Person}})

(component :Marketplace.Product
  {:refer [:Marketplace.Core]})

(entity :Marketplace.Product/Item
  {:SerialNumber {:type :String :guid true} ; :guid is globally unique
   :Name {:type :String :id true} ; :id is unique under the parent - :Marketplace.Core/Seller
   :Description :String
   :Price :Decimal
   :Category {:type :String :default \"NA\"}
   :QuantityAvailable :Int
   :Images {:setof :String}
   :CreationDate :Now ; :Now is :DateTime that defaults to the current date and time.
   :ActiveStatus {:type :Boolean :default true}})

(relationship :Marketplace.Product/Listing
  {:meta {:contains [:Marketplace.Core/Seller :Marketplace.Product/Item]}})

(defn rating? [r]
  (and (int? r) (<= 0 r 5)))

(entity :Marketplace.Product/Review
  {:Id :Identity
   :Reviewer {:type :Email :id true}
   :Rating {:check rating?}
   :Comment {:type :String :optional true}
   :Date :Now})

(relationship :Marketplace.Product/BuyerReview
  {:meta {:between [:Marketplace.Core/Buyer :Marketplace.Product/Review]}})

(relationship :Marketplace.Product/ItemReviews
  {:meta {:contains [:Marketplace.Product/Item :Marketplace.Product/Review]}})

(component :Marketplace.Sales
 {:refer [:Marketplace.Core :Marketplace.Product]})

(entity :Marketplace.Sales/Order
  {:Id :Identity
   :SerialNumber {:type :Int :id true}
   :Quantity :Int
   :TotalPrice :Decimal
   :OrderDate :Now
   :Status {:oneof [\"pending\" \"processing\" \"fulfilled\"]}})

(relationship :Marketplace.Sales/BuyerOrders
  {:meta {:contains [:Marketplace.Core/Buyer :Marketplace.Sales/Order]}})

(relationship :Marketplace.Sales/ItemOrders
  {:meta {:between [:Marketplace.Product/Item :Marketplace.Sales/Order]}})"}
   {:role "user" :content "Looks good. Now please create the data-model for social-networking platform"}
   {:role "assistant" :content
    "(component :SocialNetwork.Core)

(entity :SocialNetwork.Core/Profile
  {:Email {:type :Email :guid true}
   :Name :String
   :Bio :String
   :ProfilePicture :String
   :Location :String
   :Birthdate :Date
   :JoinDate :Now})

(entity :SocialNetwork.Core/Post
  {:Id {:type :UUID
        :default fractl.util/uuid-string
        :id true}
   :Content :String
   :Images {:setof :String}
   :Created :Now
   :Likes :Int})

(relationship :SocialNetwork.Core/Posts
  {:meta {:contains [:SocialNetwork.Core/Profile :SocialNetwork.Core/Post]}})

(relationship :SocialNetwork.Core/Friends
  {:meta {:between [:SocialNetwork.Core/Profile :SocialNetwork.Core/Profile :as [:From :To]]}
   :FriendsSince :Now})"}])
