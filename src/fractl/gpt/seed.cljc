(ns fractl.gpt.seed)

(def full-conversation
  [{:role "system" :content "You are the fractl assistant."}
   {:role "user", :content "Let's create an e-commerce application similar to Etsy called Marketplace."}
   {:role "assistant", :content
    "(model :Marketplace)

; User component
(component :Marketplace.User)

; Buyer entity
(entity :Marketplace.User/Buyer
  {:Id {:type :UUID
        :unique true}
   :Username :String
   :Email :Email
   :Password :String
   :FullName :String
   :Address :String
   :PhoneNumber :String})

; Seller entity
(entity :Marketplace.User/Seller
  {:Id {:type :UUID
        :unique true}
   :Username :String
   :Email :Email
   :Password :String
   :FullName :String
   :Address :String
   :PhoneNumber :String})

; Product component
(component :Marketplace.Product)

; Product Listing entity
(entity :Marketplace.Product/Listing
  {:Id {:type :UUID
        :unique true}
   :SellerId {:type :UUID}
   :Name :String
   :Description :String
   :Price :Decimal
   :Category :String
   :QuantityAvailable :Int
   :Images :SetOf :String
   :CreationDate :DateTime
   :ActiveStatus :Boolean
   :SoldQuantity :Int})

; Order component
(component :Marketplace.Order)

; Order Item entity
(entity :Marketplace.Order/Item
  {:Id {:type :UUID
        :unique true}
   :BuyerId {:type :UUID}
   :SellerId {:type :UUID}
   :ProductId {:type :UUID}
   :Quantity :Int
   :TotalPrice :Decimal
   :OrderDate :DateTime
   :Status :String})

; Review entity
(entity :Marketplace.Order/Review
  {:Id {:type :UUID
        :unique true}
   :ProductId {:type :UUID}
   :UserId {:type :UUID}
   :Rating :Int
   :Comment :String
   :Date :DateTime})

; Relationships
(relationship :Marketplace.User/Products
  {:meta {:contains [:Marketplace.User/Buyer :Marketplace.Product/Listing]}})

(relationship :Marketplace.Product/Orders
  {:meta {:contains [:Marketplace.Product/Listing :Marketplace.Order/Item]}})

(relationship :Marketplace.Product/Reviews
  {:meta {:contains [:Marketplace.Product/Listing :Marketplace.Order/Review]}})"}

   {:role "user" :content "Okay you can drop the model and comments from above and also, if you are using contains relationship, make :path-identity mandatory for child entities in :contains relationships."}
   {:role "assistant" :content
    "(component :Marketplace.User)

(entity :Marketplace.User/Buyer
  {:Id {:type :UUID
        :unique true}
   :Username :String
   :Email :Email
   :Password :String
   :FullName :String
   :Address :String
   :PhoneNumber :String})

(entity :Marketplace.User/Seller
  {:Id {:type :UUID
        :unique true}
   :Username :String
   :Email :Email
   :Password :String
   :FullName :String
   :Address :String
   :PhoneNumber :String})

(component :Marketplace.Product)

(entity :Marketplace.Product/Listing
  {:Id {:type :UUID
        :unique true
        :path-identity true}
   :SellerId {:type :UUID}
   :Name :String
   :Description :String
   :Price :Decimal
   :Category :String
   :QuantityAvailable :Int
   :Images :SetOf :String
   :CreationDate :DateTime
   :ActiveStatus :Boolean
   :SoldQuantity :Int})

(component :Marketplace.Order)

(entity :Marketplace.Order/Item
  {:Id {:type :UUID
        :unique true
        :path-identity true}
   :BuyerId {:type :UUID}
   :SellerId {:type :UUID}
   :ProductId {:type :UUID}
   :Quantity :Int
   :TotalPrice :Decimal
   :OrderDate :DateTime
   :Status :String})

(entity :Marketplace.Order/Review
  {:Id {:type :UUID
        :unique true}
   :ProductId {:type :UUID}
   :UserId {:type :UUID}
   :Rating :Int
   :Comment :String
   :Date :DateTime})
  
(relationship :Marketplace.User/Products
  {:meta {:contains [:Marketplace.User/Buyer :Marketplace.Product/Listing]}})

(relationship :Marketplace.Product/Orders
  {:meta {:contains [:Marketplace.Product/Listing :Marketplace.Order/Item]}})

(relationship :Marketplace.Product/Reviews
  {:meta {:contains [:Marketplace.Product/Listing :Marketplace.Order/Review]}})"}

   {:role "user" :content "The :SetOf  is wrong, it should be something like this `{:setof <type>}`"}
   {:role "assistant" :content
    "(component :Marketplace.User)

    (entity :Marketplace.User/Buyer
      {:Id {:type :UUID
            :unique true}
       :Username :String
       :Email :Email
       :Password :String
       :FullName :String
       :Address :String
       :PhoneNumber :String})

    (entity :Marketplace.User/Seller
      {:Id {:type :UUID
            :unique true}
       :Username :String
       :Email :Email
       :Password :String
       :FullName :String
       :Address :String
       :PhoneNumber :String})

   (component :Marketplace.Product)

  (entity :Marketplace.Product/Listing
    {:Id {:type :UUID
          :unique true
          :path-identity true}
     :SellerId {:type :UUID}
     :Name :String
     :Description :String
     :Price :Decimal
     :Category :String
     :QuantityAvailable :Int
     :Images {:setof :String} ; Images is now setof :String
     :CreationDate :DateTime
     :ActiveStatus :Boolean
     :SoldQuantity :Int})


  (component :Marketplace.Order)

  (entity :Marketplace.Order/Item
    {:Id {:type :UUID
          :unique true
          :path-identity true}
     :BuyerId {:type :UUID}
     :SellerId {:type :UUID}
     :ProductId {:type :UUID}
     :Quantity :Int
     :TotalPrice :Decimal
     :OrderDate :DateTime
     :Status :String})

  (entity :Marketplace.Order/Review
    {:Id {:type :UUID
          :unique true}
     :ProductId {:type :UUID}
     :UserId {:type :UUID}
     :Rating :Int
     :Comment :String
     :Date :DateTime})

  (relationship :Marketplace.User/Products
    {:meta {:contains [:Marketplace.User/Buyer :Marketplace.Product/Listing]}})

  (relationship :Marketplace.Product/Orders
    {:meta {:contains [:Marketplace.Product/Listing :Marketplace.Order/Item]}})

(relationship :Marketplace.Product/Reviews
  {:meta {:contains [:Marketplace.Product/Listing :Marketplace.Order/Review]}})"}
   {:role "user" :content "Create data-model for social network platform"}
   {:role "assistant" :content
    "(component :User)

(entity :SocialNetwork.User/Profile
  {:Id {:type :UUID
        :unique true}
   :Username :String
   :FullName :String
   :Email :Email
   :Password :String
   :Bio :String
   :ProfilePicture :String
   :Location :String
   :Birthdate :Date
   :JoinDate :DateTime})

(entity :SocialNetwork.User/Post
  {:Id {:type :UUID
        :unique true
        :path-identity true}
   :AuthorId {:type :UUID}
   :Content :String
   :Images {:setof :String}
   :Timestamp :DateTime
   :Likes :Int
   :Comments :Int})

(entity :User.Friend
  {:Id {:type :UUID
        :unique true
        :path-identity true}
   :UserId :UUID
   :FriendId :UUID
   :Status :String})

(relationship :SocialNetwork.User/Posts
  {:meta {:contains [:SocialNetwork.User/Profile :SocialNetwork.User/Post]}})

(relationship :SocialNetwork.User/Friends
  {:meta {:contains [:SocialNetwork.User/Profile :SocialNetwork.User/Friend]}})"}
   {:role "user" :content "The above example is wrong, the entity name should always have ModelName.ComponentName/Name-of-entity format and also the component name must contain ModelName.ComponentName format. Additionally, do not use the same component or model name as entity name or relationship name or record name."}
   {:role "assistant" :content
    "(component :SocialNetwork.User)

(entity :SocialNetwork.User/Profile
  {:Id {:type :UUID
        :unique true}
   :Username :String
   :FullName :String
   :Email :Email
   :Password :String
   :Bio :String
   :ProfilePicture :String
   :Location :String
   :Birthdate :Date
   :JoinDate :DateTime})

(entity :SocialNetwork.User/Post
  {:Id {:type :UUID
        :unique true
        :path-identity true}
   :AuthorId {:type :UUID}
   :Content :String
   :Images {:setof :String}
   :Timestamp :DateTime
   :Likes :Int
   :Comments :Int})

(entity :SocialNetwork.User/Friend
  {:Id {:type :UUID
        :unique true
        :path-identity true}
   :UserId :UUID
   :FriendId :UUID
   :Status :String})

(relationship :SocialNetwork.User/Posts
  {:meta {:contains [:SocialNetwork.User/Profile :SocialNetwork.User/Post]}})

(relationship :SocialNetwork.User/Friends
  {:meta {:contains [:SocialNetwork.User/Profile :SocialNetwork.User/Friend]}})"}
   {:role "user" :content "Let's model a school-management application. Define an entity to represent students."}
   {:role "assistant" :content "(entity :School.Core/Student {:Id {:type :UUID :identity true}, :Name :Text :Class :Text, :DateOfBirth :DateTime})"}
   {:role "user" :content "There's an error. Textual attributes are always represented by the :String type."}
   {:role "assistant" :content "(entity :School.Core/Student {:Id {:type :UUID :identity true}, :Name :String :Class :String, :DateOfBirth :DateTime})"}
   {:role "user" :content "That looks good. Is a shorter definition possible for the :Id attribute?"}
   {:role "assistant" :content "(entity :School.Core/Student {:Id :Identity, :Name :String :Class :String, :DateOfBirth :DateTime})"}
   {:role "user" :content "Great. Can we have a similar definition for teachers?"}
   {:role "assistant" :content "(entity :School.Core/Teacher {:Id :Identity, :Name :Text, :DateOfBirth :DateTime, :Qualification {:oneof [\"graduate\" \"phd\"]}})"}
   {:role "user" :content "Did you forget? There's no type called :Text. Valid types you can use for attributes are :String, :Keyword, :Path, :DateTime, :Date, :Time, :UUID, :Int, :Int64, :BigInteger, :Float, :Double, :Decimal, :Boolean, :Record, :Entity, :Event, :Any, :Email, :Password, :Map, :Edn, :Identity and :Now. :Identity can be used for auto-generated UUID values. You may also use record names as types."}
   {:role "assistant" :content "(entity :School.Core/Teacher {:Id :Identity, :Name :String, :DateOfBirth :DateTime, :Qualification {:oneof [\"graduate\" \"phd\"]}})"}
   {:role "user" :content "OK. Define an entity for Course."}
   {:role "assistant" :content "(entity :School.Core/Course {:Id :Identity, :Name {:type :String :unique true}, :Start :Date, :End :Date})"}
   {:role "user" :content "We need a way to assign students and teachers to a Course."}
   {:role "assistant" :content "(relationship :School.Core/TeachingAssignment {:meta {:between [:School.Core/Course :School.Core/Teacher]}}) (relationship :School.Core/Enrollement {:meta {:between [:School.Core/Course :School.Core/Student]}})"}
   {:role "user" :content "Can we group all the above definitions in the :School.Core component?"}
   {:role "assistant" :content "(component :School.Core) (entity :School.Core/Student {:Id :Identity, :Name :String :Class :String, :DateOfBirth :DateTime}) (entity :School.Core/Teacher {:Id :Identity, :Name :String, :DateOfBirth :DateTime, :Qualification {:oneof [\"graduate\" \"phd\"]}}) (entity :School.Core/Course {:Id :Identity, :Name {:type :String :unique true}, :Start :Date, :End :Date}) (relationship :School.Core/TeachingAssignment {:meta {:between [:School.Core/Course :School.Core/Teacher]}}) (relationship :School.Core/Enrollement {:meta {:between [:School.Core/Course :School.Core/Student]}})"}])
