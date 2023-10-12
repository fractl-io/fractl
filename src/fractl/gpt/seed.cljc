(ns fractl.gpt.seed)

(def simple-conversation
  [{:role "system" :content "You are the fractl assistant."},
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
   {:role "assistant" :content "(component :School.Core) (entity :School.Core/Student {:Id :Identity, :Name :String :Class :String, :DateOfBirth :DateTime}) (entity :School.Core/Teacher {:Id :Identity, :Name :String, :DateOfBirth :DateTime, :Qualification {:oneof [\"graduate\" \"phd\"]}}) (entity :School.Core/Course {:Id :Identity, :Name {:type :String :unique true}, :Start :Date, :End :Date}) (relationship :School.Core/TeachingAssignment {:meta {:between [:School.Core/Course :School.Core/Teacher]}}) (relationship :School.Core/Enrollement {:meta {:between [:School.Core/Course :School.Core/Student]}})"}
   {:role "user" :content "Excellent! Let's model a library for the school. Define an entity for Book."}
   {:role "assistant" :content "(entity :School.Library/Book {:Isbn {:type :String :identity true}, :Title :String :Author :String})"}
   {:role "user" :content "Can you create an entity for a Library?"}
   {:role "assistant" :content "(entity :School.Library/Library {:Name {:type :String :identity true}})"}
   {:role "user" :content "How can ensure that a Book belongs to a Library?"}
   {:role "assistant" :content "(relationship :School.Library/Listing {:meta {:contains [:School.Library/Library :Book]}})"}
   {:role "user" :content "How can a student become a member of a Library?"}
   {:role "assistant" :content "(relationship :School.Library/Membership {:meta {:between [:School.Library/Library :School.Core/Student]}, :DateOfJoining {:type :DateTime, :default fractl.util.datetime/now}})"}
   {:role "user" :content "Can we have a shorter specification for the :DateOfJoining attribute?"}
   {:role "assistant" :content "(relationship :School.Library/Membership {:meta {:between [:School.Library/Library :School.Core/Student]}, :DateOfJoining :Now})"}
   {:role "user" :content "Great! How can we model the book-checkout process?"}
   {:role "assistant" :content "(relationship :School.Library/Checkout {:meta {:between [:School.Core/Student :School.Library/Book :one-one true]}, :DateOfCheckout :Now})"}
   {:role "user" :content "Fantastic! Don't you think we have reasonably good Library `component` for a School application now?"}
   {:role "assistant" :content "(component :School.Library)(entity :School.Library/Book {:Isbn {:type :String :identity true}, :Title :String :Author :String})(entity :School.Library/Library {:Name {:type :String :identity true}})(relationship :School.Library/Listing {:meta {:contains [:School.Library/Library :School.Library/Book]}})(relationship :School.Library/Membership {:meta {:between [:School.Library/Library :School.Core/Student]}, :DateOfJoining :Now})(relationship :School.Library/Checkout {:meta {:between [:School.Core/Student :School.Library/Book :one-one true]}, :DateOfCheckout :Now})"}
   {:role "user" :content "I have the following accounting component: (component :School.Accounting)(entity :School.Accounting/AccountHead {:Name {:type :String :identity true}}). Can you please show me how transactions may be represented?"}
   {:role "assistant" :content "(relationship :School.Accounting/Transaction {:meta {:between [:School.Accounting/AccountHead :School.Accounting/AccountHead :as [:Debit :Credit]]} :Date :Now :Amount :Decimal})"}
   {:role "user" :content "Please make sure :Amount is always a positive number. You can use Clojure to implement your predicate."}
   {:role "assistant" :content "(relationship :School.Accounting/Transaction {:meta {:between [:School.Accounting/AccountHead :School.Accounting/AccountHead :as [:Debit :Credit]]} :Date :Now :Amount {:type :Decimal :check pos?}})"}
   {:role "user" :content "Define a Person entity with detailed contact information. The component is :Contact"}
   {:role "assistant" :content "(defn street? [s] (and (string? s) (< 0 (count s) 100))) (defn city? \"TODO: add more cities to the set.\" [s] (some #{s} #{\"NY\" \"BOST\" \"SJS\"})) (defn state? \"TODO: add more states to the set.\" [s] (some #{s} #{\"NY\" \"MA\" \"CAL\"})) (record :Contact/Address {:Type {:oneof [\"work\", \"home\"]}, :Street1 {:check street?}, :Street1 {:check street?, :optional true}, :City {:check city?}, :State {:check state?}, :Zip {:format \"^[0-9]{5}(?:-[0-9]{4})?$\"}}) (record :Contact/PhoneNumber {:Type {:oneof [\"work\", \"home\"]}, :Number {:format \"^(1\\s?)?(\\d{3}|\\(\\d{3}\\))[\\s\\-]?\\d{3}[\\s\\-]?\\d{4}$\"}}) (entity :Contact/Person {:FirstName :String, :LastName :String, :Addresses {:listof :Contact/Address}, :PhoneNumbers {:listof :Contact/PhoneNumber}, :Email :Email})"}
   {:role "user" :content "Create a blog application."}
   {:role "assistant" :content "(component :Blog) (entity :Blog/User {:Id :Identity, :Username {:type :String :unique true}, :Email :Email}) (entity :Blog/Post {:Id :Identity, :Title :String, :Content :String, :DatePublished :DateTime, :Author {:ref :Blog/User}}) (relationship :Blog/Comment {:meta {:between [:Blog/Post :Blog/User :one-n true]}, :Content :String, :DatePosted :DateTime})"}
   {:role "user" :content "There are three problems - (1) :ref should point to the identity attribute. (2) It's idiomatic to name the main component as :Core. (3) A :User can have `n` :Comments on the same :Post."}
   {:role "assistant" :content "(component :Blog.Core) (entity :Blog.Core/User {:Id :Identity, :Username {:type :String :unique true}, :Email :Email}) (entity :Blog.Core/Post {:Id :Identity, :Title :String, :Content :String, :DatePublished :DateTime, :Author {:ref :Blog.Core/User.Id}}) (relationship :Blog.Core/Comment {:meta {:between [:Blog.Core/Post :Blog.Core/User :one-n false]}, :Content :String, :DatePosted :DateTime})"}
   {:role "user" :content "Good. Can we have the current-datetime as default for the `:DateTime` attributes? Also note that `:one-n` is `false` by default."}
   {:role "assistant" :content "(component :Blog.Core) (entity :Blog.Core/User {:Id :Identity, :Username {:type :String :unique true}, :Email :Email}) (entity :Blog.Core/Post {:Id :Identity, :Title :String, :Content :String, :DatePublished :Now, :Author {:ref :Blog.Core/User.Id}}) (relationship :Blog.Core/Comment {:meta {:between [:Blog.Core/Post :Blog.Core/User]}, :Content :Text, :DatePosted :Now})"}
   {:role "user" :content "Fine. While :ref is OK, the preferred method is to define relationships."}
   {:role "assistant" :content "(component :Blog.Core) (entity :Blog.Core/User {:Id :Identity, :Username {:type :String :unique true}, :Email :Email}) (entity :Blog.Core/Post {:Id :Identity, :Title :String, :Content :String, :DatePublished :Now}) (entity :Blog.Core/Posts {:meta {:contains [:Blog.Core/User :Blog.Core/Post]}}) (relationship :Blog.Core/Comment {:meta {:between [:Blog.Core/Post :Blog.Core/User]}, :Content :Text, :DatePosted :Now})"}
   {:role "user" :content "As a final example can you generate a shopping application?"}
   {:role "assistant" :content "(component :Shopping.Core) (entity :Shopping.Core/User {:Id :Identity, :Username {:type :String :unique true}, :Email :Email}) (entity :Shopping.Core/Product {:Id :Identity, :Name :String, :Description :String, :Price :Decimal}) (entity :Shopping.Core/Order {:Id {:type :Integer :identity true}, :User {:ref :Shopping.Core/User.Id}, :Products {:listof :Shopping.Core/Product}, :TotalAmount :Decimal, :OrderDate :Now})"}
   {:role "user" :content "There's an error - there is no type called :Integer, use :Int instead. Also model using relationships, no :refs please. As :Product is an entity, it will be persisted twice because :Order has a :listof :Product."}
   {:role "assistant" :content "(component :Shopping.Core) (entity :Shopping.Core/User {:Id :Identity, :Username {:type :String :unique true}, :Email :Email}) (entity :Shopping.Core/Product {:Id :Identity, :Name :String, :Description :String, :Price :Decimal}) (entity :Shopping.Core/Order {:Id {:type :Int :identity true} :TotalAmount :Decimal, :OrderDate :Now}) (relationship :Shopping.Core/Orders {:meta {:contains [:Shopping.Core/User :Shopping.Core/Order]}}) (relationship :Shopping.Core/OrderItem {:meta {:between [:Shopping.Core/Product :Shopping.Core/Order]} :Quantity :Int})"}])

(def conversation
  [{:role "system" :content "You are the fractl assistant."},
   {:role "user" :content "Model a blog application."},
   {:role "assistant" :content "(component :Blog.Core) (entity :Blog.Core/Post {:Title :String :Content :String :CreatedBy :String :CreatedOn :String}) (entity :Blog.Core/Comment {:Title :String :Content :String :CreatedBy :String :CreatedOn :String})"},
   {:role "user" :content "As :Post and :Comment share a common structure, they can be inherited from a common record"},
   {:role "assistant" :content "(record :Blog.Core/BlogEntry {:Title :String :Content :String :CreatedBy :String :CreatedOn :String}) (entity :Blog.Core/Post {:meta {:inherits :Blog.Core/BlogEntry}}) (entity :Blog.Core/Comment {:meta {:inherits :Blog.Core/BlogEntry}})"},
   {:role "user" :content "Good! it would help to make :CreatedBy an email attribute and :CreatedOn to default to the current date-time"},
   {:role "assistant" :content "(record :Blog.Core/BlogEntry {:Title :String :Content :String :CreatedBy :Email :CreatedOn :Now})"},
   {:role "user" :content "Can we add a unique identity for :BlogEntry and also make it searchable by :Title and :CreatedBy?"},
   {:role "assistant" :content "(record :Blog.Core/BlogEntry {:Title {:type :String :indexed true} :Content :String :CreatedBy {:type :Email :indexed true} :CreatedOn :Now})"},
   {:role "user" :content "Perfect! can the :Comments be contained under :Posts?"},
   {:role "assistant" :content "(relationship :Blog.Core/CommentOf {:meta {:contains [:Blog.Core/Post :Blog.Core/Comment]}})"},
   {:role "user" :content "How can I lookup-up :Posts created by a particular author?"},
   {:role "assistant" :content "(dataflow :Blog.Core/PostsBy {:Blog.Core/Post {:CreatedBy? :Blog.Core/PostsBy.Author}})"}
   {:role "user" :content "Can you generate an accounting application?"}
   {:role "assistant" :content "(component :Accounts.Core {:clj-import '[(:require [clojure.string :as s])]}) (attribute :Accounts.Core/UniqueName {:type :String :identity true}) (defn- image-url? [s] (and (string? s) true)) (def lower-case s/lower-case) (attribute :Accounts.Core/Image {:check image-url?}) (entity :Accounts.Core/Company {:Name :Accounts.Core/UniqueName :Logo :Accounts.Core/Image}) (dataflow :Accounts.Core/CreateCompany {:Accounts.Core/Company {:Name '(lower-case :Accounts.Core/CreateCompany.Instance.Name) :Logo :Accounts.Core/CreateCompany.Instance.Logo}}) (entity :Accounts.Core/AccountHead :Name :Accounts.Core/UniqueName :Type {:oneof [:income :expense] :default :expense}}) (relationship :Accounts.Core/CompanyAccount {:meta {:contains [:Accounts.Core/Company :Accounts.Core/AccountHead]}}) (relationship :Accounts.Core/Transaction {:meta {:between [:Accounts.Core/AccountHead :Accounts.Core/AccountHead :as [:Debit :Credit] :cascade-on-delete true]} :Amount :Decimal :Date :Now :Comments {:type :String :optional true}}) (event :Accounts.Core/TransactionReport {:StartDate :DateTime :EndDate :DateTime}) (dataflow :Accounts.Core/TransactionReport {:Accounts.Core/Transaction? {:where [:and [:>= :Date :Accounts.Core/TransactionReport.StartDate] [:<= :Date :Accounts.Core/TransactionReport.EndDate]]}})"}
   {:role "user" :content "That's great. I see you defined a custom :CreateCompany dataflow because a user-defined transformation is required for the :Name attribute. Do you know what built-in types are allowed for attributes?"}
   {:role "assistant" :content "Yes. :String, :Keyword, :Path, :DateTime, :Date, :Time, :UUID, :Int, :Int64, :BigInteger, :Float, :Double, :Decimal, :Boolean, :Record, :Entity, :Event, :Any, :Email, :Password, :Map, :Edn, :Identity, :Now. :Identity can be used for auto-generated UUID values."}
   {:role "user" :content "Example of an attribute that's a sequence of another type."}
   {:role "assistant" :content "(attribute :Acme/StreetAddress {:listof :String})"}
   {:role "user" :content "Example of an entity with an attribute that's an enumeration."}
   {:role "assistant" :content "(entity :Acme/Person {:Name :String :Age :Int :Sex {:oneof [\"male\" \"female\"]}})"}
   {:role "user" :content "Can you extend the :Person entity with an optional address attribute?"}
   {:role "assistant" :content "(entity :Acme/Person {:Name :String :Address {:type :Acme/StreetAddress :optional true} :Age :Int :Sex {:oneof [\"male\" \"female\"]}})"}
   {:role "user" :content "It's better to always have an identity attribute for an entity"}
   {:role "assistant" :content "(entity :Acme/Person {:Id :Identity :Name :String :Address {:type :Acme/StreetAddress :optional true} :Age :Int :Sex {:oneof [\"male\" \"female\"]}})"}
   {:role "user" :content "Good! Can you think of an alternative?"}
   {:role "assistant" :content "(entity :Acme/Person {:Name {:type :String :identity true} :Address {:type :Acme/StreetAddress :optional true} :Age :Int :Sex {:oneof [\"male\" \"female\"]}})"}
   {:role "user" :content "That's fine. Given the entity (entity :Acme/Sales {:Date :DateTime :Price :Decimal}), write a dataflow the total and average prices of sales after a particular date"}
   {:role "assistant" :content "(dataflow :Acme/SalesSummary {:Acme/Sales? {:where [:> :Date :Acme/SalesSummary.Date] :sum :Price :avg :Price}})"}
   {:role "user" :content "Show me a sample invocation of :SalesSummary"}
   {:role "assistant" :content "{:Acme/SalesSummary {:Date \"2023-06-04T10:56:46.41097409\"}}"}
   {:role "user" :content "How to find the maximum and minumum price?"}
   {:role "assistant" :content "(dataflow :Acme/SalesRange {:Acme/Sales? {:where [:> :Date :Acme/SalesRange.Date] :max :Price :min :Price}})"}
   {:role "user" :content "Report the count of sales?"}
   {:role "assistant" :content "(dataflow :Acme/SalesCount {:Acme/Sales? {:where [:> :Date :Acme/SalesCount.Date] :count :Price}})"}
   {:role "user" :content "Show all sales below or equal-to the amount 1500, ordered by date"}
   {:role "assistant" :content "(dataflow :Acme/SalesBelowOrEqual1500 {:Acme/Sales? {:where [:<= :Price 1500] :order-by [:Date]}})"}
   {:role "user" :content "How to query the same results in descending order?"}
   {:role "assistant" :content "(dataflow :Acme/SalesBelowOrEqual1500Desc {:Acme/Sales? {:where [:<= :Price 1500] :order-by [[:Date :desc]]}})"}
   {:role "user" :content "How to list all sales ordered-by :Price"}
   {:role "assistant" :content "(dataflow :Acme/SalesByPrice {:Acme/Sales? {:order-by [:Price]}})"}
   {:role "user" :content "How will I execute this dataflow?"}
   {:role "assistant" :content "{:Acme/SalesByPrice {}}"}
   {:role "user" :content "How to list all sales ordered-by ratings, top ratings first?"}
   {:role "assistant" :content "(dataflow :Acme/SalesByRating {:Acme/Sales? {:order-by [:Rating :desc]}})"}
   {:role "user" :content "There are two errors in this dataflow, :desc has to be enclosed in a vector and there's no attribute called :Rating in :Sales"}
   {:role "assistant" :content "(entity :Acme/Sales {:Date :DateTime :Price :Decimal :Rating :Int}) (dataflow :Acme/SalesByRating {:Acme/Sales? {:order-by [[:Rating :desc]]}})"}
   {:role "user" :content "Great! Show me an example of a dataflow for creating a new instance of an entity from another entity"}
   {:role "assistant" :content "(dataflow :Acme/CreateAFromE {:Acme/E {:Id? :Acme/CreateAFromE.E} :as :E} {:Acme/A {:X :E.V :Y 100}})"}
   {:role "user" :content "What if more than one instance needs to be queried and created?"}
   {:role "assistant" :content "(dataflow :Example/CreateAsFromEs {:Example/E {:V? [:>= :Example/CreateAsFromEs.V]} :as :Es} [:for-each :Es {:Example/A {:X :%.V :Y 100}}]))"}
   {:role "user" :content "Show me a sample invocation of the event :CreateAFromE"}
   {:role "assistant" :content "{:Example/CreateAFromE {:E 3}}"}
   {:role "user" :content "Show me a sample invocation of the event :CreateAsFromEs"}
   {:role "assistant" :content "{:Example/CreateAsFromEs {:V 200}}"}
   {:role "user" :content "Good! From the :CreateAFromE and :CreateAsFromEs and their sample invocations, can you derive the data-model for the :Example component?"}
   {:role "assistant" :content "(component :Example) (entity :Example/E {:Id {:type :Int :identity true} :V {:type :Int :indexed true}}) (record :Example/A {:X :Int :Y :Int}) (event :Example/CreateAFromE {:E :Int}) (event :Example/CreateAsFromEs {:V :Int})"}])
