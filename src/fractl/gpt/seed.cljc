(ns fractl.gpt.seed)

(def conversation
  [{:role "system" :content "You are the fractl assistant."},
   {:role "user" :content "Model a blog application."},
   {:role "assistant" :content "(component :Blog.Core) (entity :Post {:Title :String :Content :String :CreatedBy :String :CreatedOn :String}) (entity :Comment {:Title :String :Content :String :CreatedBy :String :CreatedOn :String})"},
   {:role "user" :content "As :Post and :Comment share a common structure, they can be inherited from a common record"},
   {:role "assistant" :content "(record :BlogEntry {:Title :String :Content :String :CreatedBy :String :CreatedOn :String}) (entity :Post {:meta {:inherits :BlogEntry}}) (entity :Comment {:meta {:inherits :BlogEntry}})"},
   {:role "user" :content "Good! it would help to make :CreatedBy an email attribute and :CreatedOn to default to the current date-time"},
   {:role "assistant" :content "(record :BlogEntry {:Title :String :Content :String :CreatedBy :Email :CreatedOn :Now})"},
   {:role "user" :content "Can we add a unique identity for :BlogEntry and also make it searchable by :Title and :CreatedBy?"},
   {:role "assistant" :content "(record :BlogEntry {:Title {:type :String :indexed true} :Content :String :CreatedBy {:type :Email :indexed true} :CreatedOn :Now})"},
   {:role "user" :content "Perfect! can the :Comments be contained under :Posts?"},
   {:role "assistant" :content "(relationship :CommentOf {:meta {:contains [:Post :Comment]}})"},
   {:role "user" :content "How can I lookup-up :Posts created by a particular author?"},
   {:role "assistant" :content "(dataflow :PostsBy {:Post {:CreatedBy? :PostsBy.Author}})"}
   {:role "user" :content "Can you generate an accounting application?"}
   {:role "assistant" :content "(component :Accounts.Core {:clj-import '[(:require [clojure.string :as s])]}) (attribute :UniqueName {:type :String :identity true}) (defn- image-url? [s] (and (string? s) true)) (def lower-case s/lower-case) (attribute :Image {:check image-url?}) (entity :Company {:Name :UniqueName :Logo :Image}) (dataflow :CreateCompany {:Company {:Name '(lower-case :CreateCompany.Instance.Name) :Logo :CreateCompany.Instance.Logo}}) (entity :AccountHead :Name :UniqueName :Type {:oneof [:income :expense] :default :expense}}) (relationship :CompanyAccount {:meta {:contains [:Company :AccountHead]}}) (relationship :Transaction {:meta {:between [:AccountHead :AccountHead :as [:Debit :Credit] :cascade-on-delete true]} :Amount :Decimal :Date :Now :Comments {:type :String :optional true}}) (event :TransactionReport {:StartDate :DateTime :EndDate :DateTime}) (dataflow :TransactionReport {:Transaction? {:where [:and [:>= :Date :TransactionReport.StartDate] [:<= :Date :TransactionReport.EndDate]]}})"}
   {:role "user" :content "That's great. I see you defined a custom :CreateCompany dataflow because a user-defined transofrmation is required for the :Name attribute. Do you know what built-in types are allowed for attributes?"}
   {:role "assistant" :content "Yes. :String, :Keyword, :Path, :DateTime, :Date, :Time, :UUID, :Int, :Int64, :BigInteger, :Float, :Double, :Decimal, :Boolean, :Record, :Entity, :Event, :Any, :Email, :Password, :Map, :Edn, :Identity, :Now. :Identity can be used for auto-generated UUID values."}
   {:role "user" :content "I think you can generate fairly complex models now"}
   {:role "assistant" :content "Yes"}])
