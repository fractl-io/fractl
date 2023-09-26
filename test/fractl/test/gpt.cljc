(ns fractl.test.gpt
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.gpt.core :as gpt]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest make-new-resolver
  (gpt/interactive-generate (fn [choices]
                                (let [choice (first choices)]
                                  (println "Assistant's response:" choice)
                                  (if (= choice "Goodbye!")
                                      ;; Terminate the recursive calling by returning nil
                                    nil
                                      ;; Continue the recursive calling by prompting for the next request
                                    (let [next-request (read-line)]
                                      [choice next-request])))) 
                            "Generate a resolver for the following fractl data model using Google Classroom's API:
                             (component :Google.Classroom) 
                             (entity :Google.Classroom/User {:Id {:type :String :identity true} :Name :String :Email :String}) 
                             (entity :Google.Classroom/Course {:Id {:type :String :identity true} :Title :String :Description :String :CreatedBy {:type :Google.Classroom/User :indexed true} :CreatedOn :DateTime}) 
                             (entity :Google.Classroom/Assignment {:Id {:type :String :identity true} :Title :String :Description :String :Course {:type :Google.Classroom/Course :indexed true} :CreatedBy {:type :Google.Classroom/User :indexed true} :CreatedOn :DateTime}) 
                             (relationship :Google.Classroom/Enrollment {:meta {:contains [:Google.Classroom/Course :Google.Classroom/User]}})
                             Use OAuth 2.0 for authentication"))
;;Generate a resolver that backs the following fractl data model with AWS SNS: (component :AWS.SNS) (entity :AWS.SNS/Topic {:Name :String :TopicArn :String}) (entity :AWS.SNS/Subscription {:Endpoint :String :Protocol :String :SubscriptionArn :String}) (entity :AWS.SNS/Message {:TopicArn :String :Content :String :Subject :String}) (relationship :AWS.SNS/TopicSubscription {:meta {:contains [:AWS.SNS/Topic :AWS.SNS/Subscription]}}) (relationship :AWS.SNS/TopicMessage {:meta {:contains [:AWS.SNS/Topic :AWS.SNS/Message]}})