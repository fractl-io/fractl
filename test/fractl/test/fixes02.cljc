(ns fractl.test.fixes02
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.evaluator :as e]
            [fractl.lang.datetime :as dt]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-352-datetime-index
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I352DtIndex
     (entity
      {:I352DtIndex/E
       {:A {:type :Kernel/DateTime
            :indexed true}
        :B :Kernel/Int}})
     (dataflow :I352DtIndex/FindByDateTime
               {:I352DtIndex/E
                {:A? :I352DtIndex/FindByDateTime.Input}})
     (dataflow :I352DtIndex/FindBetween
               {:I352DtIndex/E
                {:A? [[:> :I352DtIndex/FindBetween.Start]
                      [:< :I352DtIndex/FindBetween.End]]}}))
   (let [dt "2021-12-30T03:30:24"
         r1 (tu/first-result
             {:I352DtIndex/Upsert_E
              {:Instance
               {:I352DtIndex/E
                {:A dt
                 :B 100}}}})
         r2 (tu/first-result
             {:I352DtIndex/FindByDateTime
              {:Input dt}})
         r3 (tu/first-result
             {:I352DtIndex/FindBetween
              {:Start "2021-11-30T00:00:00"
               :End "2022-01-30T00:00:00"}})
         r4 (first
             (e/eval-all-dataflows
              {:I352DtIndex/FindBetween
               {:Start "2022-11-30T00:00:00"
                :End "2023-01-30T00:00:00"}}))]
     (is (cn/instance-of? :I352DtIndex/E r1))
     (is (cn/instance-of? :I352DtIndex/E r2))
     (is (cn/instance-of? :I352DtIndex/E r3))
     (is (cn/same-instance? r1 r2))
     (is (cn/same-instance? r1 r3))
     (is (= :not-found (:status r4))))))

(deftest issue-352-date-time-formats
  (#?(:clj do
      :cljs cljs.core.async/go)
   (let [dates ["January 8, 2021" "2021-Jan-08"
                "Jan-08-2021" "08-Jan-2021" "20210108"]
         times ["04:05:06.789" "04:05" "040506"
                "04:05 pm" "04:05:06 PST"
                "04:05:06 America/New_York"]]
     (is (every? dt/parse-date dates))
     (is (every? dt/parse-time times)))))
