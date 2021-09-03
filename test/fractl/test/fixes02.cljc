(ns fractl.test.fixes02
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.evaluator :as e]
            [fractl.lang.datetime :as dt]
            [clojure.java.io :as io]
            #?(:clj [fractl.datafmt.csv :as csv])
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
                "04:05 pm"
                "04:05 AM"
                "04:05:06 PST"
                "04:05:06 America/New_York"]
         date-times ["2021-01-08T04:05:06"
                     "2021-01-08 04:05:06"
                     "2021-01-08 04:05:06.789"
                     "20210108040506"
                     "2021-01-08 04:05:06 PST"
                     "2021-01-08 04:05:06 America/New_York"]]
     (is (every? dt/parse-date dates))
     (is (every? dt/parse-time times))
     (is (every? dt/parse-date-time date-times)))))

(deftest issue-352-date-time-upserts
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I352Dtu
     (entity
      :I352Dtu/E
      {:A :Kernel/Date
       :B :Kernel/Time}))
   (let [r1 (tu/first-result
             {:I352Dtu/Upsert_E
              {:Instance
               {:I352Dtu/E
                {:A "2021-08-26"
                 :B "14:24:30"}}}})
         r2 (tu/first-result
             {:I352Dtu/Lookup_E
              {:Id (:Id r1)}})]
     (is (cn/same-instance? r1 r2)))))

#?(:clj
   (deftest issue-358-csv-import
     (defcomponent :I358Csv01
       (entity
        :I358Csv01/Employee
        {:FirstName :Kernel/String
         :LastName :Kernel/String
         :Salary :Kernel/Decimal})
       (dataflow
        :I358Csv01/ImportEmployees
        {:Kernel/DataSync
         {:Source
          {:Kernel/DataSource
           {:Uri "file://sample/emp.csv"
            :Entity "I358Csv01/Employee"
            :AttributeMapping
            {"first_name" "FirstName"
             "last_name" "LastName"
             "salary" "Salary"}}}}})
       (dataflow
        :I358Csv01/ExportEmployees
        {:Kernel/DataSync
         {:Source
          {:Kernel/DataSource
           {:Entity "I358Csv01/Employee"
            :AttributeMapping
            {"FirstName" "first_name"
             "LastName" "last_name"
             "Salary" "salary"}}}
          :DestinationUri "file://sample/emp2.csv"}}))
     (let [r (first
              (e/eval-all-dataflows
               {:I358Csv01/ImportEmployees {}}))
           result (first (second (:result r)))]
       (is (= :ok (:status r)))
       (is (= :data-sync (:resolver result)))
       (is (every?
            (partial cn/instance-of? :I358Csv01/Employee)
            (:result result)))
       (let [id (:Id (first (:result result)))
             r (tu/first-result
                {:I358Csv01/Lookup_Employee
                 {:Id id}})]
         (is (cn/same-instance? r (first (:result result)))))
       (let [r (first
                (e/eval-all-dataflows
                 {:I358Csv01/ExportEmployees {}}))
             result (first (second (:result r)))
             csv-file "sample/emp2.csv"]
         (is (= :ok (:status r)))
         (is (= :data-sync (:resolver result)))
         (is (= csv-file (:result result)))
         (let [csv (csv/read-csv csv-file)]
           (io/delete-file csv-file true)
           (is (= ["first_name" "last_name" "salary"] (first csv)))
           (doseq [row (rest csv)]
             (is (some #{row} [["robert" "k" "2400"] ["jane" "a" "5600"]]))))))))
