(ns fractl.test.instance-meta
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.evaluator.intercept :as ei]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            #?(:clj  [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(defn- finally-reset-interceptors [f]  
  (try
    (f)
    (finally
      (ei/reset-interceptors!))))

(deftest meta-data-crud
  (is (= [:instance-meta] (ei/init-interceptors [:instance-meta])))
  (defcomponent :MetaDataCrud
    (entity
     :MetaDataCrud/E
     {:X :Kernel/Int})
    (dataflow
     :MetaDataCrud/Update
     {:MetaDataCrud/E
      {cn/q-id-attr :MetaDataCrud/Update.E
       :X 200}}))       
  (finally-reset-interceptors
   #(let [e1 (first
              (tu/result
               (cn/assoc-event-context-user
                "abcd"
                (cn/make-instance
                 {:MetaDataCrud/Upsert_E
                  {:Instance
                   (cn/make-instance {:MetaDataCrud/E {:X 100}})}}))))]
      (is (cn/instance-of? :MetaDataCrud/E e1))
      (let [id (cn/id-attr e1)
            lookup-meta
            (fn []
              (let [evt (cn/instance-meta-lookup-event
                         :MetaDataCrud/E id)
                    m1 (first (tu/result evt))]
                (is (cn/instance-of? (cn/meta-entity-name :MetaDataCrud/E) m1))
                (is (= (cn/id-attr m1) id))
                m1))
            m1 (lookup-meta)]
        (is (= "abcd" (:LastUpdatedBy m1) (:Owner m1)))
        (let [e2 (first
                  (tu/result
                   (cn/assoc-event-context-user
                    "xyz"
                    (cn/make-instance
                     {:MetaDataCrud/Update
                      {:E id}}))))
              e3 (first
                  (tu/result
                   (cn/make-instance
                    {:MetaDataCrud/Lookup_E
                     {cn/id-attr id}})))
              m2 (lookup-meta)]
          (is (= 200 (:X (get-in e2 [:transition :to]))))
          (is (= id (cn/id-attr e3)))
          (is (= 200 (:X e3)))
          (is (= "abcd" (:Owner m2)))
          (is (= "xyz" (:LastUpdatedBy m2))))))))
