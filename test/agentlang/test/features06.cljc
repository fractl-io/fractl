(ns agentlang.test.features06
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.lang
             :refer [component entity relationship dataflow attribute syntax pattern]]
            [agentlang.lang.raw :as lr]
            [agentlang.lang.syntax :as ls]
            #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(deftest attribute-extension
  (defcomponent :AttrEx
    (entity
     :AttrEx/A
     {:Id {:type :Int :guid true}
      :X :Int})
    (entity
     :AttrEx/B
     {:Id {:type :Int :guid true}
      :Y :Int})
    (relationship
     :AttrEx/R
     {:meta {:between [:AttrEx/A :AttrEx/B]}})
    (attribute
     :AttrEx/AB
     {:extend :AttrEx/A
      :type :AttrEx/B
      :relationship :AttrEx/R})
     (dataflow
      :AttrEx/LookupRByB
      {:AttrEx/R {:B? :AttrEx/LookupRByB.B}}))
  (let [a1 (tu/first-result
            {:AttrEx/Create_A
             {:Instance
              {:AttrEx/A {:Id 1 :X 100 :AB [:q# {:Id 2 :Y 200}]}}}})
        a? (partial cn/instance-type :AttrEx/A)
        b? (partial cn/instance-type :AttrEx/B)
        r? (partial cn/instance-type :AttrEx/R)
        lookup-b #(tu/first-result {:AttrEx/Lookup_B {:Id %}})
        lookup-r-by-b #(tu/first-result {:AttrEx/LookupRByB {:B %}})
        b1 (lookup-b 2)
        r1 (lookup-r-by-b 2)]
    (is (a? a1))
    (is (b? b1))
    (is (r? r1))
    (is (= 1 (:A r1)))))

(deftest syntax-raw-test
  (defcomponent :Rsd
    (entity :Rsd/A {:Id {:type :Int :guid true} :X :Int})
    (entity :Rsd/B {:Id {:type :Int :guid true} :Y :Int})
    (relationship :Rsd/AB {:meta {:between [:Rsd/A :Rsd/B]}})
    (attribute :Rsd/Bs {:extend :Rsd/A :type :Rsd/B :relationship :Rsd/AB})
    (syntax 'ab {:Rsd/A {:ident :Id}})
    (entity :Rsd/E {:K :Int}))
  (is (= (lr/as-edn :Rsd)
         '(do
            (component :Rsd)
            (entity :Rsd/A {:Id {:type :Int, :guid true}, :X :Int})
            (entity :Rsd/B {:Id {:type :Int, :guid true}, :Y :Int})
            (relationship
             :Rsd/AB
             {:meta {:between [:Rsd/A :Rsd/B], :cascade-on-delete true}})
            (attribute
             :Rsd/Bs
             {:extend :Rsd/A, :type :Rsd/B, :relationship :Rsd/AB})
            (syntax ab #:Rsd{:A {:ident :Id}})
            (entity :Rsd/E {:K :Int})))))

(deftest pattern-raw-test
  (defcomponent :Prt
    (entity :Prt/E {:Id {:type :Int :guid true} :X :Int})
    (pattern {:Prt/E {:Id 1 :X 100}})
    (entity :Prt/F {:Id {:type :Int :guid true} :Y :Int})
    (pattern {:Prt/E {:Id 2 :X 200}})
    (pattern {:Prt/F {:Id 3 :X 300}}))
  (is (= (lr/as-edn :Prt)
         '(do
            (component :Prt)
            (entity :Prt/E {:Id {:type :Int, :guid true}, :X :Int})
            #:Prt{:E {:Id 1, :X 100}}
            (entity :Prt/F {:Id {:type :Int, :guid true}, :Y :Int})
            #:Prt{:E {:Id 2, :X 200}}
            #:Prt{:F {:Id 3, :X 300}})))
  (let [check-pts (fn [ids]
                    (let [pts (lr/fetch-all-patterns :Prt)]
                      (is (= (count ids) (count pts)))
                      (mapv (fn [id inst]
                              (is (= id (:Id (first (vals inst))))))
                            ids pts)))]
    (check-pts [1 2 3])
    (lr/remove-pattern :Prt 1)
    (check-pts [1 3]))
  (is (= (lr/as-edn :Prt)
         '(do
            (component :Prt)
            (entity :Prt/E {:Id {:type :Int, :guid true}, :X :Int})
            #:Prt{:E {:Id 1, :X 100}}
            (entity :Prt/F {:Id {:type :Int, :guid true}, :Y :Int})
            #:Prt{:F {:Id 3, :X 300}}))))
