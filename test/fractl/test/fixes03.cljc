(ns fractl.test.fixes03
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.component :as cn]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [fractl.util.hash :as sh]
            [fractl.lang.syntax :as ls]
            [fractl.lang.datetime :as dt]
            [fractl.lang
             :refer [component attribute event
                     entity record relationship dataflow]]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-576-alias
  (defcomponent :I576
    (entity
     {:I576/E
      {:X :Int}})
    (record
     {:I576/R
      {:A {:listof :I576/E}
       :B :Int
       :C {:listof :I576/E}}})
    (dataflow
     :I576/Evt
     {:I576/E? {} :as [:Result
                       [:E1 :_ :_ :E4 :& :Es]]}
     {:I576/R
      {:A :Result
       :B '(+ :E1.X :E4.X)
       :C :Es}}))
  (let [xs (range 1 11)
        sum (apply + xs)
        rs1 (mapv #(tu/first-result
                    {:I576/Upsert_E
                     {:Instance
                      {:I576/E
                       {:X %}}}})
                  xs)
        r (tu/first-result {:I576/Evt {}})
        e? (partial cn/instance-of? :I576/E)]
    (is (every? e? rs1))
    (is (= sum (apply + (mapv :X rs1))))
    (is (cn/instance-of? :I576/R r))
    (is (every? e? (:A r)))
    (is (= sum (apply + (mapv :X (:A r)))))
    (is (= 5 (:B r)))
    (is (every? e? (:C r)))
    (is (= 6 (count (:C r))))))

(deftest issue-585-eval
  (defcomponent :I585
    (entity
     :I585/E
     {:K {:type :String
          :indexed true}
      :X :Int})
    (record
     :I585/R
     {:Y :Int})
    (defn i585-f1 [e]
      (cn/make-instance
       :I585/R
       {:Y (* (:X e) 200)}))
    (defn i585-f2 [e]
      [(cn/make-instance
        :I585/R
        {:Y (* (:X e) 10)})])
    (defn i585-seq-of-r? [xs]
      (every? (partial cn/instance-of? :I585/R) xs))
    (dataflow
     :I585/Evt1
     {:I585/E {:K? :I585/Evt1.K}}
     [:eval '(fractl.test.fixes03/i585-f1 :I585/E)
      :check :I585/R :as :Result]
     {:I585/E {:K "result" :X :Result.Y}})
    (dataflow
     :I585/Evt2
     {:I585/E {:K? :I585/Evt2.K}}
     [:eval '(fractl.test.fixes03/i585-f2 :I585/E)
      :check fractl.test.fixes03/i585-seq-of-r? :as [:R1]]
     {:I585/E {:K "result" :X :R1.Y}})
    (dataflow
     :I585/Evt3
     {:I585/E {:K? :I585/Evt3.K}}
     [:eval '(fractl.test.fixes03/i585-f1 :I585/E)]))
  (let [e1 (tu/first-result
            {:I585/Upsert_E
             {:Instance
              {:I585/E {:K "abc" :X 10}}}})
        r1 (tu/first-result
            {:I585/Evt1 {:K "abc"}})
        r2 (tu/first-result
            {:I585/Evt2 {:K "abc"}})
        r3 (tu/fresult
            (tu/eval-all-dataflows
             {:I585/Evt3 {:K "abc"}}))]
    (is (cn/instance-of? :I585/E r1))
    (is (= 2000 (:X r1)))
    (is (cn/instance-of? :I585/E r2))
    (is (= 100 (:X r2)))
    (is (cn/instance-of? :I585/R r3))
    (is (= 2000 (:Y r3)))))

(deftest issue-599-uq-error
  (defcomponent :I599
    (entity
     :I599/E
     {:N {:type :Int :indexed true}
      :X :Int})
    (record
     :I599/R
     {:Data :Map})
    (dataflow
     :I599/Evt
     {:I599/E {:N? 1} :as [:A :& :_]}
     {:I599/R
      {:Data
       [:q#
        {:custom-value 1234
         :resolvers
         [{:name :abc
           :config {:x [:uq# :A.X] :y 20}}]}]}}))
  (let [e (tu/first-result
           {:I599/Upsert_E
            {:Instance
             {:I599/E {:N 1 :X 10}}}})
        r (tu/first-result
           {:I599/Evt {}})]
    (is (cn/instance-of? :I599/R r))
    (let [{x :x y :y} (:config (first (get-in r [:Data :resolvers])))]
      (is (and (= 10 x) (= 20 y))))))

(deftest issue-621-ref-as-hex
  (defcomponent :I621
    (entity
     :I621/Model
     {:Name {:type :Path
             :unique true}
      :Version :String
      :Config {:type :Map
               :optional true}
      :ClojureImports {:listof :Any
                       :optional true}})
    (entity
     :I621/Component
     {:Name :Path
      :Model {:ref :I621/Model.Name}
      :ClojureImports {:listof :Any
                       :optional true}
      :meta {:unique [:Model :Name]}})

    (entity
     :I621/Rec
     {:Name :Path
      :Model {:ref :I621/Model.Name}
      :Component :Path
      :Attributes :Map
      :meta {:unique [:Model :Component :Name]}
      :Meta {:type :Map
             :optional true}})

    (event
     :I621/CreateRec
     {:Name :Path
      :Model :Path
      :Component :Path
      :Attributes :Map
      :Meta {:type :Map
             :optional true}})

    (dataflow
     :I621/CreateRec
     {:I621/Component {:Name? :I621/CreateRec.Component
                       :Model? :I621/CreateRec.Model} :as :C}
     {:I621/Rec {:Name :I621/CreateRec.Name
                 :Model :C.Model
                 :Component :C.Name
                 :Attributes :I621/CreateRec.Attributes
                 :Meta :I621/CreateRec.Meta}}))

  (let [m (tu/first-result
           {:I621/Upsert_Model
            {:Instance
             {:I621/Model
              {:Name :m :Version "1.0"}}}})
        c (tu/first-result
           {:I621/Upsert_Component
            {:Instance
             {:I621/Component
              {:Name :c :Model :m}}}})
        attrs {:a 1 :b false :c 3}
        r (tu/first-result
           {:I621/CreateRec
            {:Name :r1
             :Model :m
             :Component :c
             :Attributes attrs}})
        m1 (tu/first-result
            {:I621/Lookup_Model
             {cn/id-attr (cn/id-attr m)}})
        c1 (tu/first-result
            {:I621/Lookup_Component
             {cn/id-attr (cn/id-attr c)}})
        r1 (tu/first-result
            {:I621/Lookup_Rec
             {cn/id-attr (cn/id-attr r)}})]
    (defn same-instance? [a b ks]
      (every? #(= (% a) (% b)) ks))
    (is (same-instance? m m1 [cn/id-attr :Name :Version]))
    (is (same-instance? c c1 [cn/id-attr :Name :Model]))
    (is (same-instance? r r1 [cn/id-attr :Name :Model :Component]))
    (is (= (:Model r1) :m))
    (is (= (:Component r1) :c))
    (is (= (:Attributes r1) attrs))))

(deftest issue-669-keyword-query-bug
  (defcomponent :I669
    (entity
     :I669/E
     {:K :Keyword
      :P :Path}))
  (let [e (tu/first-result
           {:I669/Upsert_E
            {:Instance
             {:I669/E
              {:K :hello
               :P :I669/F}}}})
        e1 (tu/first-result
            {:I669/Lookup_E
             {cn/id-attr (cn/id-attr e)}})]
    (is (cn/same-instance? e e1))))

(defn first-result [rs]
  (mapv first rs))

(deftest issue-686-list-of-path
  (defcomponent :I686
    (entity
     :I686/E
     {:Name {:type :Path
             :unique true}})
    (event
     :I686/GetEs
     {:Names {:listof :Path}})
    (record :I686/Result {:Es {:listof :I686/E}})
    (dataflow
     :I686/GetEs
     [:for-each :I686/GetEs.Names
      {:I686/E {:Name? :%}}
      :as :Es]
     {:I686/Result {:Es '(fractl.test.fixes03/first-result :Es)}}))
  (let [names [:A :B :C]
        es01 (mapv
              #(tu/first-result
                {:I686/Upsert_E
                 {:Instance
                  {:I686/E
                   {:Name %}}}})
              names)]
    (is (every? (partial cn/instance-of? :I686/E) es01))
    (let [rs (:Es
              (tu/first-result
               {:I686/GetEs
                {:Names [:A :C]}}))]
      (is (every? (partial cn/instance-of? :I686/E) rs))
      (is (= 2 (count rs)))
      (is (every? identity (mapv (fn [n] (some #{n} #{:A :C})) (mapv :Name rs)))))))

(deftest issue-741-rel-delete
  (defcomponent :I741
    (entity
     :I741/E1
     {:X {:type :Int
          :identity true}
      :Y :Int})
    (entity
     :I741/E2
     {:A {:type :Int
          :identity true}
      :B :Int})
    (entity
     :I741/E3
     {:C {:type :Int
          :identity true}
      :D :Int})
    (relationship
     :I741/R1
     {:meta {:contains [:I741/E1 :I741/E2]}})
    (relationship
     :I741/R2
     {:meta {:contains [:I741/E2 :I741/E3]}})
    (dataflow
     :I741/CreateE2
     {:I741/E1 {:X? :I741/CreateE2.E1} :as :E1}
     {:I741/E2
      {:A 10 :B 20}
      :-> [{:I741/R1 {}} :E1]})
    (dataflow
     :I741/CreateE3
     {:I741/E2
      {:A? :I741/CreateE3.E2}
      :-> [:I741/R1? {:I741/E1 {:X? :I741/CreateE3.E1}}]
      :as :E2}
     {:I741/E3
      {:C 3 :D 5}
      :-> [{:I741/R2 {}} :E2]})
    (dataflow
     :I741/LookupE2
     {:I741/E2? {}
      :-> [:I741/R1?
           {:I741/E1 {:X? :I741/LookupE2.E1}}]})
    (dataflow
     :I741/RemoveR2
     [:delete :I741/R2 [:->
                        {:I741/E2 {:A? :I741/RemoveR2.E2}}
                        {:I741/E3 {:C? :I741/RemoveR2.E3}}]])
    (dataflow
     :I741/RemoveR1
     [:delete :I741/R1 [:->
                        {:I741/E1 {:X? :I741/RemoveR1.E1}}
                        {:I741/E2 {:A? :I741/RemoveR1.E2}}]]))
  (let [e1 (tu/first-result
            {:I741/Upsert_E1
             {:Instance
              {:I741/E1 {:X 1 :Y 10}}}})
        e2 (tu/result
            {:I741/CreateE2
             {:E1 1}})]
    (is (cn/instance-of? :I741/E1 e1))
    (is (cn/instance-of? :I741/E2 e2))
    (is (cn/instance-of? :I741/R1 (first (:-> e2))))
    (defn- lookup-e2 [exists]
      (let [r (tu/result
               {:I741/LookupE2
                {:E1 1}})]
        (if exists
          (is (and (cn/instance-of? :I741/E2 (first r))
                   (= 20 (:B (first r)))))
          (is (= [:I741 :E2] r)))))
    (lookup-e2 true)
    (is (cn/instance-of?
         :I741/E3
         (tu/result
          {:I741/CreateE3
           {:E1 1 :E2 10}})))
    (is (not (tu/first-result
              {:I741/RemoveR1
               {:E1 1 :E2 10}})))
    (is (cn/instance-of?
         :I741/R2
         (tu/first-result
          {:I741/RemoveR2
           {:E2 10 :E3 3}})))
    (lookup-e2 true)
    (let [d1 (tu/first-result
              {:I741/RemoveR1
               {:E1 1 :E2 10}})]
      (is (cn/instance-of? :I741/R1 d1))
      (lookup-e2 false))))

(deftest issue-741-rel-delete-between
  (defcomponent :I741B
    (entity
     :I741B/E1
     {:X {:type :Int
          :identity true}
      :Y :Int})
    (entity
     :I741B/E2
     {:A {:type :Int
          :identity true}
      :B :Int})
    (relationship
     :I741B/R1
     {:meta {:between [:I741B/E1 :I741B/E2]}})
    (dataflow
     :I741B/CreateE2
     {:I741B/E1 {:X? :I741B/CreateE2.E1} :as :E1}
     {:I741B/E2
      {:A 10 :B 20}
      :-> [{:I741B/R1 {}} :E1]})
    (dataflow
     :I741B/LookupE2
     {:I741B/E2? {}
      :-> [:I741B/R1?
           {:I741B/E1 {:X? :I741B/LookupE2.E1}}]})
    (dataflow
     :I741B/RemoveR1
     [:delete :I741B/R1 [:->
                        {:I741B/E1 {:X? :I741B/RemoveR1.E1}}
                        {:I741B/E2 {:A? :I741B/RemoveR1.E2}}]]))
  (let [e1 (tu/first-result
            {:I741B/Upsert_E1
             {:Instance
              {:I741B/E1 {:X 1 :Y 10}}}})
        e2 (tu/result
            {:I741B/CreateE2
             {:E1 1}})]
    (is (cn/instance-of? :I741B/E1 e1))
    (is (cn/instance-of? :I741B/E2 e2))
    (is (cn/instance-of? :I741B/R1 (first (:-> e2))))
    (defn- lookup-e2 [exists]
      (let [r (if exists
                (tu/result
                 {:I741B/LookupE2
                  {:E1 1}})
                (tu/result
                 {:I741B/Lookup_E2
                  {:A 10}}))]
        (is (and (cn/instance-of? :I741B/E2 (first r))
                 (= 20 (:B (first r)))))))
    (lookup-e2 true)
    (let [d1 (tu/first-result
              {:I741B/RemoveR1
               {:E1 1 :E2 10}})]
      (is (cn/instance-of? :I741B/R1 d1))
      (lookup-e2 false))))

(deftest issue-754-for-each-introspect
  (let [s1 (ls/introspect
            [:for-each :E1
             {:FeDel/E2 {:A? :%.X}}
             [:delete :FeDel/E1 {:X :%.X}]
             :as :P])
        s2 (ls/introspect
            [:for-each :collection
             {:Department {:Name "hamza"}}
             [:delete :Department {:Name "hamza"}] :as :p])]
    (is (and (ls/for-each? s1) (ls/for-each? s2)))
    (is (= :E1 (ls/value-tag s1)))
    (is (= :collection (ls/value-tag s2)))
    (is (= :P (ls/alias-tag s1)))
    (is (= :p (ls/alias-tag s2)))
    (is (= 2 (count (ls/body-tag s1))))
    (is (= 2 (count (ls/body-tag s2))))))

(deftest issue-761-relationship-syntax
  (let [pat1 {:Acme/Employee
              {:Name "xyz"}
              :-> [{:Acme/WorksFor {:Location "south"}} :Dept]}
        obj1 (ls/introspect pat1)
        pat2 {:Acme/Employee? {}
              :-> [:Acme/WorksFor? :Dept]}
        obj2 (ls/introspect pat2)
        pat3 {:Acme/Employee? {}
              :-> [:Acme/WorksFor?
                   {:Acme/Dept {:No :DeptNo}
                    :-> [:Acme/PartOf? {:Acme/Company {:Name :CompanyName}}]}]}
        obj3 (ls/introspect pat3)
        pat4 {:C/E {:X 100}
              :-> [[{:C/R1 {}} :A]
                   [{:C/R2 {}} :B]]}
        obj4 (ls/introspect pat4)]
    (is (and (ls/upsert? obj1) (ls/relationship-object obj1)))
    (is (and (ls/query-upsert? obj2) (ls/relationship-object obj2)))
    (is (and (ls/query-upsert? obj3) (ls/relationship-object obj3)))
    (is (and (ls/upsert? obj4) (ls/relationship-object obj4)))
    (is (= (ls/raw obj1) pat1))
    (is (= (ls/raw obj2) pat2))
    (is (= (ls/raw obj3) pat3))
    (is (= (ls/raw obj4) pat4))
    (let [u1 (ls/upsert {ls/record-tag :Person
                         ls/attrs-tag {:Age :Int :Name :String}
                         ls/alias-tag :P1
                         ls/rel-tag [{:Spouse {}} :P2]})
          qu1 (ls/query-upsert {ls/record-tag :Person
                                ls/attrs-tag {:Name? "abc" :Age 100}
                                ls/alias-tag :P1
                                ls/rel-tag [:Spouse? {:Person {:Name? "xyz"}}]})
          d1 (ls/delete {ls/record-tag :Spouse
                         ls/rel-tag [{:Person {:Name "xyz"}} {:Person{:Name "abc"}}]})]
      (is (ls/upsert? u1))
      (is (ls/query-upsert? qu1))
      (is (= (ls/raw-relationship (ls/rel-tag u1)) [{:Spouse {}} :P2]))
      (is (= (ls/raw-relationship (ls/rel-tag qu1)) [:Spouse? {:Person {:Name? "xyz"}}]))
      (is (= (ls/raw-relationship (ls/rel-tag d1)) [{:Person {:Name "xyz"}} {:Person{:Name "abc"}}])))))

(deftest issue-765-delete-in-match
  (defcomponent :I765
    (entity
     :I765/E1
     {:X {:type :Int :identity true}})
    (entity
     :I765/E2
     {:Y {:type :Int :identity true}})
    (dataflow
     :I765/DelE
     [:match :I765/DelE.V
      1 [:delete :I765/E1 {:X :I765/DelE.X}]
      2 [:delete :I765/E2 {:Y :I765/DelE.Y}]]))
  (let [e1 (tu/first-result
            {:I765/Upsert_E1
             {:Instance
              {:I765/E1 {:X 100}}}})
        e2 (tu/first-result
            {:I765/Upsert_E2
             {:Instance
              {:I765/E2 {:Y 200}}}})
        r1 (tu/first-result
            {:I765/DelE {:V 1 :X 100}})
        r2 (tu/first-result
            {:I765/DelE {:V 2 :Y 200}})]
    (is (cn/same-instance? e1 r1))
    (is (cn/same-instance? e2 r2))))

(deftest issue-775-syntax-api-delete-bug
  (let [pat [:delete :CommentOnPost
             [:->
              {:Post {:Id :DeleteComment.PostId}}
              {:Comment {:Id :DeleteComment.CommentId}}]]]
    (is (= pat (ls/raw (ls/introspect pat))))))

(deftest syntax-api-alias-bug
  (let [pat {:User {:Email? :CreatePost.UserEmail}, :as :U}
        ir (ls/introspect pat)]
    (is (= (ls/alias-tag ir) :U))
    (is (= pat (ls/raw ir)))))

(deftest redefine-core-types
  (defcomponent :RedefTypes
    (attribute
     :RedefTypes/DateTime
     {:type :String})
    (entity
     :RedefTypes/E
     {:A :RedefTypes/DateTime
      :B :DateTime}))
  (is (tu/is-error
       #(cn/make-instance
         {:RedefTypes/E
          {:A "abc"
           :B "xyz"}})))
  (let [e1 (cn/make-instance
            {:RedefTypes/E
             {:A "abc"
              :B (dt/now)}})]
    (is (cn/instance-of? :RedefTypes/E e1))))
