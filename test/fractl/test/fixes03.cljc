(ns fractl.test.fixes03
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.set :as set]
            [fractl.component :as cn]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [fractl.util :as u]
            [fractl.util.hash :as sh]
            [fractl.lang.syntax :as ls]
            [fractl.lang.datetime :as dt]
            [fractl.lang.internal :as li]
            [fractl.lang.raw :as raw]
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
                    {:I576/Create_E
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
            {:I585/Create_E
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
           {:I599/Create_E
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
           {:I621/Create_Model
            {:Instance
             {:I621/Model
              {:Name :m :Version "1.0"}}}})
        c (tu/first-result
           {:I621/Create_Component
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
           {:I669/Create_E
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
                {:I686/Create_E
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
    (is (= :E1 (ls/name-tag (ls/value-tag s1))))
    (is (= :collection (ls/name-tag (ls/value-tag s2))))
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
    (is (and (ls/query? obj2) (ls/relationship-object obj2)))
    (is (and (ls/query? obj3) (ls/relationship-object obj3)))
    (is (and (ls/upsert? obj4) (ls/relationship-object obj4)))
    (is (= (ls/raw obj1) pat1))
    (is (= (ls/raw obj2) pat2))
    (is (= (ls/raw obj3) pat3))
    (is (= (ls/raw obj4) pat4))
    (let [u1 (ls/upsert {ls/record-tag :Person
                         ls/attrs-tag {:Age :Int :Name :String}
                         ls/alias-tag :P1
                         ls/rel-tag [{:Spouse {}} :P2]})
          qu1 (ls/query {ls/record-tag :Person
                         ls/attrs-tag {:Name? "abc" :Age 100}
                         ls/alias-tag :P1
                         ls/rel-tag [:Spouse? {:Person {:Name? "xyz"}}]})
          d1 (ls/delete {ls/record-tag :Spouse
                         ls/rel-tag [{:Person {:Name "xyz"}} {:Person{:Name "abc"}}]})]
      (is (ls/upsert? u1))
      (is (ls/query? qu1))
      (is (= (ls/raw-relationship (ls/rel-tag u1)) [{:Spouse {}} :P2]))
      (is (= (ls/raw-relationship (ls/rel-tag qu1)) [:Spouse? {:Person {:Name? "xyz"}}]))
      (is (= (ls/raw-relationship (ls/rel-tag d1)) [{:Person {:Name "xyz"}} {:Person{:Name "abc"}}])))))

(deftest issue-765-delete-in-match
  (defcomponent :I765
    (entity
     :I765/E1
     {:X {:type :Int tu/guid true}})
    (entity
     :I765/E2
     {:Y {:type :Int tu/guid true}})
    (dataflow
     :I765/DelE
     [:match :I765/DelE.V
      1 [:delete :I765/E1 {:X :I765/DelE.X}]
      2 [:delete :I765/E2 {:Y :I765/DelE.Y}]]))
  (let [e1 (tu/first-result
            {:I765/Create_E1
             {:Instance
              {:I765/E1 {:X 100}}}})
        e2 (tu/first-result
            {:I765/Create_E2
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

(deftest query-object-bug
  (let [obj (ls/query-object {ls/record-tag :Blog/PostAuthorship?})]
    (is (= (ls/raw obj) :Blog/PostAuthorship?))))

(deftest path-query-syntax
  (let [p1 {:C/E? "path://A/Evt.A/R/B"}
        r1 (ls/introspect p1)
        p2 {:C/E {:? "path://A/Evt.A/R/B"
                  :K 100}}
        r2 (ls/introspect p2)]
    (is (ls/query? r1))
    (is (ls/query-upsert? r2))
    (is (= p1 (ls/raw r1)))
    (is (= p2 (ls/raw r2)))))

(deftest reference-syntax
  (let [p1 :X
        r1 (ls/introspect p1)
        p2 :Abc/Xyz
        r2 (ls/introspect p2)]
    (is (ls/reference? r1))
    (is (ls/reference? r2))
    (is (= p1 (ls/raw r1)))
    (is (= p2 (ls/raw r2)))))

(deftest eval-syntax-bug
  (let [fn-call '(a/f :A/K.Arg1 :A/K.Arg2)
        exp [:eval fn-call]
        p1 (ls/introspect exp)
        exp-with-check (vec (concat exp [:check :A/B]))
        p2 (ls/introspect exp-with-check)
        p3 (ls/introspect (vec (concat exp [:as :R])))
        p4 (ls/introspect (vec (concat exp-with-check [:as :R])))
        ps [p1 p2 p3 p4]]
    (is (every? ls/eval? ps))
    (is (every? #(= fn-call (second (ls/raw (ls/exp-tag %)))) ps))
    (is (= :A/B (ls/raw (ls/check-tag p2))))
    (is (= :R (ls/raw (ls/alias-tag p3))))
    (is (and (= :A/B (ls/raw (ls/check-tag p4)))
             (= :R (ls/raw (ls/alias-tag p4)))))))

(deftest issue-886-create-update
  (defcomponent :I886
    (entity
     :I886/E
     {:Id {:type :Int tu/guid true}
      :Name :String})
    (dataflow
     :I886/CreateE
     {:I886/E {} :from :I886/CreateE.Instance})
    (dataflow
     :I886/UpdateE
     {:I886/E
      {:Id? :I886/UpdateE.Id
       :Name :I886/UpdateE.Name}}))
  (let [e1 (tu/first-result
            {:I886/CreateE
             {:Instance
              {:I886/E
               {:Id 1 :Name "abc"}}}})
        e2 (tu/first-result
            {:I886/Lookup_E
             {:Id 1}})]
    (is (cn/same-instance? e1 e2))
    (let [e3 (tu/first-result
              {:I886/UpdateE
               {:Id 1 :Name "xyz"}})
          e4 (tu/first-result
              {:I886/Lookup_E
               {:Id 1}})]
      (is (cn/instance-eq? e1 e3))
      (is (= (:Name e3) "xyz"))
      (is (cn/instance-eq? e1 e4))
      (is (= (:Name e4) "xyz")))))

(deftest issue-991-record-names-bug
  (defcomponent :I991
    (entity :I991/A {:Id :Identity :X :Int})
    (entity :I991/B {:Id :Identity :Y :Int})
    (entity :I991/C {:Id :Identity :Y :Int})
    (record :I991/D {:Z :Int})
    (event :I991/F {:K :Int})
    (relationship :I991/R1 {:meta {:contains [:I991/A :I991/B]}})
    (relationship :I991/R2 {:meta {:between [:I991/B :I991/C]}}))
  (is (not (raw/find-record :I991/R1)))
  (is (= #{:I991/D} (cn/record-names :I991)))
  (is (= #{:I991/A :I991/B :I991/C} (cn/entity-names :I991)))
  (is (= #{:I991/F} (set/intersection #{:I991/F} (cn/event-names :I991)))))

(deftest entity-default-id
  (defcomponent :Edid
    (entity :Edid/E {}))
  (is (li/id-attr (cn/fetch-schema :Edid/E)))
  (is (= li/id-attr (cn/identity-attribute-name :Edid/E)))
  (is (not (seq (raw/find-entity :Edid/E)))))

#?(:clj
   (deftest issue-1023-spec-in-raw
     (let [spec '(do (component :I1023)
                     (attribute :I1023/UniqueName {:type :String, :unique true})
                     (entity :I1023/E {:Id :Identity, :Name :I1023/UniqueName, :X {:type :Int, :optional true}}))
           preproc-spec (map (fn [x]
                               (if (seqable? x)
                                 (let [n (first x) xs (rest x)]
                                   `(~(symbol (str "fractl.lang/" (name n))) ~@xs))
                                 x))
                             spec)
           third #(nth % 2)
           third-of-third (comp third third)
           third-of-second (comp third second)]
       (eval preproc-spec)
       (is (= spec (raw/as-edn :I1023)))
       (is (= (third-of-third (rest spec)) (raw/find-entity :I1023/E)))
       (is (= (third-of-second (rest spec)) (raw/find-attribute :I1023/UniqueName))))))

(deftest contains-in-relnames
  (defcomponent :Cir
    (record :Cir/X {:Y :Int})
    (entity :Cir/A {:Id :Identity})
    (entity :Cir/B {:Id :Identity})
    (entity :Cir/C {:Id :Identity})
    (relationship :Cir/R1 {:meta {:contains [:Cir/A :Cir/B]}})
    (relationship :Cir/R2 {:meta {:contains [:Cir/B :Cir/C]}})
    (relationship :Cir/R3 {:meta {:between [:Cir/A :Cir/A]}}))
  (is (= (cn/record-names :Cir) #{:Cir/X}))
  (is (= (cn/entity-names :Cir) #{:Cir/B :Cir/C :Cir/A}))
  (is (= (cn/relationship-names :Cir) #{:Cir/R1 :Cir/R2 :Cir/R3})))

(deftest issue-1067-empty-uuid
  (defcomponent :I1067
    (entity
     :I1067/E
     {:Id {:type :Int tu/guid true}
      :X {:type :UUID :optional true}}))
  (let [create-e (fn [id x]
                   (tu/first-result
                    {:I1067/Create_E
                     {:Instance
                      {:I1067/E
                       (merge {:Id id} (when x {:X x}))}}}))
        lookup-e (fn [id]
                   (tu/first-result
                    {:I1067/Lookup_E
                     {:Id id}}))
        e? (partial cn/instance-of? :I1067/E)
        e1 (create-e 1 nil)
        e2 (create-e 2 "")
        e3 (create-e 3 (u/uuid-string))]
    (is (e? e1))
    (is (not (:X e1)))
    (is (not e2))
    (is (e? e3))
    (is (u/uuid-from-string (:X e3)))
    (is (not (:X (lookup-e 1))))
    (is (not (e? (lookup-e 2))))
    (is (cn/same-instance? e3 (lookup-e 3)))))

#?(:clj
   (deftest optional-data-conversion-error-in-db
     (defcomponent :DbDataConv
       (entity
        :DbDataConv/E
        {:Id :Identity
         :X :String
         :Y {:type :Int :optional true}
         :Z {:type :Decimal :optional true}
         :W {:listof :String :optional true}}))
     (let [e (tu/first-result
              {:DbDataConv/Create_E
               {:Instance
                {:DbDataConv/E {:X "hello"}}}})
           e? (partial cn/instance-of? :DbDataConv/E)]
       (is (e? e))
       (is (cn/instance-eq?
            e
            (tu/first-result
             {:DbDataConv/Update_E
              {:Id (:Id e)
               :Data {:X "hi" :W ["a" "b"]}}})))
       (is (cn/instance-eq?
            e
            (tu/first-result
             {:DbDataConv/Update_E
              {:Id (:Id e)
               :Data {:Y 10 :Z 145.34}}})))
       (let [e (tu/first-result
                {:DbDataConv/Lookup_E
                 {:Id (:Id e)}})]
         (is (= "hi" (:X e)))
         (is (= ["a" "b"] (:W e)))
         (is (= 10 (:Y e)))
         (is (= 145.34M (:Z e))))
       (let [e (tu/first-result
                {:DbDataConv/Create_E
                 {:Instance
                  {:DbDataConv/E
                   {:X "krs"
                    :Y 1 :Z 1.23
                    :W ["k" "r" "s"]}}}})]
         (is (e? e))
         (is (= "krs" (:X e)))
         (is (= 1 (:Y e)))
         (is (= 1.23 (:Z e)))
         (is (= ["k" "r" "s"] (:W e)))))))

(deftest match-query-issue
  (defcomponent :Mqs
    (entity
     :Mqs/E
     {:Id {:type :Int :guid true}
      :X :String})
    (entity
     :Mqs/F
     {:Id {:type :Int :guid true}
      :Y :Int})
    (entity
     :Mqs/G
     {:Id :Identity
      :Pid {:type :Int :id true}
      :Z :Int})
    (relationship
     :Mqs/R
     {:meta {:contains [:Mqs/F :Mqs/G]}})
    (dataflow
     :Mqs/Q
     {:Mqs/E
      {:Id? :Mqs/Q.E}
      :as [:E1]}
     {:Mqs/F
      {:Id? :Mqs/Q.F}
      :as [:F1]}
     [:match :Mqs/E.X
      "a" {:Mqs/G {:Pid 100 :Z 12}
           :-> [[:Mqs/R :F1]]}
      "b" {:Mqs/G {:Pid 200 :Z 24}
           :-> [[:Mqs/R :F1]]}]))
  (let [es (mapv #(tu/first-result
                   {:Mqs/Create_E
                    {:Instance
                     {:Mqs/E {:Id %1 :X %2}}}})
                 [1 2 3] ["a" "b" "c"])
        fs (mapv #(tu/first-result
                   {:Mqs/Create_F
                    {:Instance
                     {:Mqs/F {:Id % :Y (* % 10)}}}})
                 [1 2 3])
        e? (partial cn/instance-of? :Mqs/E)
        f? (partial cn/instance-of? :Mqs/F)
        g? (partial cn/instance-of? :Mqs/G)]
    (is (= (count es) 3) (every? e? es))
    (is (= (count fs) 3) (every? f? fs))
    (let [g (tu/first-result {:Mqs/Q {:E 1 :F 2}})]
      (is (g? g))
      (is (= (:Pid g) 100))
      (is (cn/same-instance?
           g (tu/first-result
              {:Mqs/Lookup_G {li/path-attr (li/path-attr g)}}))))))
