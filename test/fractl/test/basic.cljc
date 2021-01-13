(ns fractl.test.basic
  "A basic cljs test."
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.util :as u]
            [fractl.util.hash :as sh]
            [fractl.store :as store]
            [fractl.component :as cn]
            [fractl.compiler :as c]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.lang.opcode :as opc]
            [fractl.compiler.context :as ctx]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

(def store (store/open-default-store nil))

(def eval-all-dataflows-for-event (tu/make-df-eval))

(defn- install-test-component []
  (cn/remove-component :CompileTest)
  (component :CompileTest)
  (entity {:CompileTest/E1
           {:X :Kernel/Int
            :Y :Kernel/Int}}))

(defn- init-test-context []
  (install-test-component)
  (let [ctx (c/make-context)
        f (partial store/compile-query store)]
    (ctx/bind-compile-query-fn! ctx f)
    ctx))

(defn- compile-pattern [ctx pat]
  (:opcode (c/compile-pattern ctx pat)))

(defn- pattern-compiler []
  (let [ctx (init-test-context)]
    [ctx (partial compile-pattern ctx)]))

(defn- valid-opcode? [opc-predic opcode v]
  (is (opc-predic opcode))
  (if (fn? v)
    (is (v (opc/arg opcode)))
    (is (= v (opc/arg opcode)))))

(defn- valid-opcode-with-query? [opcode farg]
  (is (opc/query-instances? opcode))
  (let [arg (opc/arg opcode)]
    (is (= farg (first arg)))))

(def ^:private load-instance? (partial valid-opcode? opc/load-instance?))
(def ^:private match-inst? (partial valid-opcode? opc/match-instance?))

(deftest compile-path
  (let [[_ c] (pattern-compiler)
        p1 :CompileTest/E1
        p1e :CompileTest/E111
        p2 :CompileTest/Upsert_E1
        p2e :CompileTest/Upsert_E111]
    (load-instance? (c p1) [[:CompileTest :E1] nil])
    (tu/is-error #(c p1e))
    (load-instance? (c p2) [[:CompileTest :Upsert_E1] nil])
    (tu/is-error #(c p2e))))

(deftest compile-pattern-01
  (let [[_ c] (pattern-compiler)
        p1 {:CompileTest/E1
            {:X 100
             :Y 200}}
        opcs (c p1)]
    (is (valid-opcode? opc/new-instance?
                       (first opcs) [:CompileTest :E1]))
    (is (valid-opcode? opc/set-literal-attribute?
                       (second opcs) [:X 100]))
    (is (valid-opcode? opc/set-literal-attribute?
                       (nth opcs 2) [:Y 200]))
    (is (valid-opcode? opc/intern-instance?
                       (nth opcs 3) [[:CompileTest :E1] nil]))))

(deftest compile-pattern-02
  (let [[ctx c] (pattern-compiler)
        p1 {:CompileTest/E1
            {:Id? 'id
             :X 100
             :Y '(+ :X 10)}}
        uuid (u/uuid-string)]
    ;; Variable `id` not in context.
    (tu/is-error #(c p1))
    ;; Any value will do, variable validation
    ;; will happen only during runtime.
    ;; In this case, the variable is resolved at
    ;; compile-time itself.
    (ctx/bind-variable! ctx 'id uuid)
    (let [opcs (c p1)]
      (is (valid-opcode-with-query? (first opcs) [:CompileTest :E1]))
      (is (valid-opcode? opc/set-literal-attribute?
                         (second opcs) [:X 100]))
      (is (valid-opcode? opc/set-compound-attribute?
                         (nth opcs 2) (fn [[n f]]
                                        (and (= :Y n) (fn? f)))))
      (is (valid-opcode? opc/intern-instance?
                         (nth opcs 3) [[:CompileTest :E1] nil])))))

(deftest circular-dependency
  (let [[ctx c] (pattern-compiler)
        p1 {:CompileTest/E1
            {:Id? 'id
             :X '(+ :Y 20)
             :Y '(+ :X 10)}}
        uuid (u/uuid-string)]
    (ctx/bind-variable! ctx 'id uuid)
    ;; Compilation fail on cyclic-dependency
    (tu/is-error #(c p1))))

(deftest compile-ref
  (defcomponent :Df01
    (entity {:Df01/E
             {:X :Kernel/Int
              :Y :Kernel/Int}}))
  (let [e (cn/make-instance :Df01/E {:X 10 :Y 20})
        evt (cn/make-instance :Df01/Upsert_E {:Instance e})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (is (cn/same-instance? e result))))

(deftest compile-create
  (defcomponent :Df02
    (entity {:Df02/E
             {:X :Kernel/Int
              :Y :Kernel/Int}})
    (record {:Df02/R {:A :Kernel/Int}})
    (event {:Df02/PostE {:R :Df02/R}}))
  (dataflow :Df02/PostE
            {:Df02/E {:X :Df02/PostE.R.A
                      :Y '(* :X 10)}})
  (let [r (cn/make-instance :Df02/R {:A 100})
        evt (cn/make-instance :Df02/PostE {:R r})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (is (cn/instance-of? :Df02/E result))
    (is (u/uuid-from-string (:Id result)))
    (is (= 100 (:X result)))
    (is (= 1000 (:Y result)))))

(deftest dependency
  (defcomponent :Df03
    (record {:Df03/R {:A :Kernel/Int}})
    (entity {:Df03/E {:X :Kernel/Int
                      :Y :Kernel/Int
                      :Z :Kernel/Int}})
    (event {:Df03/PostE {:R :Df03/R}}))
  (dataflow :Df03/PostE
            {:Df03/E {:X :Df03/PostE.R.A
                      :Z '(+ :X :Y)
                      :Y '(* :X 10)}})
  (let [r (cn/make-instance :Df03/R {:A 100})
        evt (cn/make-instance :Df03/PostE {:R r})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (is (cn/instance-of? :Df03/E result))
    (is (u/uuid-from-string (:Id result)))
    (is (= 100 (:X result)))
    (is (= 1000 (:Y result)))
    (is (= 1100 (:Z result)))))

(deftest compound-attributes
  (defcomponent :Df04
    (entity {:Df04/E1 {:A :Kernel/Int}})
    (entity {:Df04/E2 {:AId {:ref :Df04/E1.Id}
                       :X :Kernel/Int
                       :Y {:expr '(* :X :AId.A)}}})
    (event {:Df04/PostE2 {:E1 :Df04/E1}}))
  (dataflow :Df04/PostE2
            {:Df04/E2 {:AId :Df04/PostE2.E1.Id
                       :X 500}})
  (let [e (cn/make-instance :Df04/E1 {:A 100})
        evt (cn/make-instance :Df04/Upsert_E1 {:Instance e})
        e1 (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))
        id (:Id e1)
        e2 (cn/make-instance :Df04/E2 {:AId id
                                       :X 20})
        evt (cn/make-instance :Df04/PostE2 {:E1 e1})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (is (cn/instance-of? :Df04/E2 result))
    (is (u/uuid-from-string (:Id result)))
    (is (= (:AId result) id))
    (is (= (:X result) 500))
    (is (= (:Y result) 50000))))

(defn- assert-ca-e! [result]
  (is (cn/instance-of? :CA/E result))
  (is (= 20 (:A result)))
  (is (= 200 (:B result))))

(deftest compound-attributes-with-default-events
  (defcomponent :CA
    (entity {:CA/E {:A :Kernel/Int
                    :B {:expr '(* :A 10)}}}))
  (let [e (cn/make-instance :CA/E {:A 20})
        evt (cn/make-instance :CA/Upsert_E {:Instance e})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (assert-ca-e! result)
    (let [id (:Id result)
          evt (cn/make-instance :CA/Lookup_E {:Id id})
          result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
      (assert-ca-e! result))))

(deftest compound-attributes-literal-arg
  (defcomponent :Df04_1
    (record {:Df04_1/R {:A :Kernel/Int}})
    (entity {:Df04_1/E {:X :Kernel/Int
                      :Y {:expr '(* :X 10)}}})
    (event {:Df04_1/PostE {:R :Df04_1/R}}))
  (dataflow :Df04_1/PostE
            {:Df04_1/E {:X :Df04_1/PostE.R.A}})
  (let [r (cn/make-instance :Df04_1/R {:A 100})
        evt (cn/make-instance :Df04_1/PostE {:R r})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (is (cn/instance-of? :Df04_1/E result))
    (is (= (:X result) 100))
    (is (= (:Y result) 1000))))

(deftest fire-event
  (defcomponent :Df05
    (entity {:Df05/E1 {:A :Kernel/Int}})
    (entity {:Df05/E2 {:B :Kernel/Int}})
    (event {:Df05/Evt01 {:E1 :Df05/E1}})
    (event {:Df05/Evt02 {:E1 :Df05/E1}})
    (dataflow :Df05/Evt01
              {:Df05/Evt02 {:E1 :Df05/Evt01.E1}})
    (dataflow :Df05/Evt02
              {:Df05/E2 {:B :Df05/Evt02.E1.A}}))
  (let [e1 (cn/make-instance :Df05/E1 {:A 100})
        evt (cn/make-instance :Df05/Evt01 {:E1 e1})
        result (tu/fresult (eval-all-dataflows-for-event evt))
        inst (ffirst (tu/fresult (first result)))]
    (is (cn/instance-of? :Df05/E2 inst))
    (is (= (:B inst) 100))))

(deftest refcheck
  (defcomponent :RefCheck
    (entity {:RefCheck/E1 {:A :Kernel/Int}})
    (entity {:RefCheck/E2 {:AId {:ref :RefCheck/E1.Id}
                           :X :Kernel/Int}}))
  (let [e (cn/make-instance :RefCheck/E1 {:A 100})
        id (:Id e)
        e2 (cn/make-instance :RefCheck/E2 {:AId id :X 20})
        evt (cn/make-instance :RefCheck/Upsert_E2 {:Instance e2})]
    (tu/is-error
     #(tu/fresult (eval-all-dataflows-for-event evt)))
    (let [evt (cn/make-instance :RefCheck/Upsert_E1 {:Instance e})
          e1 (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))
          id (:Id e1)
          e2 (cn/make-instance :RefCheck/E2 {:AId id :X 20})
          evt (cn/make-instance :RefCheck/Upsert_E2 {:Instance e2})
          inst (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
      (is (cn/instance-of? :RefCheck/E2 inst))
      (is (= (:AId inst) id)))))

(deftest s3-test
  (defcomponent :AWS
    (record {:AWS/CreateBucketConfig
             {:LocationConstraint :Kernel/String}})
    (entity {:AWS/S3Bucket
             {:Bucket :Kernel/String
              :CreateBucketConfiguration :AWS/CreateBucketConfig}})
    (event {:AWS/CreateBucket
            {:Bucket :Kernel/String
             :Region :Kernel/String}}))
  (dataflow :AWS/CreateBucket
            {:AWS/CreateBucketConfig {:LocationConstraint :AWS/CreateBucket.Region}}
            {:AWS/S3Bucket {:Bucket :AWS/CreateBucket.Bucket
                            :CreateBucketConfiguration :AWS/CreateBucketConfig}})
  ;(override-test-resolver :AWSS3Resolver :AWS/S3Bucket)
  (let [bucket "ftltestbucket11"
        region "us-east-1"
        evt (cn/make-instance :AWS/CreateBucket {:Bucket bucket :Region region})
        e1 (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (is (cn/instance-of? :AWS/S3Bucket e1))
    (is (= bucket (:Bucket e1)))
    (is (cn/instance-of? :AWS/CreateBucketConfig (:CreateBucketConfiguration e1)))
    (is (= region (get-in e1 [:CreateBucketConfiguration :LocationConstraint])))))

(deftest record-in-entity
  (defcomponent :RecordEnt
    (record {:RecordEnt/R {:A :Kernel/Int}})
    (entity {:RecordEnt/E {:Q :Kernel/Int
                           :R :RecordEnt/R}})
    (event {:RecordEnt/PostE {:RA :Kernel/Int}}))
  (dataflow :RecordEnt/PostE
            {:RecordEnt/R {:A :RecordEnt/PostE.RA}}
            {:RecordEnt/E {:Q 100
                           :R :RecordEnt/R}})
  (let [evt (cn/make-instance :RecordEnt/PostE {:RA 10})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (is (cn/instance-of? :RecordEnt/E result))
    (is (u/uuid-from-string (:Id result)))
    (is (= 100 (:Q result)))
    (is (= 10 (:A (:R result))))))

(deftest hidden-attributes
  (defcomponent :H
    (entity {:H/E {:A :Kernel/Int
                   :X {:type :Kernel/String
                       :encryption :default
                       :write-only true}}}))
  (let [x "this is a secret"
        e (cn/make-instance :H/E {:A 10 :X x})
        evt (cn/make-instance :H/Upsert_E {:Instance e})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))
        r2 (cn/dissoc-write-only result)]
    (is (cn/instance-of? :H/E result))
    (is (sh/hash-eq? (:X result) x))
    (is (= 10 (:A result)))
    (is (cn/instance-of? :H/E r2))
    (is (not (:X r2)))
    (is (= 10 (:A r2)))))

(deftest alias-for-instances
  (defcomponent :Alias
    (entity {:Alias/E {:X :Kernel/Int}})
    (entity {:Alias/F {:Y :Kernel/Int}})
    (record {:Alias/R {:F :Alias/F}})
    (event {:Alias/Evt {:Instance :Alias/E}})
    (dataflow :Alias/Evt
              {:Alias/F {:Y :Alias/Evt.Instance.X} :as :G}
              {:Alias/R {:F :G}}))
  (let [e (cn/make-instance :Alias/E {:X 100})
        evt (cn/make-instance :Alias/Evt {:Instance e})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (is (cn/instance-of? :Alias/R result))
    (is (cn/instance-of? :Alias/F (:F result)))
    (is (= 100 (get-in result [:F :Y])))))

(deftest multi-alias
  (defcomponent :MultiAlias
    (entity {:MultiAlias/E {:X :Kernel/Int}})
    (entity {:MultiAlias/F {:A :Kernel/Int
                            :B :Kernel/Int}})
    (event {:MultiAlias/Evt {:EX1 :Kernel/Int
                             :EX2 :Kernel/Int}})
    (dataflow :MultiAlias/Evt
              {:MultiAlias/E {:X :MultiAlias/Evt.EX1} :as :E1}
              {:MultiAlias/E {:X :MultiAlias/Evt.EX2} :as :E2}
              {:MultiAlias/F {:A :E1.X :B :E2.X}}))
  (let [evt (cn/make-instance :MultiAlias/Evt {:EX1 100 :EX2 10})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (is (cn/instance-of? :MultiAlias/F result))
    (is (= 100 (:A result)))
    (is (= 10 (:B result)))))

(defn- conditional-event-01 [i x]
  (let [evt (cn/make-instance :Cond/Evt {:I i})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (is (cn/instance-of? :Cond/R result))
    (is (= x (:X result)))))

(defn- conditional-event-02 [r predic]
  (let [evt (cn/make-instance :Cond/EvtWithInst {:R r})
        result (tu/fresult (eval-all-dataflows-for-event evt))]
    (is (predic result))))

(deftest conditional
  (defcomponent :Cond
    (record {:Cond/R {:X :Kernel/Int}})
    (event {:Cond/Evt {:I :Kernel/Int}})
    (dataflow :Cond/Evt
              [:match :Cond/Evt.I
               0 {:Cond/R {:X 100}}
               1 {:Cond/R {:X 200}}
               {:Cond/R {:X 300}}])
    (event {:Cond/EvtWithInst {:R :Cond/R}})
    (dataflow :Cond/EvtWithInst
              {:Cond/R {:X 100} :as :R1}
              [:match :Cond/EvtWithInst.R.X
               :R1.X true]))
  (conditional-event-01 0 100)
  (conditional-event-01 1 200)
  (conditional-event-01 3 300)
  (conditional-event-02 (cn/make-instance :Cond/R {:X 200}) false?)
  (conditional-event-02 (cn/make-instance :Cond/R {:X 100}) true?))

(deftest match-with-alias
  (defcomponent :MA
    (record {:MA/R {:X :Kernel/Int}})
    (event {:MA/Evt {:I :Kernel/Int}})
    (dataflow :MA/Evt
              [:match :MA/Evt.I
               0 {:MA/R {:X 100}}
               1 {:MA/R {:X 200}}
               {:MA/R {:X 300}} :as :K]
              :K))
  (let [evt (cn/make-instance :MA/Evt {:I 1})
        r01 (tu/fresult (eval-all-dataflows-for-event evt))
        evt (cn/make-instance :MA/Evt {:I 0})
        r02 (tu/fresult (eval-all-dataflows-for-event evt))
        evt (cn/make-instance :MA/Evt {:I 3})
        r03 (tu/fresult (eval-all-dataflows-for-event evt))]
    (is (cn/instance-of? :MA/R r01))
    (is (= 200 (:X r01)))
    (is (cn/instance-of? :MA/R r02))
    (is (= 100 (:X r02)))
    (is (cn/instance-of? :MA/R r03))
    (is (= 300 (:X r03)))))

(deftest alias-scope
  (defcomponent :AScope
    (entity {:AScope/E {:X :Kernel/Int}})
    (record {:AScope/R {:A :Kernel/Int :B :Kernel/Int}})
    (event {:AScope/Evt {:I :Kernel/Int}})
    (dataflow :AScope/Evt
              {:AScope/E {:X :AScope/Evt.I} :as :E1}
              {:AScope/E {:X '(+ :E1.X 1)} :as :E2}
              {:AScope/R {:A :E1.X :B :E2.X}}))
  (let [evt (cn/make-instance :AScope/Evt {:I 10})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (is (cn/instance-of? :AScope/R result))
    (is (= 10 (:A result)))
    (is (= 11 (:B result)))))

(deftest delete-insts
  (defcomponent :Del
    (entity {:Del/E {:X :Kernel/Int}}))
  (let [e (cn/make-instance :Del/E {:X 100})
        evt (cn/make-instance :Del/Upsert_E {:Instance e})
        e01 (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))
        id (:Id e01)
        lookup-evt (cn/make-instance :Del/Lookup_E {:Id id})
        e02 (ffirst (tu/fresult (eval-all-dataflows-for-event lookup-evt)))
        del-evt (cn/make-instance :Del/Delete_E {:Id id})
        del-result (eval-all-dataflows-for-event del-evt)
        r01 (second (first (tu/fresult del-result)))
        r02 (eval-all-dataflows-for-event lookup-evt)]
    (is (cn/instance-of? :Del/E e01))
    (is (cn/same-instance? e01 e02))
    (is (= id r01))
    (is (= :not-found (:status (first r02))))))

(defn- assert-le
  ([n obj xs y]
   (is (cn/instance-of? n obj))
   (is (:Id obj))
   (is (= xs (:Xs obj)))
   (is (= y (:Y obj))))
  ([obj xs y] (assert-le :L/E obj xs y)))

(deftest listof
  (defcomponent :L
    (entity {:L/E {:Xs {:listof :Kernel/Int}
                   :Y :Kernel/Int}})
    (event {:L/MakeE0 {:Xs {:listof :Kernel/Int} :Y :Kernel/Int}})
    (dataflow :L/MakeE0
              {:L/E {:Xs :L/MakeE0.Xs :Y :L/MakeE0.Y}})
    (event {:L/MakeE1 {:X1 :Kernel/Int :X2 :Kernel/Int :Y :Kernel/Int}})
    (dataflow :L/MakeE1
              {:L/E {:Xs [:L/MakeE1.X1 :L/MakeE1.X2] :Y :L/MakeE1.Y}})
    (event {:L/MakeE2 {:X :Kernel/Int :Y :Kernel/Int}})
    (dataflow :L/MakeE2
              {:L/E {:Xs [(* 100 2) 780 :L/MakeE2.X] :Y :L/MakeE2.Y}})
    (entity {:L/F {:Xs {:listof :Kernel/Any}
                   :Y :Kernel/Int}}))
  (let [e (cn/make-instance :L/E {:Xs [1 2 3] :Y 100})
        evt (cn/make-instance :L/Upsert_E {:Instance e})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (assert-le result [1 2 3] 100))
  (let [evt (cn/make-instance :L/MakeE0 {:Xs [10 20 30 40] :Y 1})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (assert-le result [10 20 30 40] 1))
  (try
    (let [evt (cn/make-instance :L/MakeE0 {:Xs [10 "hi"] :Y 1})
          result (tu/fresult (eval-all-dataflows-for-event evt))]
      (is false))
    (catch #?(:clj Exception :cljs :default) ex
      (is ex)))
  (let [evt (cn/make-instance :L/MakeE1 {:X1 10 :X2 20 :Y 1})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (assert-le result [10 20] 1))
  (let [evt (cn/make-instance :L/MakeE2 {:X 10 :Y 90})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (assert-le result [200 780 10] 90))
  (let [e (cn/make-instance :L/F {:Xs [10 "hi"] :Y 1})
        evt (cn/make-instance :L/Upsert_F {:Instance e})
        result (ffirst (tu/fresult (eval-all-dataflows-for-event evt)))]
    (assert-le :L/F result [10 "hi"] 1)))

(deftest optional-attributes
  (defcomponent :OptAttr
    (entity {:OptAttr/E {:X :Kernel/Int
                         :Y {:type :Kernel/Int
                             :optional true}
                         :S :Kernel/String}})
    (entity {:OptAttr/F {:X :Kernel/Int
                         :Y :Kernel/Int
                         :S :Kernel/String
                         :meta {:required-attributes [:X :S]}}}))
  (let [e1 (cn/make-instance :OptAttr/E {:X 10 :S "hello"})
        e2 (cn/make-instance :OptAttr/E {:X 1 :Y 2 :S "hi"})]
    (is (cn/instance-of? :OptAttr/E e1))
    (is (= [10 0 "hello"] [(:X e1) (:Y e1) (:S e1)]))
    (is (cn/instance-of? :OptAttr/E e2))
    (is (= [1 2 "hi"] [(:X e2) (:Y e2) (:S e2)])))
  (let [f1 (cn/make-instance :OptAttr/F {:X 10 :S "hello"})
        f2 (cn/make-instance :OptAttr/F {:X 1 :Y 2 :S "hi"})]
    (is (cn/instance-of? :OptAttr/F f1))
    (is (= [10 0 "hello"] [(:X f1) (:Y f1) (:S f1)]))
    (is (cn/instance-of? :OptAttr/F f2))
    (is (= [1 2 "hi"] [(:X f2) (:Y f2) (:S f2)]))))
