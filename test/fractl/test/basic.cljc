(ns fractl.test.basic
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [fractl.util :as u]
            [fractl.util.hash :as sh]
            [fractl.store :as store]
            [fractl.component :as cn]
            [fractl.compiler :as c]
            [fractl.lang
             :refer [component attribute event
                     entity record relationship dataflow]]
            [fractl.evaluator :as e]
            [fractl.lang.opcode :as opc]
            [fractl.compiler.context :as ctx]
            [fractl.lang.datetime :as dt]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

#? (:clj
    (def store (store/open-default-store nil))
    :cljs
    (def store (store/open-default-store {:type :reagent})))

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
        evt {:Df01/Upsert_E {:Instance e}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
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
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
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
        evt {:Df03/PostE {:R r}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :Df03/E result))
    (is (u/uuid-from-string (:Id result)))
    (is (= 100 (:X result)))
    (is (= 1000 (:Y result)))
    (is (= 1100 (:Z result)))))

(deftest boolean-type
  (defcomponent :Bool
    (entity {:Bool/E {:X :Kernel/Boolean
                      :Y :Kernel/Boolean}})
    (event {:Bool/PostE1 {:B :Kernel/Boolean}})
    (event {:Bool/PostE2 {:B :Kernel/Boolean}}))
  (dataflow :Bool/PostE1
            {:Bool/E {:X :Bool/PostE1.B
                      :Y true}})
  (dataflow :Bool/PostE2
            {:Bool/E {:X :Bool/PostE2.B
                      :Y false}})
  (let [evt (cn/make-instance :Bool/PostE1 {:B true})
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :Bool/E result))
    (is (u/uuid-from-string (:Id result)))
    (is (= true (:X result)))
    (is (= true (:Y result))))
  (let [evt (cn/make-instance :Bool/PostE2 {:B false})
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :Bool/E result))
    (is (u/uuid-from-string (:Id result)))
    (is (= false (:X result)))
    (is (= false (:Y result)))))

(deftest self-reference
  (defcomponent :SelfRef
    (entity {:SelfRef/E
             {:X :Kernel/Int
              :Y :Kernel/Int
              :Z :Kernel/Int}})
    (event {:SelfRef/AddToX {:EId :Kernel/UUID
                             :Y :Kernel/Int}}))
  (dataflow :SelfRef/AddToX
            {:SelfRef/E {:Id? :SelfRef/AddToX.EId
                         :X '(+ :X :SelfRef/AddToX.Y)
                         :Y :SelfRef/AddToX.Y
                         :Z 1}})
  (let [e (cn/make-instance :SelfRef/E {:X 100 :Y 200 :Z 300})
        evt (cn/make-instance :SelfRef/Upsert_E {:Instance e})
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :SelfRef/E result))
    (is (u/uuid-from-string (:Id result)))
    (is (= 100 (:X result)))
    (is (= 200 (:Y result)))
    (is (= 300 (:Z result)))
    (let [id (:Id result)
          addevt (cn/make-instance :SelfRef/AddToX {:EId id :Y 10})
          result (first (tu/fresult (e/eval-all-dataflows addevt)))
          inst (or (get-in result [:transition :to]) result)]
      (is (cn/instance-of? :SelfRef/E inst))
      (is (u/uuid-from-string (:Id inst)))
      (is (= 110 (:X inst)))
      (is (= 10 (:Y inst)))
      (is (= 1 (:Z inst))))))

(deftest compound-attributes
  (defcomponent :Df04
    (entity {:Df04/E1 {:A :Kernel/Int}})
    (entity {:Df04/E2 {:AId {:ref :Df04/E1.Id}
                       :X :Kernel/Int
                       :Y {:type :Kernel/Int
                           :expr '(* :X :AId.A)}}})
    (event {:Df04/PostE2 {:E1 :Df04/E1}}))
  (dataflow :Df04/PostE2
            {:Df04/E2 {:AId :Df04/PostE2.E1.Id
                       :X 500}})
  (let [e (cn/make-instance :Df04/E1 {:A 100})
        evt (cn/make-instance :Df04/Upsert_E1 {:Instance e})
        e1 (first (tu/fresult (e/eval-all-dataflows evt)))
        id (:Id e1)
        e2 (cn/make-instance :Df04/E2 {:AId (:Id e1)
                                       :X 20})
        evt (cn/make-instance :Df04/PostE2 {:E1 e1})
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
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
                    :B {:type :Kernel/Int
                        :expr '(* :A 10)}}}))
  (let [e (cn/make-instance :CA/E {:A 20})
        evt {:CA/Upsert_E {:Instance e}}
        r (e/eval-all-dataflows evt)
        result (first (tu/fresult r))]
    (assert-ca-e! result)
    (let [id (:Id result)
          evt {:CA/Lookup_E {:Id id}}
          r (e/eval-all-dataflows evt)
          result (first (tu/fresult r))]
      (assert-ca-e! result))))

(deftest compound-attributes-literal-arg
  (defcomponent :Df041
    (record {:Df041/R {:A :Kernel/Int}})
    (entity {:Df041/E {:X :Kernel/Int
                       :Y {:type :Kernel/Int
                           :expr '(* :X 10)}}})
    (event {:Df041/PostE {:R :Df041/R}}))
  (dataflow :Df041/PostE
            {:Df041/E {:X :Df041/PostE.R.A}})
  (let [r (cn/make-instance :Df041/R {:A 100})
        evt (cn/make-instance :Df041/PostE {:R r})
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :Df041/E result))
    (is (= (:X result) 100))
    (is (= (:Y result) 1000))))

(deftest fire-event
  (#?(:clj do
      :cljs cljs.core.async/go)
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
         evt {:Df05/Evt01 {:E1 e1}}
         result (first (tu/fresult (e/eval-all-dataflows evt)))]
     (is (cn/instance-of? :Df05/E2 result))
     (is (= (:B result) 100)))))

(deftest refcheck
  (defcomponent :RefCheck
    (entity {:RefCheck/E1 {:A :Kernel/Int}})
    (entity {:RefCheck/E2 {:AId {:ref :RefCheck/E1.Id}
                           :X :Kernel/Int}}))
  (let [e (cn/make-instance :RefCheck/E1 {:A 100})
        id (:Id e)
        e2 (cn/make-instance :RefCheck/E2 {:AId (:Id e) :X 20})
        evt (cn/make-instance :RefCheck/Upsert_E2 {:Instance e2})]
    (tu/is-error
     #(tu/fresult (e/eval-all-dataflows evt)))
    (let [evt (cn/make-instance :RefCheck/Upsert_E1 {:Instance e})
          e1 (first (tu/fresult (e/eval-all-dataflows evt)))
          id (:Id e1)
          e2 (cn/make-instance :RefCheck/E2 {:AId (:Id e1) :X 20})
          evt (cn/make-instance :RefCheck/Upsert_E2 {:Instance e2})
          inst (first (tu/fresult (e/eval-all-dataflows evt)))]
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
        evt {:AWS/CreateBucket {:Bucket bucket :Region region}}
        e1 (first (tu/fresult (e/eval-all-dataflows evt)))]
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
  (let [evt {:RecordEnt/PostE {:RA 10}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :RecordEnt/E result))
    (is (u/uuid-from-string (:Id result)))
    (is (= 100 (:Q result)))
    (is (= 10 (:A (:R result))))))

(deftest hidden-attributes
  (defcomponent :H
    (entity {:H/E {:A :Kernel/Int
                   :X {:type :Kernel/String
                       :secure-hash true
                       :write-only true}}}))
  (let [x "this is a secret"
        e (cn/make-instance :H/E {:A 10 :X x})
        evt {:H/Upsert_E {:Instance e}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))
        r2 (cn/dissoc-write-only result)]
    (is (cn/instance-of? :H/E result))
    (is (sh/crypto-hash-eq? (:X result) x))
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
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
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
  (let [evt {:MultiAlias/Evt {:EX1 100 :EX2 10}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :MultiAlias/F result))
    (is (= 100 (:A result)))
    (is (= 10 (:B result)))))

(defn- conditional-event-01 [i x]
  (let [evt {:Cond/Evt {:I i}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :Cond/R result))
    (is (= x (:X result)))))

(defn- conditional-event-02 [r predic]
  (let [evt {:Cond/EvtWithInst {:R r}}
        result (tu/fresult (e/eval-all-dataflows evt))]
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

(deftest conditional-boolean
  (defcomponent :CondBool
    (record {:CondBool/R {:X :Kernel/Int}})
    (event {:CondBool/Evt {:I :Kernel/Boolean}})
    (dataflow :CondBool/Evt
              [:match :CondBool/Evt.I
               true {:CondBool/R {:X 100}}
               false {:CondBool/R {:X 200}}
               {:CondBool/R {:X 300}}]))
  (let [evt {:CondBool/Evt {:I true}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :CondBool/R result))
    (is (= 100 (:X result))))
  (let [evt {:CondBool/Evt {:I false}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :CondBool/R result))
    (is (= 200 (:X result)))))

(deftest conditional-pattern-list
  (defcomponent :CondPatList
    (record {:CondPatList/R {:X :Kernel/Int}})
    (event {:CondPatList/Evt {:I :Kernel/Int}})
    (dataflow :CondPatList/Evt
              [:match :CondPatList/Evt.I
               0 [{:CondPatList/R {:X 100}}
                  {:CondPatList/R {:X 101}}]
               1 {:CondPatList/R {:X 200}}
               {:CondPatList/R {:X 300}}]))
  (let [evt {:CondPatList/Evt {:I 0}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :CondPatList/R result))
    (is (= 101 (:X result))))
  (let [evt {:CondPatList/Evt {:I 1}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :CondPatList/R result))
    (is (= 200 (:X result)))))

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
  (let [r01 (first (tu/fresult (e/eval-all-dataflows {:MA/Evt {:I 1}})))
        r02 (first (tu/fresult (e/eval-all-dataflows {:MA/Evt {:I 0}})))
        r03 (first (tu/fresult (e/eval-all-dataflows {:MA/Evt {:I 3}})))]
    (is (cn/instance-of? :MA/R r01))
    (is (= 200 (:X r01)))
    (is (cn/instance-of? :MA/R r02))
    (is (= 100 (:X r02)))
    (is (cn/instance-of? :MA/R r03))
    (is (= 300 (:X r03)))))

(deftest match-with-alias-no-alternative-case
  (defcomponent :MA2
    (record {:MA2/R {:X :Kernel/Int}})
    (event {:MA2/Evt {:I :Kernel/Int}})
    (dataflow :MA2/Evt
              [:match :MA2/Evt.I
               0 {:MA2/R {:X 100}}
               1 {:MA2/R {:X 200}} :as :K]
              :K))
  (let [r01 (first (tu/fresult (e/eval-all-dataflows {:MA2/Evt {:I 1}})))
        r02 (first (tu/fresult (e/eval-all-dataflows {:MA2/Evt {:I 0}})))
        r03 (first (tu/fresult (e/eval-all-dataflows {:MA2/Evt {:I 2}})))]
    (is (cn/instance-of? :MA2/R r01))
    (is (= 200 (:X r01)))
    (is (cn/instance-of? :MA2/R r02))
    (is (= 100 (:X r02)))))

(deftest alias-scope
  (defcomponent :AScope
    (entity {:AScope/E {:X :Kernel/Int}})
    (record {:AScope/R {:A :Kernel/Int :B :Kernel/Int}})
    (event {:AScope/Evt {:I :Kernel/Int}})
    (dataflow :AScope/Evt
              {:AScope/E {:X :AScope/Evt.I} :as :E1}
              {:AScope/E {:X '(+ :E1.X 1)} :as :E2}
              {:AScope/R {:A :E1.X :B :E2.X}}))
  (let [result (first (tu/fresult (e/eval-all-dataflows {:AScope/Evt {:I 10}})))]
    (is (cn/instance-of? :AScope/R result))
    (is (= 10 (:A result)))
    (is (= 11 (:B result)))))

(deftest for-each
  (defcomponent :ForEach
    (entity {:ForEach/E {:X :Kernel/Int}})
    (record {:ForEach/R {:A :Kernel/Int}})
    (event {:ForEach/Evt {:I :Kernel/Int}})
    (dataflow :ForEach/Evt
              {:ForEach/E {:X :ForEach/Evt.I} :as :E1}
              {:ForEach/E {:X '(+ :E1.X 1)} :as :E2}
              [:for-each :ForEach/E?
               {:ForEach/R {:A :ForEach/E.X}}]))
  (let [result (tu/fresult (e/eval-all-dataflows {:ForEach/Evt {:I 10}}))
        firstE (first result)
        secondE (second result)]
    (is (= 2 (count result)))
    (is (cn/instance-of? :ForEach/R firstE))
    (is (= 10 (:A firstE)))
    (is (cn/instance-of? :ForEach/R secondE))
    (is (= 11 (:A secondE)))))

(deftest for-each-literal-result
  (defcomponent :ForEachLR
    (entity {:ForEachLR/E {:X :Kernel/Int}})
    (record {:ForEachLR/R {:A :Kernel/Int}})
    (event {:ForEachLR/Evt {:I :Kernel/Int}})
    (dataflow :ForEachLR/Evt
              {:ForEachLR/E {:X :ForEachLR/Evt.I} :as :E1}
              {:ForEachLR/E {:X '(+ :E1.X 1)} :as :E2}
              [:for-each :ForEachLR/E?
               {:ForEachLR/R {:A :ForEachLR/E.X}}
               :ForEachLR/R.A]))
  (let [result (tu/fresult (e/eval-all-dataflows {:ForEachLR/Evt {:I 10}}))]
    (is (= 2 (count result)))
    (is (= 10 (first result)))
    (is (= 11 (second result)))))

(deftest for-each-basic-query
  (defcomponent :ForEachBQ
    (entity {:ForEachBQ/E {:X {:type :Kernel/Int
                               :indexed true}}})
    (record {:ForEachBQ/R {:A :Kernel/Int}})
    (event {:ForEachBQ/Evt {:I :Kernel/Int}})
    (dataflow :ForEachBQ/Evt
              {:ForEachBQ/E {:X :ForEachBQ/Evt.I} :as :E1}
              {:ForEachBQ/E {:X '(+ :E1.X 1)} :as :E2}
              [:for-each {:ForEachBQ/E {:X? 10}}
               {:ForEachBQ/R {:A :ForEachBQ/E.X}}]))
  (let [result (tu/fresult (e/eval-all-dataflows {:ForEachBQ/Evt {:I 10}}))
        firstE (first result)]
    (is (= 1 (count result)))
    (is (cn/instance-of? :ForEachBQ/R firstE))
    (is (= 10 (:A firstE)))))

(deftest for-each-with-alias
  (defcomponent :ForEachAlias
    (entity {:ForEachAlias/E {:X :Kernel/Int}})
    (record {:ForEachAlias/R {:A :Kernel/Int}})
    (event {:ForEachAlias/Evt {:I :Kernel/Int}})
    (dataflow :ForEachAlias/Evt
              {:ForEachAlias/E {:X :ForEachAlias/Evt.I} :as :E1}
              {:ForEachAlias/E {:X '(+ :E1.X 1)} :as :E2}
              [:for-each :ForEachAlias/E?
               {:ForEachAlias/R {:A :ForEachAlias/E.X}} :as :L]
              :L))
  (let [result (tu/fresult (e/eval-all-dataflows {:ForEachAlias/Evt {:I 10}}))
        firstE (first result)
        secondE (second result)]
    (is (= 2 (count result)))
    (is (cn/instance-of? :ForEachAlias/R firstE))
    (is (= 10 (:A firstE)))
    (is (cn/instance-of? :ForEachAlias/R secondE))
    (is (= 11 (:A secondE)))))

(deftest delete-insts
  (defcomponent :Del
    (entity {:Del/E {:X :Kernel/Int}}))
  (let [e (cn/make-instance :Del/E {:X 100})
        e01 (first (tu/fresult (e/eval-all-dataflows {:Del/Upsert_E {:Instance e}})))
        id (:Id e01)
        lookup-evt (cn/make-instance :Del/Lookup_E {:Id id})
        e02 (first (tu/fresult (e/eval-all-dataflows lookup-evt)))
        del-result (e/eval-all-dataflows {:Del/Delete_E {:Id id}})
        r01 (str (second (tu/fresult del-result)))
        r02 (e/eval-all-dataflows lookup-evt)]
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
        evt {:L/Upsert_E {:Instance e}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (assert-le result [1 2 3] 100))
  (let [evt {:L/MakeE0 {:Xs [10 20 30 40] :Y 1}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (assert-le result [10 20 30 40] 1))
  (try
    (let [evt {:L/MakeE0 {:Xs [10 "hi"] :Y 1}}
          result (tu/fresult (e/eval-all-dataflows evt))]
      (is false))
    (catch #?(:clj Exception :cljs :default) ex
      (is ex)))
  (let [evt {:L/MakeE1 {:X1 10 :X2 20 :Y 1}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (assert-le result [10 20] 1))
  (let [evt {:L/MakeE2 {:X 10 :Y 90}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (assert-le result [200 780 10] 90))
  (let [e (cn/make-instance :L/F {:Xs [10 "hi"] :Y 1})
        evt {:L/Upsert_F {:Instance e}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
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
    (is (= [10 nil "hello"] [(:X e1) (:Y e1) (:S e1)]))
    (is (cn/instance-of? :OptAttr/E e2))
    (is (= [1 2 "hi"] [(:X e2) (:Y e2) (:S e2)])))
  (let [f1 (cn/make-instance :OptAttr/F {:X 10 :S "hello"})
        f2 (cn/make-instance :OptAttr/F {:X 1 :Y 2 :S "hi"})]
    (is (cn/instance-of? :OptAttr/F f1))
    (is (= [10 nil "hello"] [(:X f1) (:Y f1) (:S f1)]))
    (is (cn/instance-of? :OptAttr/F f2))
    (is (= [1 2 "hi"] [(:X f2) (:Y f2) (:S f2)]))))

(deftest optional-record-attribute
  (defcomponent :OptRecAttr
    (record {:OptRecAttr/R {:A :Kernel/Int}})
    (entity {:OptRecAttr/E {:Q :Kernel/Int
                            :R {:type :OptRecAttr/R
                                :optional true}}})
    (event {:OptRecAttr/PostE {:Q :Kernel/Int}}))
  (dataflow :OptRecAttr/PostE
            {:OptRecAttr/E {:Q :OptRecAttr/PostE.Q}})
  (let [evt {:OptRecAttr/PostE {:Q 10}}
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :OptRecAttr/E result))
    (is (u/uuid-from-string (:Id result)))
    (is (= 10 (:Q result)))))

(deftest inherits
  (defcomponent :Inherits
    (record {:Inherits/Base {:A {:type :Kernel/Int
                                 :optional true}
                             :B :Kernel/Int}})
    (entity {:Inherits/E {:meta {:inherits :Inherits/Base}
                          :X :Kernel/Int
                          :Y {:type :Kernel/Int
                              :optional true}
                          :S :Kernel/String}})
    (record {:Inherits/BaseMeta {:A :Kernel/Int
                                 :B :Kernel/Int
                                 :meta {:required-attributes [:B]}}})
    (entity {:Inherits/F {:meta {:inherits :Inherits/BaseMeta}
                          :X :Kernel/Int
                          :Y {:type :Kernel/Int
                              :optional true}
                          :S :Kernel/String}}))
  (let [e1 (cn/make-instance :Inherits/E {:X 10 :S "hello" :A 100 :B 200})
        e2 (cn/make-instance :Inherits/E {:X 1 :Y 2 :S "hi" :B 200})]
    (is (cn/instance-of? :Inherits/E e1))
    (is (= [10 nil "hello" 100 200] [(:X e1) (:Y e1) (:S e1) (:A e1) (:B e1)]))
    (is (cn/instance-of? :Inherits/E e2))
    (is (= [1 2 "hi" nil 200] [(:X e2) (:Y e2) (:S e2) (:A e2) (:B e2)])))
  (let [f1 (cn/make-instance :Inherits/F {:X 10 :S "hello" :A 100 :B 200})
        f2 (cn/make-instance :Inherits/F {:X 1 :Y 2 :S "hi" :B 200})]
    (is (cn/instance-of? :Inherits/F f1))
    (is (= [10 nil "hello" 100 200] [(:X f1) (:Y f1) (:S f1) (:A f1) (:B f1)]))
    (is (cn/instance-of? :Inherits/F f2))
    (is (= [1 2 "hi" nil 200] [(:X f2) (:Y f2) (:S f2) (:A f2) (:B f2)]))))

(deftest multi-level-inherits
  (defcomponent :MultiInherits
    (record {:MultiInherits/Base1 {:A {:type :Kernel/Int
                                       :optional true}
                                   :B :Kernel/Int}})
    (record {:MultiInherits/Base2 {:C :Kernel/Int
                                   :meta {:inherits :MultiInherits/Base1}}})
    (entity {:MultiInherits/E {:X :Kernel/Int
                               :Y {:type :Kernel/Int
                                   :optional true}
                               :S :Kernel/String
                               :meta {:inherits :MultiInherits/Base2}}}))
  (let [e1 (cn/make-instance :MultiInherits/E {:X 10 :S "hello" :A 100 :B 200 :C 300})
        e2 (cn/make-instance :MultiInherits/E {:X 1 :Y 2 :S "hi" :B 200 :C 300})]
    (is (cn/instance-of? :MultiInherits/E e1))
    (is (= [10 nil "hello" 100 200 300] [(:X e1) (:Y e1) (:S e1) (:A e1) (:B e1) (:C e1)]))
    (is (cn/instance-of? :MultiInherits/E e2))
    (is (= [1 2 "hi" nil 200 300] [(:X e2) (:Y e2) (:S e2) (:A e2) (:B e2) (:C e1)]))))

(deftest multi-level-inherits-meta
  (defcomponent :MultiInheritsMeta
    (record {:MultiInheritsMeta/Base1 {:A :Kernel/Int
                                       :B :Kernel/Int
                                       :meta {:required-attributes [:B]}}})
    (record {:MultiInheritsMeta/Base2 {:C :Kernel/Int
                                       :meta {:inherits :MultiInheritsMeta/Base1}}})
    (entity {:MultiInheritsMeta/E {:X :Kernel/Int
                                   :Y {:type :Kernel/Int
                                       :optional true}
                                   :S :Kernel/String
                                   :meta {:inherits :MultiInheritsMeta/Base2}}}))
  (let [e1 (cn/make-instance :MultiInheritsMeta/E {:X 10 :S "hello" :A 100 :B 200 :C 300})
        e2 (cn/make-instance :MultiInheritsMeta/E {:X 1 :Y 2 :S "hi" :B 200 :C 300})]
    (is (cn/instance-of? :MultiInheritsMeta/E e1))
    (is (= [10 nil "hello" 100 200 300] [(:X e1) (:Y e1) (:S e1) (:A e1) (:B e1) (:C e1)]))
    (is (cn/instance-of? :MultiInheritsMeta/E e2))
    (is (= [1 2 "hi" nil 200 300] [(:X e2) (:Y e2) (:S e2) (:A e2) (:B e2) (:C e1)]))))

(deftest edn-attribute
  (defcomponent :EdnAttr
    (entity :EdnAttr/Form {:Title :Kernel/String
                           :X :Kernel/Int
                           :Y {:type :Kernel/Int
                               :expr '(+ :X 10)}
                           :View :Kernel/Edn})
    (event :EdnAttr/RenderLoginForm {:Title :Kernel/String
                                     :X :Kernel/Int
                                     :Y :Kernel/Int})
    (dataflow :EdnAttr/RenderLoginForm
              {:EdnAttr/Form
               {:Title :EdnAttr/RenderLoginForm.Title
                :X :EdnAttr/RenderLoginForm.X
                :View [:q#
                       [:div [:p [:b [:uq# :EdnAttr/RenderLoginForm.Title]]]
                        [:div
                         [:label "Username: "]
                         [:textinput [:size [:uq# '(* :EdnAttr/RenderLoginForm.Y 10)]]]]
                        [:div
                         [:label "Password: "]
                         [:password [:size 40]]]
                        [:div
                         [:submit [:text "Login"]]]]]}}))
  (let [result (first
                (tu/fresult
                 (e/eval-all-dataflows
                  {:EdnAttr/RenderLoginForm
                   {:Title "Login" :X 150 :Y 12}})))]
    (is (cn/instance-of? :EdnAttr/Form result))
    (is (= 160 (:Y result)))
    (is (= [:div [:p [:b "Login"]]]
           [(first (:View result))
            (second (:View result))]))
    (is (= 120 (-> (nthrest (:View result) 2)
                   first
                   (nth 2) second second)))))

(deftest patterns-in-attributes
  (defcomponent :PA
    (event {:PA/OnClickEvent {:Source {:type :Kernel/UUID :optional true}}})
    (record {:PA/Position {:X :Kernel/Int :Y :Kernel/Int
                           :W :Kernel/Int :H :Kernel/Int}})
    (entity {:PA/Button {:Title :Kernel/String
                         :Position :PA/Position
                         :OnClick :PA/OnClickEvent}})
    (event {:PA/AddButton {:Title :Kernel/String :Position :PA/Position}})
    (dataflow :PA/AddButton
              {:PA/Button {:Title :PA/AddButton.Title
                           :Position :PA/AddButton.Position
                           :OnClick {:PA/OnClickEvent {:Source :Id}}}}))
  (let [pos (cn/make-instance {:PA/Position {:X 10 :Y 10 :W 100 :H 50}})
        add-btn (cn/make-instance {:PA/AddButton {:Title "OK" :Position pos}})
        result (first (tu/fresult (e/eval-all-dataflows add-btn)))]
    (is (cn/instance-of? :PA/Button result))
    (is (cn/instance-of? :PA/OnClickEvent (:OnClick result)))))

(deftest edn-ui
  (defcomponent :EdnUI
    (entity {:EdnUI/UserLogin
             {:UserNameLabel {:type :Kernel/String
                              :default "Username: "}
              :PasswordLabel {:type :Kernel/String
                              :default "Password: "}
              :ButtonTitle {:type :Kernel/String
                            :default "Login"}
              :HandlerEvent {:type :Kernel/Keyword
                             :optional true}
              :View {:type :Kernel/Edn
                     :default
                     [:div
                      [:div
                       [:label :UserNameLabel]
                       [:input [:type "text"]]]
                      [:div
                       [:label :PasswordLabel]
                       [:input [:type "password"]]]
                      [:div
                       [:button [:title :ButtonTitle]
                        :on-click :HandlerEvent]]]}}})
    (entity {:EdnUI/LoginForm
             {:UserLogin {:type :EdnUI/UserLogin
                          :optional true}
              :Title {:type :Kernel/String
                      :default "Login"}
              :DOMTarget :Kernel/String
              :View {:type :Kernel/Edn
                     :default
                     [:div
                      [:h2 :Title]
                      [:div :UserLogin.View]]}}})

    (event {:EdnUI/LoginEvent
            {:Data :Kernel/Any}})

    (dataflow :EdnUI/MakeLoginForm
              {:EdnUI/UserLogin
               {:UserNameLabel :EdnUI/MakeLoginForm.UserNameLabel
                :PasswordLabel :EdnUI/MakeLoginForm.PasswordLabel
                :ButtonTitle :EdnUI/MakeLoginForm.ButtonTitle
                :HandlerEvent :EdnUI/MakeLoginForm.HandlerEvent}}
              {:EdnUI/LoginForm
               {:Title :EdnUI/MakeLoginForm.FormTitle
                :DOMTarget "app"
                :UserLogin :EdnUI/UserLogin}})
    (let [evt (cn/make-instance {:EdnUI/MakeLoginForm
                                 {:FormTitle "Login to the V8 Platform"
                                  :UserNameLabel "Your V8 userId or email: "
                                  :PasswordLabel "Password: "
                                  :ButtonTitle "Login"
                                  :HandlerEvent :EdnUI/LoginEvent}})
          result (first (tu/fresult (e/eval-all-dataflows evt)))]
      (is (cn/instance-of? :EdnUI/LoginForm result)))))

(deftest async-event
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :AE
     (record {:AE/R01 {:X :Kernel/Int}})
     (event {:AE/Evt01 {:A :Kernel/Int}})
     (event {:AE/Evt02 {:B :Kernel/Int}})
     (dataflow :AE/Evt01
               {:AE/Evt02 {:B :AE/Evt01.A}})
     (dataflow :AE/Evt02
               {:AE/R01 {:X :AE/Evt02.B}}))
   (let [evt01 (cn/make-instance {:AE/Evt01 {:A 100}})
         result (first (tu/fresult (e/eval-all-dataflows evt01)))]
     (is (cn/instance-of? :AE/R01 result))
     (is (= 100 (:X result))))))

(deftest path-type
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :PathType
     (entity {:PathType/E
              {:X :Kernel/Path}}))
   (let [e1 (cn/make-instance {:PathType/E {:X :A/B.R}})
         e2 (cn/make-instance {:PathType/E {:X "A/B.R"}})]
     (is (cn/instance-of? :PathType/E e1))
     (is (= :A/B.R (:X e1)))
     (is (cn/instance-of? :PathType/E e2))
     (is (= :A/B.R (keyword (:X e2))))
     (is (= "k/j" (:X (cn/make-instance {:PathType/E {:X "k/j"}}))))
     (is (= :k (:X (cn/make-instance {:PathType/E {:X :k}})))))))

(deftest format-test
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :FormatTest
     (entity {:FormatTest/E
              {:DOB {:type :Kernel/String
                     :format "^((19|2[0-9])[0-9]{2})-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$"}}})
     (is (cn/instance-of?
          :FormatTest/E
          (cn/make-instance
           {:FormatTest/E
            {:DOB "1999-03-20"}})))
     (tu/is-error #(cn/make-instance
                    {:FormatTest/E
                     {:DOB "1877-09-01"}})))))

(deftest numeric-types
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :NT
     (entity
      :NT/E
      {:X :Kernel/Int
       :Y :Kernel/Int64
       :Z :Kernel/BigInteger
       :A :Kernel/Float
       :B :Kernel/Double
       :C :Kernel/Decimal
       :D :Kernel/DateTime})
     (let [bi 8993993938884848858996996
           f 1.2
           d "90.8"
           dt "2014-03-14T12:34:20.000000"
           e (cn/make-instance
              {:NT/E
               {:X 10
                :Y #?(:clj Long/MAX_VALUE
                      :cljs Number.MAX_VALUE)
                :Z bi
                :A f
                :B f
                :C d
                :D dt}})]
       (is (cn/instance-of? :NT/E e))
       (is (= 90.8M (:C e)))
       (is (= (float f) (:A e)))
       (is (= (double f) (:B e)))
       (is (= bi (:Z e)))
       (is (= dt (:D e)))))))

(deftest match-cond
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :MC
     (record
      {:MC/R
       {:X :Kernel/Int}})
     (dataflow
      :MC/E
      [:match
       [:< :MC/E.X 10] {:MC/R {:X 1}}
       [:>= :MC/E.X 100] {:MC/R {:X 2}}
       {:MC/R {:X 3}}
       :as :Result]
      :Result))
   (defn run [x result]
     (let [r (first
              (tu/fresult
               (e/eval-all-dataflows
                (cn/make-instance
                 {:MC/E {:X x}}))))]
       (is (= (:X r) result))))
   (run 200 2)
   (run 9 1)
   (run 22 3)))

(deftest inheritance-type-check
  (defcomponent :Itc
    (record
     :Itc/Base
     {:X :Kernel/Int})
    (record
     :Itc/Child1
     {:meta {:inherits :Itc/Base}
      :Y :Kernel/Int})
    (record
     :Itc/Child2
     {:meta {:inherits :Itc/Base}
      :Z :Kernel/Int})
    (record
     :Itc/Child3
     {:meta {:inherits :Itc/Child1}
      :A :Kernel/Int})
    (entity
     :Itc/E
     {:Vals {:listof :Itc/Base}}))
  (let [v1 (cn/make-instance {:Itc/Child1 {:X 1 :Y 2}})
        v2 (cn/make-instance {:Itc/Child2 {:X 10 :Z 20}})
        v3 (cn/make-instance {:Itc/Child3 {:X 9 :Y 8 :A 7}})
        e (cn/make-instance {:Itc/E
                             {:Vals [v1 v2 v3]}})
        result (first
                (tu/fresult
                 (e/eval-all-dataflows
                  (cn/make-instance
                   {:Itc/Upsert_E
                    {:Instance e}}))))]
    (is (cn/instance-of? :Itc/E result))
    (every? #(or (cn/instance-of? :Itc/Child1 %)
                 (cn/instance-of? :Itc/Child2 %)
                 (cn/instance-of? :Itc/Child3 %))
            (:Vals result))
    (every? (partial cn/instance-of? :Itc/Base) (:Vals result))))

#?(:clj
   (deftest check-wrong-reference-attribute-use
     (defcomponent :UserAccount
                   (entity {:UserAccount/Estimate
                            {:Balance :Kernel/Float
                             :Loan    :Kernel/Float}})
                   (record {:UserAccount/Total {:Total :Kernel/Float}})
                   (event {:UserAccount/IncreaseLoan {:Balance :UserAccount/Total}}))
     (dataflow :UserAccount/IncreaseLoan
               {:UserAccount/Estimate {:Balance :UserAccount/IncreaseLoan.Total
                                       :Loan    '(+ :Balance 10000)}})
     (let [r (cn/make-instance :UserAccount/Total {:Total 100000})
           evt (cn/make-instance :UserAccount/IncreaseLoan {:Balance r})]
       (is (thrown-with-msg? Exception #"Error in: Event "
                             (cn/instance-of? :UserAccount/Estimate
                                              (first (tu/fresult (e/eval-all-dataflows evt)))))))))

#?(:clj
   (deftest access-wrong-event-entity
     (defcomponent :UserAccount
                   (entity {:UserAccount/Estimate
                            {:Balance :Kernel/Float
                             :Loan    :Kernel/Float}})
                   (record {:UserAccount/Total {:Total :Kernel/Float}})
                   (event {:UserAccount/IncreaseLoan {:Balance :UserAccount/Total}}))
     (dataflow :UserAccount/IncreaseLoan
               ;Intentional
               {:UserAccount/Estimate {:Balance :UserAccount/Increase.Balance.Total
                                       :Loan    '(+ :Balance 10000)}})
     (let [r (cn/make-instance :UserAccount/Total {:Total 100000})
           evt (cn/make-instance :UserAccount/IncreaseLoan {:Balance r})]
       (is (thrown-with-msg? Exception #"Reference cannot be found for"
                             (cn/instance-of? :UserAccount/Estimate
                                              (first (tu/fresult (e/eval-all-dataflows evt)))))))))

#?(:clj
   (deftest ref-id-of-record
          (defcomponent :UserAccount
                        (record {:UserAccount/Estimate
                                 {:Balance :Kernel/Float
                                  :Loan    :Kernel/Float}})
                        (record {:UserAccount/Total {:Total :Kernel/Float}})
                        (event {:UserAccount/IncreaseLoan {:Id      {:type    :Kernel/UUID
                                                                     :default "167d0b04-fa75-11eb-9a03-0242ac130003"}
                                                           :Balance :UserAccount/Total}}))
          (dataflow :UserAccount/IncreaseLoan
                    {:UserAccount/Estimate {:Id? :UserAccount/IncreaseLoan.Id}}
                    {:UserAccount/Estimate {:Balance :UserAccount/IncreaseLoan.Balance.Total
                                            :Loan    '(+ :Balance 10000)}})
          (let [r (cn/make-instance :UserAccount/Total {:Total 100000})
                evt (cn/make-instance :UserAccount/IncreaseLoan {:Balance r})]
            (is (thrown? Exception (cn/instance-of? :UserAccount/Estimate (first (tu/fresult (e/eval-all-dataflows evt)))))))))

(deftest try_
  (defcomponent :Try
    (entity
     :Try/E
     {:X {:type :Kernel/Int
          :indexed true}})
    (record
     :Try/R
     {:Y :Kernel/Boolean})
    (dataflow
     :Try/Find
     [:try
      {:Try/E {:X? :Try/Find.X}}
      :ok {:Try/R {:Y true}}
      [:error :not-found] {:Try/R {:Y false}}]))
  (let [r1 (tu/first-result {:Try/Find {:X 100}})
        e (cn/make-instance {:Try/E {:X 100}})
        _ (tu/first-result {:Try/Upsert_E
                            {:Instance e}})
        r2 (tu/first-result {:Try/Find {:X 100}})]
    (is (not (:Y r1)))
    (is (:Y r2))))

(deftest expr-compile
  (c/register-expression-compiler
   :ui (fn [_ _ _ v] (constantly v)))
  (let [spec [:ui
              [:div
               "Name: " [:input {:type "text"
                                 :on-change [:set :Name]}]
               [:br]
               "Age: " [:input {:type "text"
                                :on-change [:set :Age]}]
               [:br]
               [:input {:type "button" :value "OK"
                        :on-click {:ExprCompile/SayHello
                                   {:Name :Name
                                    :Age :Age}}}]]]]
    (defcomponent :ExprCompile
      (entity
       :ExprCompile/Form
       {:Name :Kernel/String
        :Age :Kernel/Int
        :Spec
        {:type :Kernel/Edn
         :expr spec}})
      (dataflow
       :ExprCompile/ShowForm
       {:ExprCompile/Form
        {:Name "xyz" :Age 20}}))
    (let [e (tu/first-result
             (cn/make-instance
              {:ExprCompile/ShowForm
               {:Name "xyz"
                :Age 20}}))]
      (is (cn/instance-of? :ExprCompile/Form e))
      (is (= spec (:Spec e))))))

(deftest relationships
  (defcomponent :Relationships
    (entity
     :Relationships/User
     {:UserName :Kernel/String})
    (entity
     :Relationships/Book
     {:Title :Kernel/String
      :Author :Kernel/String})
    (relationship
     :Relationships/CheckoutBook
     {:User {:ref :Relationships/User.Id}
      :Book {:ref :Relationships/Book.Id}
      :Date {:type :Kernel/DateTime
             :default dt/now}
      :meta
      {:from :User
       :to :Book
       :cardinality
       {:type :1-M
        :exclusive true}}}))
  (let [u1 (tu/first-result
            {:Relationships/Upsert_User
             {:Instance
              {:Relationships/User
               {:UserName "K R"}}}})
        b1 (tu/first-result
            {:Relationships/Upsert_Book
             {:Instance
              {:Relationships/Book
               {:Title "ABC"
                :Author "JJK"}}}})
        r1 (tu/first-result
            {:Relationships/Upsert_CheckoutBook
             {:Instance
              {:Relationships/CheckoutBook
               {:User (:Id u1)
                :Book (:Id b1)}}}})
        r2 (tu/first-result
            {:Relationships/Lookup_CheckoutBook
             {:Id (:Id r1)}})]
    ;; TODO: implement and test relationships API functions.
    (is (cn/instance-of? :Relationships/User u1))
    (is (cn/instance-of? :Relationships/Book b1))
    (is (cn/instance-of? :Relationships/CheckoutBook r1))
    (is (cn/same-instance? r1 r2))))
