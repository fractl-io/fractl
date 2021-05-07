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
                     entity record dataflow]]
            [fractl.evaluator :as e]
            [fractl.lang.opcode :as opc]
            [fractl.compiler.context :as ctx]
            #?(:clj [fractl.test.util :as tu :refer [defcomponent]]
               :cljs [fractl.test.util :as tu :refer-macros [defcomponent]])))

#? (:clj
    (def store (store/open-default-store nil))
    :cljs
    (def store (store/open-default-store {:type :alasql})))

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
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
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
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
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
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
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
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :Bool/E result))
    (is (u/uuid-from-string (:Id result)))
    (is (= true (:X result)))
    (is (= true (:Y result))))
  (let [evt (cn/make-instance :Bool/PostE2 {:B false})
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
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
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :SelfRef/E result))
    (is (u/uuid-from-string (:Id result)))
    (is (= 100 (:X result)))
    (is (= 200 (:Y result)))
    (is (= 300 (:Z result)))
    (let [id (:Id result)
          addevt (cn/make-instance :SelfRef/AddToX {:EId id :Y 10})
          result (ffirst (tu/fresult (e/eval-all-dataflows addevt)))]
      (is (cn/instance-of? :SelfRef/E result))
      (is (u/uuid-from-string (:Id result)))
      (is (= 110 (:X result)))
      (is (= 10 (:Y result)))
      (is (= 1 (:Z result))))))

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
        e1 (ffirst (tu/fresult (e/eval-all-dataflows evt)))
        id (:Id e1)
        e2 (cn/make-instance :Df04/E2 {:AId id
                                       :X 20})
        evt (cn/make-instance :Df04/PostE2 {:E1 e1})
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
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
        evt {:CA/Upsert_E {:Instance e}}
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
    (assert-ca-e! result)
    (let [id (:Id result)
          evt {:CA/Lookup_E {:Id id}}
          result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
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
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :Df04_1/E result))
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
         result (ffirst (tu/fresult (e/eval-all-dataflows evt)))
         inst (ffirst (:result result))]
     (is (cn/instance-of? :Df05/E2 inst))
     (is (= (:B inst) 100)))))

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
     #(tu/fresult (e/eval-all-dataflows evt)))
    (let [evt (cn/make-instance :RefCheck/Upsert_E1 {:Instance e})
          e1 (ffirst (tu/fresult (e/eval-all-dataflows evt)))
          id (:Id e1)
          e2 (cn/make-instance :RefCheck/E2 {:AId id :X 20})
          evt (cn/make-instance :RefCheck/Upsert_E2 {:Instance e2})
          inst (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
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
        e1 (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
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
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
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
        evt {:H/Upsert_E {:Instance e}}
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))
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
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
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
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :MultiAlias/F result))
    (is (= 100 (:A result)))
    (is (= 10 (:B result)))))

(defn- conditional-event-01 [i x]
  (let [evt {:Cond/Evt {:I i}}
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
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
  (let [r01 (tu/fresult (e/eval-all-dataflows {:MA/Evt {:I 1}}))
        r02 (tu/fresult (e/eval-all-dataflows {:MA/Evt {:I 0}}))
        r03 (tu/fresult (e/eval-all-dataflows {:MA/Evt {:I 3}}))]
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
  (let [result (ffirst (tu/fresult (e/eval-all-dataflows {:AScope/Evt {:I 10}})))]
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
        e01 (ffirst (tu/fresult (e/eval-all-dataflows {:Del/Upsert_E {:Instance e}})))
        id (:Id e01)
        lookup-evt (cn/make-instance :Del/Lookup_E {:Id id})
        e02 (ffirst (tu/fresult (e/eval-all-dataflows lookup-evt)))
        del-result (e/eval-all-dataflows {:Del/Delete_E {:Id id}})
        r01 (second (first (tu/fresult del-result)))
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
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
    (assert-le result [1 2 3] 100))
  (let [evt {:L/MakeE0 {:Xs [10 20 30 40] :Y 1}}
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
    (assert-le result [10 20 30 40] 1))
  (try
    (let [evt {:L/MakeE0 {:Xs [10 "hi"] :Y 1}}
          result (tu/fresult (e/eval-all-dataflows evt))]
      (is false))
    (catch #?(:clj Exception :cljs :default) ex
      (is ex)))
  (let [evt {:L/MakeE1 {:X1 10 :X2 20 :Y 1}}
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
    (assert-le result [10 20] 1))
  (let [evt {:L/MakeE2 {:X 10 :Y 90}}
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
    (assert-le result [200 780 10] 90))
  (let [e (cn/make-instance :L/F {:Xs [10 "hi"] :Y 1})
        evt {:L/Upsert_F {:Instance e}}
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
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
        result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :OptRecAttr/E result))
    (is (u/uuid-from-string (:Id result)))
    (is (= 10 (:Q result)))))

(deftest inherits
  (defcomponent :Inherits
    (record {:Inherits/Base {:A {:type :Kernel/Int
                                 :optional true}
                             :B :Kernel/Int}})
    (entity {:Inherits/E {:X :Kernel/Int
                          :Y {:type :Kernel/Int
                              :optional true}
                          :S :Kernel/String
                          :inherits :Inherits/Base}})
    (record {:Inherits/BaseMeta {:A :Kernel/Int
                                 :B :Kernel/Int
                                 :meta {:required-attributes [:B]}}})
    (entity {:Inherits/F {:X :Kernel/Int
                          :Y {:type :Kernel/Int
                              :optional true}
                          :S :Kernel/String
                          :inherits :Inherits/BaseMeta}}))
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
                                   :inherits :MultiInherits/Base1}})
    (entity {:MultiInherits/E {:X :Kernel/Int
                               :Y {:type :Kernel/Int
                                   :optional true}
                               :S :Kernel/String
                               :inherits :MultiInherits/Base2}}))
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
                                       :inherits :MultiInheritsMeta/Base1}})
    (entity {:MultiInheritsMeta/E {:X :Kernel/Int
                                   :Y {:type :Kernel/Int
                                       :optional true}
                                   :S :Kernel/String
                                   :inherits :MultiInheritsMeta/Base2}}))
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
                           :Y {:expr '(+ :X 10)}
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
  (let [result (ffirst
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
        result (ffirst (tu/fresult (e/eval-all-dataflows add-btn)))]
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
              :DOM_Target :Kernel/String
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
                :DOM_Target "app"
                :UserLogin :EdnUI/UserLogin}})
    (let [evt (cn/make-instance {:EdnUI/MakeLoginForm
                                 {:FormTitle "Login to the V8 Platform"
                                  :UserNameLabel "Your V8 userId or email: "
                                  :PasswordLabel "Password: "
                                  :ButtonTitle "Login"
                                  :HandlerEvent :EdnUI/LoginEvent}})
          result (ffirst (tu/fresult (e/eval-all-dataflows evt)))]
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
         result (ffirst (tu/fresult (e/eval-all-dataflows evt01)))]
     (let [r (ffirst (:result result))]
       (is (cn/instance-of? :AE/R01 r))
       (is (= 100 (:X r)))))))
