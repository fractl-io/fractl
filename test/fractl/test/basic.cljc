(ns fractl.test.basic
  #?(:clj (:use [fractl.lang]))
  (:require [clojure.test :refer [deftest is]]
            [fractl.test.util :as tu]
            [fractl.util :as u]
            [fractl.namespace :as n]
            [fractl.compiler :as c]
            [fractl.lang.opcode :as opc]
            [fractl.compiler.context :as ctx]
            [fractl.store :as store]
            [fractl.runtime.resolver :as r])
  #?(:cljs [fractl.lang
            :refer [namespace attribute event
                    entity record kernel-decimal?]
            :refer-macros [dataflow]]))

(defn- install-test-namespace []
  (n/remove-namespace :CompileTest)
  (namespace :CompileTest)
  (entity {:CompileTest/E1
           {:X :Kernel/Int
            :Y :Kernel/Int}}))

(defn- init-test-context []
  (install-test-namespace)
  (c/make-context))

(defn- compile-pattern [ctx pat]
  (:opcode (c/compile-pattern r/resolver-for-path ctx pat)))

(defn- pattern-compiler []
  (let [ctx (init-test-context)]
    [ctx (partial compile-pattern ctx)]))

(defn- valid-opcode? [opc-predic opcode v]
  (is (opc-predic opcode))
  (if (fn? v)
    (is (v (opc/arg opcode)))
    (is (= v (opc/arg opcode)))))

(def ^:private load-instance? (partial valid-opcode? opc/load-instance?))
(def ^:private match-inst? (partial valid-opcode? opc/match-instance?))

(deftest compile-path
  (let [[_ c] (pattern-compiler)
        p1 :CompileTest/E1
        p1e :CompileTest/E111
        p2 :CompileTest/Create_E1
        p2e :CompileTest/Create_E111]
    (load-instance? (c p1) [:CompileTest :E1])
    (tu/is-error (c p1e))
    (load-instance? (c p2) [:CompileTest :Create_E1])
    (tu/is-error (c p2e))))

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
                       (nth opcs 3) [:CompileTest :E1]))))

(deftest compile-pattern-02
  (let [[ctx c] (pattern-compiler)
        p1 {:CompileTest/E1
            {:Id? 'id
             :X 100
             :Y '(+ :X 10)}}
        uuid (u/uuid-str)]
    ;; Variable `id` not in context.
    (tu/is-error (c p1))
    ;; Any value will do, variable validation
    ;; will happen only during runtime.
    ;; In this case, the variable is resolved at
    ;; compile-time itself.
    (ctx/bind-variable! ctx 'id uuid)
    (let [opcs (c p1)]
      (is (valid-opcode? opc/query-instance?
                         (first opcs) [[:CompileTest :E1] [[:Id uuid]]]))
      (is (valid-opcode? opc/set-literal-attribute?
                         (second opcs) [:X 100]))
      (is (valid-opcode? opc/set-compound-attribute?
                         (nth opcs 2) (fn [[n f]]
                                        (and (= :Y n) (fn? f)))))
      (is (valid-opcode? opc/intern-instance?
                         (nth opcs 3) [:CompileTest :E1])))))

(deftest circular-dependency
  (let [[ctx c] (pattern-compiler)
        p1 {:CompileTest/E1
            {:Id? 'id
             :X '(+ :Y 20)
             :Y '(+ :X 10)}}
        uuid (u/uuid-str)]
    (ctx/bind-variable! ctx 'id uuid)
    ;; Compilation fail on cyclic-dependency
    (tu/is-error (c p1))))

(defmacro defnamespace [namespace-name & body]
  `(do (f ~namespace-name)
       ~@body
       (store/create-schema (store/get-default-store) ~namespace-name)
       ~namespace-name))

(deftest compile-ref
  (defnamespace :Df01
    (entity {:Df01/E
             {:X :Kernel/Int
              :Y :Kernel/Int}}))
  (let [e (n/make-instance :Df01/E {:X 10 :Y 20})
        evt (n/make-instance :Df01/Create_E {:Instance e})
        result (:result (first (r/eval-all-dataflows-for-event evt)))]
    (is (n/same-instance? e result))))

(deftest compile-create
  (defnamespace :Df02
    (entity {:Df02/E
             {:X :Kernel/Int
              :Y :Kernel/Int}})
    (record {:Df02/R {:A :Kernel/Int}})
    (event {:Df02/PostE {:R :Df02/R}}))
  (raw-dataflow :Df02/PostE
                {:Df02/E {:X :Df02/PostE.R.A
                          :Y '(* :X 10)}})
  (let [r (n/make-instance :Df02/R {:A 100})
        evt (n/make-instance :Df02/PostE {:R r})
        result (:result (first (r/eval-all-dataflows-for-event evt)))]
    (is (n/instance-of? :Df02/E result))
    (is (u/str->uuid (:Id result)))
    (is (= 100 (:X result)))
    (is (= 1000 (:Y result)))))

(deftest dependency
  (defnamespace :Df03
    (record {:Df03/R {:A :Kernel/Int}})
    (entity {:Df03/E {:X :Kernel/Int
                      :Y :Kernel/Int
                      :Z :Kernel/Int}})
    (event {:Df03/PostE {:R :Df03/R}}))
  (raw-dataflow :Df03/PostE
                {:Df03/E {:X :Df03/PostE.R.A
                          :Z '(+ :X :Y)
                          :Y '(* :X 10)}})
  (let [r (n/make-instance :Df03/R {:A 100})
        evt (n/make-instance :Df03/PostE {:R r})
        result (:result (first (r/eval-all-dataflows-for-event evt)))]
    (is (n/instance-of? :Df03/E result))
    (is (u/str->uuid (:Id result)))
    (is (= 100 (:X result)))
    (is (= 1000 (:Y result)))
    (is (= 1100 (:Z result)))))

(deftest compound-attributes
  (defnamespace :Df04
    (entity {:Df04/E1 {:A :Kernel/Int}})
    (entity {:Df04/E2 {:AId {:ref :Df04/E1.Id}
                       :X :Kernel/Int
                       :Y {:expr '(* :X :Df04/E1.A)}}})
    (event {:Df04/PostE2 {:E1 :Df04/E1}}))
  (raw-dataflow :Df04/PostE2
                {:Df04/E2 {:AId :Df04/PostE2.E1.Id
                           :X 500}})
  (let [e1 (n/make-instance :Df04/E1 {:A 100})
        id (:Id e1)
        e2 (n/make-instance :Df04/E2 {:AId id
                                          :X 20})
        evt (n/make-instance :Df04/PostE2 {:E1 e1})
        result (:result (first (r/eval-all-dataflows-for-event evt)))]
    (is (n/instance-of? :Df04/E2 result))
    (is (u/str->uuid (:Id result)))
    (is (= (:AId result) id))
    (is (= (:X result) 500))
    (is (= (:Y result) 50000))))

(deftest fire-event
  (defnamespace :Df05
    (entity {:Df05/E1 {:A :Kernel/Int}})
    (entity {:Df05/E2 {:B :Kernel/Int}})
    (event {:Df05/Evt01 {:E1 :Df05/E1}})
    (event {:Df05/Evt02 {:E1 :Df05/E1}})
    (raw-dataflow :Df05/Evt01
                  {:Df05/Evt02 {:E1 :Df05/Evt01.E1}})
    (raw-dataflow :Df05/Evt02
                  {:Df05/E2 {:B :Df05/Evt02.E1.A}}))
  (let [e1 (n/make-instance :Df05/E1 {:A 100})
        evt (n/make-instance :Df05/Evt01 {:E1 e1})
        result (:result (first (r/eval-all-dataflows-for-event evt)))
        inst (:result (first result))]
    (is (n/instance-of? :Df05/E2 inst))
    (is (= (:B inst) 100))))
