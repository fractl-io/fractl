(ns fractl.test.core-test
  "A basic cljs test."
  (:require [cljs.test :refer-macros [deftest is]]
            [fractl.util :as u]
            [fractl.component :as cn]
            [fractl.compiler :as c]
            [fractl.lang
             :refer [component attribute event
                     entity record dataflow]]
            [fractl.lang.opcode :as opc]
            [fractl.compiler.context :as ctx]
            [fractl.sql-store :as store]
            [fractl.resolver :as r]))

(deftest test-numbers
  (is (= 1 1)))

(defmacro is-error [exp]
  `(is (try
         (do ~exp false)
         (catch Exception ex#
           (println (str "Expected exception in test: " (js/Error ex#)))
           ex#))))

(defn- install-test-component []
  (cn/remove-component :CompileTest)
  (component :CompileTest)
  (entity {:CompileTest/E1
           {:X :Kernel/Int
            :Y :Kernel/Int}}))

(defn- init-test-context []
  (install-test-component)
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
    (is-error (c p1e))
    (load-instance? (c p2) [:CompileTest :Create_E1])
    (is-error (c p2e))))

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
        uuid (u/uuid-string)]
    ;; Variable `id` not in context.
    (is-error (c p1))
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
        uuid (u/uuid-string)]
    (ctx/bind-variable! ctx 'id uuid)
    ;; Compilation fail on cyclic-dependency
    (is-error (c p1))))

(defmacro defcomponent [component & body]
  `(do (component ~component)
       ~@body
       (store/create-schema (store/get-default-store) ~component)
       ~component))

(deftest compile-ref
  (defcomponent :Df01
    (entity {:Df01/E
             {:X :Kernel/Int
              :Y :Kernel/Int}}))
  (let [e (cn/make-instance :Df01/E {:X 10 :Y 20})
        evt (cn/make-instance :Df01/Create_E {:Instance e})
        result (:result (first (r/eval-all-dataflows-for-event evt)))]
    (is (cn/same-instance? e result))))
