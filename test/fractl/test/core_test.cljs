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
            [fractl.test.util :as tu :refer-macros [defcomponent]]
            [fractl.eval :as e]))

(deftest test-numbers
  (is (= 1 1)))

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
  (:opcode (c/compile-pattern e/resolver-for-path ctx pat)))

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
    (is (thrown? js/Error (c p1e)))
    (load-instance? (c p2) [:CompileTest :Create_E1])
    (is (thrown? js/Error (c p2e)))))

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

#_(deftest compile-pattern-02
  (let [[ctx c] (pattern-compiler)
        p1 {:CompileTest/E1
            {:Id? 'id
             :X 100
             :Y '(+ :X 10)}}
        uuid (u/uuid-string)]
    ;; Variable `id` not in context.
    ;; (is (thrown? js/Error (c p1)))
    ;; Any value will do, variable validation
    ;; will happen only during runtime.
    ;; In this case, the variable is resolved at
    ;; compile-time itself.
    (ctx/bind-variable! ctx 'id uuid)
    (is (thrown? js/Error (c p1)))
    (let [opcs (c p1)]
      (print (str "Here lies opcs " opcs))
      (is (valid-opcode? opc/query-instance?
                         (first opcs) [[:CompileTest :E1] [[:Id uuid]]]))
      (is (valid-opcode? opc/set-literal-attribute?
                         (second @opcs) [:X 100]))
      (is (valid-opcode? opc/set-compound-attribute?
                         (nth @opcs 2) (fn [[n f]]
                                        (and (= :Y n) (fn? f)))))
      (is (valid-opcode? opc/intern-instance?
                         (nth @opcs 3) [:CompileTest :E1])))))

(deftest circular-dependency
  (let [[ctx c] (pattern-compiler)
        p1 {:CompileTest/E1
            {:Id? 'id
             :X '(+ :Y 20)
             :Y '(+ :X 10)}}
        uuid (u/uuid-string)]
    (ctx/bind-variable! ctx 'id uuid)
    ;; Compilation fail on cyclic-dependency
    (is (thrown? js/Error (c p1)))))

(deftest compile-ref
  (try
    (defcomponent :Df01
                  (entity {:Df01/E
                           {:X :Kernel/Int
                            :Y :Kernel/Int}}))
    (let [e (cn/make-instance :Df01/E {:X 10 :Y 20})
          evt (cn/make-instance :Df01/Create_E {:Instance e})
          result (tu/fresult (e/eval-all-dataflows-for-event evt))]
      ;; (with-out-str (println "Here lies some data: " evt))
      (is (cn/same-instance? e result)))
    (catch js/Error e
      (.log js/console (.-stack e)))))

;; (deftest compile-create
;;   (defcomponent :Df02
;;     (entity {:Df02/E
;;              {:X :Kernel/Int
;;               :Y :Kernel/Int}})
;;     (record {:Df02/R {:A :Kernel/Int}})
;;     (event {:Df02/PostE {:R :Df02/R}}))
;;   (dataflow :Df02/PostE
;;             {:Df02/E {:X :Df02/PostE.R.A
;;                       :Y '(* :X 10)}})
;;   (let [r (cn/make-instance :Df02/R {:A 100})
;;         evt (cn/make-instance :Df02/PostE {:R r})
;;         result (:result (first (r/eval-all-dataflows-for-event evt)))]
;;     (is (cn/instance-of? :Df02/E result))
;;     (is (u/uuid-from-string (:Id result)))
;;     (is (= 100 (:X result)))
;;     (is (= 1000 (:Y result)))))

;; (deftest dependency
;;   (defcomponent :Df03
;;     (record {:Df03/R {:A :Kernel/Int}})
;;     (entity {:Df03/E {:X :Kernel/Int
;;                       :Y :Kernel/Int
;;                       :Z :Kernel/Int}})
;;     (event {:Df03/PostE {:R :Df03/R}}))
;;   (dataflow :Df03/PostE
;;             {:Df03/E {:X :Df03/PostE.R.A
;;                       :Z '(+ :X :Y)
;;                       :Y '(* :X 10)}})
;;   (let [r (cn/make-instance :Df03/R {:A 100})
;;         evt (cn/make-instance :Df03/PostE {:R r})
;;         result (:result (first (r/eval-all-dataflows-for-event evt)))]
;;     (is (cn/instance-of? :Df03/E result))
;;     (is (u/uuid-from-string (:Id result)))
;;     (is (= 100 (:X result)))
;;     (is (= 1000 (:Y result)))
;;     (is (= 1100 (:Z result)))))

;; (deftest compound-attributes
;;   (defcomponent :Df04
;;     (entity {:Df04/E1 {:A :Kernel/Int}})
;;     (entity {:Df04/E2 {:AId {:ref :Df04/E1.Id}
;;                        :X :Kernel/Int
;;                        :Y {:expr '(* :X :Df04/E1.A)}}})
;;     (event {:Df04/PostE2 {:E1 :Df04/E1}}))
;;   (dataflow :Df04/PostE2
;;             {:Df04/E2 {:AId :Df04/PostE2.E1.Id
;;                        :X 500}})
;;   (let [e1 (cn/make-instance :Df04/E1 {:A 100})
;;         id (:Id e1)
;;         e2 (cn/make-instance :Df04/E2 {:AId id
;;                                           :X 20})
;;         evt (cn/make-instance :Df04/PostE2 {:E1 e1})
;;         result (:result (first (r/eval-all-dataflows-for-event evt)))]
;;     (is (cn/instance-of? :Df04/E2 result))
;;     (is (u/uuid-from-string (:Id result)))
;;     (is (= (:AId result) id))
;;     (is (= (:X result) 500))
;;     (is (= (:Y result) 50000))))

;; (deftest fire-event
;;   (defcomponent :Df05
;;     (entity {:Df05/E1 {:A :Kernel/Int}})
;;     (entity {:Df05/E2 {:B :Kernel/Int}})
;;     (event {:Df05/Evt01 {:E1 :Df05/E1}})
;;     (event {:Df05/Evt02 {:E1 :Df05/E1}})
;;     (dataflow :Df05/Evt01
;;               {:Df05/Evt02 {:E1 :Df05/Evt01.E1}})
;;     (dataflow :Df05/Evt02
;;               {:Df05/E2 {:B :Df05/Evt02.E1.A}}))
;;   (let [e1 (cn/make-instance :Df05/E1 {:A 100})
;;         evt (cn/make-instance :Df05/Evt01 {:E1 e1})
;;         result (:result (first (r/eval-all-dataflows-for-event evt)))
;;         inst (:result (first result))]
;;     (is (cn/instance-of? :Df05/E2 inst))
;;     (is (= (:B inst) 100))))
