(ns fractl.evaluator.root
  "The default evaluator implementation"
  (:require [clojure.walk :as w]
            [taoensso.timbre :as log]
            [fractl.env :as env]
            [fractl.async :as a]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.store :as store]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [fractl.evaluator.parser :as parser]
            [fractl.evaluator.internal :as i]
            [fractl.lang :as ln]
            [fractl.lang.opcode :as opc]
            [fractl.lang.internal :as li]))

(defn- assoc-fn-attributes [env raw-obj fns]
  (loop [fns fns, raw-obj raw-obj]
    (if-let [[a f] (first fns)]
      (recur (rest fns) (assoc raw-obj a (f env raw-obj)))
      raw-obj)))

(defn- assoc-computed-attributes [env record-name raw-obj]
  (let [[efns qfns] (cn/all-computed-attribute-fns record-name)
        f (partial assoc-fn-attributes env)]
    (f (f raw-obj efns) qfns)))

(defn- set-obj-attr [env attr-name attr-value]
  (if-let [xs (env/pop-obj env)]
    (let [[env single? [n x]] xs
          objs (if single? [x] x)
          new-objs (map #(assoc % attr-name (if (fn? attr-value)
                                              (attr-value env %)
                                              attr-value))
                        objs)
          env (env/push-obj env n (if single? (first new-objs) new-objs))]
      (i/ok (if single? (first new-objs) new-objs) (env/mark-all-dirty env new-objs)))
    (i/error (str "cannot set attribute value, invalid object state - " [attr-name attr-value]))))

(defn- call-function [env f]
  (if-let [xs (env/pop-obj env)]
    (let [[env single? [n x]] xs
          inst (if single? x (first x))]
      (i/ok (f env inst) env))
    (i/error "cannot call function, cannot find argument instance in stack")))

(defn- on-inst [f xs]
  (f (if (map? xs) xs (first xs))))

(defn- need-storage? [xs]
  (on-inst cn/entity-instance? xs))

(defn- resolver-for-instance [resolver insts]
  (let [path (on-inst cn/instance-name insts)]
    (rg/resolver-for-path resolver path)))

(defn- call-resolver-upsert [f env resolver composed? data]
  (let [rs (if composed? resolver [resolver])
        insts (if (map? data) [data] data)]
    (doall (map (fn [r]
                  (doall
                   (map #(merge % (:result (f r env %))) insts)))
                rs))))

(defn- call-resolver-delete [f env resolver composed? insts]
  (let [rs (if composed? resolver [resolver])]
    (doall (map (fn [r]
                  (:result (f r env insts)))
                rs))))

(defn- async-invoke [timeout-ms f]
  (cn/make-future (a/async-invoke f) timeout-ms))

(def ^:private resolver-upsert (partial call-resolver-upsert r/call-resolver-upsert))
(def ^:private resolver-delete (partial call-resolver-delete r/call-resolver-delete))

(defn- call-resolver-eval [resolver composed? env inst]
  (let [rs (if composed? resolver [resolver])]
    (doall (map #(r/call-resolver-eval % env inst) rs))))

(defn- fire-conditional-event [event-evaluator event-name instance]
  (let [[_ n] (li/split-path (cn/instance-name instance))
        upserted-inst (when-let [t (:transition instance)]
                        (:from t))
        upserted-n (li/upserted-instance-attribute n)
        evt (cn/make-instance
             event-name
             {n instance
              upserted-n upserted-inst})]
    (event-evaluator evt)))

(defn- fire-all-conditional-events [event-evaluator insts]
  (let [f (partial fire-conditional-event event-evaluator)]
    (filter
     identity
     (map #(seq (map (fn [e] (f e %)) (cn/conditional-events %)))
          insts))))

(def ^:private inited-components (u/make-cell []))

(defn- maybe-init-schema! [store component-name]
  (when-not (some #{component-name} @inited-components)
    (u/safe-set
     inited-components
     (do (store/create-schema store component-name)
         (conj @inited-components component-name))
     component-name)))

(defn- chained-crud [store-f res resolver-upsert single-arg-path insts]
  (let [insts (if (or single-arg-path (not (map? insts))) insts [insts])
        resolver (if single-arg-path
                   (rg/resolver-for-path res single-arg-path)
                   (resolver-for-instance res insts))
        composed? (rg/composed? resolver)
        crud? (or (not resolver) composed?)
        resolver-result (when resolver
                          (resolver-upsert resolver composed? insts))
        resolved-insts (if resolver (first resolver-result) insts)
        local-result (if (and crud? store-f
                              (or single-arg-path (need-storage? resolved-insts)))
                       (store-f resolved-insts)
                       resolved-insts)]
    [local-result resolver-result]))

(defn- chained-upsert [env event-evaluator record-name insts]
  (let [store (env/get-store env)
        resolver (env/get-resolver env)]
    (when store (maybe-init-schema! store (first record-name)))
    (if (env/any-dirty? env insts)
      (let [[local-result resolver-result]
            (chained-crud
             (when store (partial store/upsert-instances store record-name))
             resolver (partial resolver-upsert env) nil insts)
            conditional-event-results
            (fire-all-conditional-events
             event-evaluator local-result)]
        [(concat local-result conditional-event-results) resolver-result])
      [insts nil])))

(defn- delete-by-id [store record-name del-list]
  [record-name (store/delete-by-id store record-name (second (first del-list)))])

(defn- chained-delete [env record-name id]
  (let [store (env/get-store env)
        resolver (env/get-resolver env)]
    (chained-crud
     (when store (partial delete-by-id store record-name))
     resolver (partial resolver-delete env) record-name [[record-name id]])))

(defn- bind-and-persist [env event-evaluator x]
  (if (cn/an-instance? x)
    (let [n (li/split-path (cn/instance-name x))
          r (chained-upsert env event-evaluator n x)]
      [(env/bind-instance env n x) r])
    [env nil]))

(defn- id-attribute [query-attrs]
  (first (filter #(= :Id (first %)) query-attrs)))

(defn- evaluate-id-result [env r]
  (if (fn? r)
    (let [result (r env nil)]
      (if (vector? result) result [result env]))
    [r env]))

(defn- evaluate-id-query [env store query param running-result]
  (let [[p env] (evaluate-id-result env param)
        rs (store/do-query store query [p])]
    [(concat running-result (map su/first-val rs)) env]))

(defn- evaluate-id-queries
  "Evaluate unique IDs from queries into index tables. Each entry in the sequence id-queries will
   be a map with two possible keys - :result and :query. If there is a :result, that will be
   bound to an ID statically evaluated by the compiler. Otherwise, execute the query and find the ID.
   Return the final sequence of IDs."
  [env store id-queries]
  (loop [idqs id-queries, env env, result []]
    (if-let [idq (first idqs)]
      (if-let [r (:result idq)]
        (let [[obj env] (evaluate-id-result env r)]
          (recur (rest idqs) env (conj result obj)))
        (let [[q p] (:query idq)
              [rs env] (evaluate-id-query env store q p result)]
          (recur (rest idqs) env rs)))
      [result env])))

(defn- normalize-raw-where-clause [env where-clause]
  (loop [wcs where-clause, env env, final-wc []]
    (if-let [wc (first wcs)]
      (let [[r env] (evaluate-id-result env wc)]
        (recur (rest wcs) env (conj final-wc r)))
      [final-wc env])))

(defn- normalize-raw-query [env q]
  (let [[wc env] (let [where-clause (:where q)]
                   (if (seqable? where-clause)
                     (normalize-raw-where-clause env where-clause)
                     [where-clause env]))]
    [(assoc q :where wc) env]))

(defn- find-instances-via-composed-resolvers [env entity-name query resolvers]
  (loop [rs resolvers]
    (if-let [r (first rs)]
      (let [result (r/call-resolver-query r env [entity-name query])]
        (if (:result result)
          [result env]
          (recur (rest rs))))
      [nil env])))

(defn- find-instances-via-resolvers [env entity-name full-query]
  (if-let [resolver (rg/resolver-for-path entity-name)]
    (let [[q env] (normalize-raw-query env (:raw-query full-query))]
      (if (rg/composed? resolver)
        (find-instances-via-composed-resolvers env entity-name q resolver)
        [(r/call-resolver-query resolver env [entity-name q]) env]))
    [nil env]))

(defn- find-instances-in-store [env store entity-name full-query]
  (let [q (or (:compiled-query full-query)
              (store/compile-query store full-query))]
    (if (:query-direct q)
      [(store/do-query store (:raw-query full-query)
                       {:lookup-fn-params [env nil]})
       env]
      (let [idqs (seq (:id-queries q))
            [id-results env]
            (if idqs
              (evaluate-id-queries env store idqs)
              [nil env])]
        [(if (seq id-results)
           (store/query-by-id store entity-name (:query q) id-results)
           (when-not idqs
             (store/query-all store entity-name (:query q))))
         env]))))

(defn find-instances [env store entity-name full-query]
  (let [[r env] (find-instances-via-resolvers env entity-name full-query)
        resolver-result (seq (:result r))
        [result env] (if resolver-result
                       [resolver-result env]
                       (find-instances-in-store env store entity-name full-query))]
    [result (env/bind-instances env entity-name result)]))

(defn- validated-instance [record-name obj]
  (if (cn/an-instance? obj)
    (if-not (cn/entity-instance? obj)
      (cn/validate-instance obj)
      obj)
    (let [n (li/make-path record-name)
          validate? (if (cn/find-entity-schema n) false true)]
      (cn/make-instance n obj validate?))))

(defn- pop-instance
  "An instance is built in stages, the partial object is stored in a stack.
   Once an instance is realized, pop it from the stack and bind it to the environment."
  [env record-name]
  (if-let [xs (env/pop-obj env)]
    (let [[env single? [_ x]] xs
          objs (if single? [x] x)
          final-objs (map #(assoc-computed-attributes env record-name %) objs)
          insts (map (partial validated-instance record-name) final-objs)
          bindable (if single? (first insts) insts)]
      [bindable single? env])
    [nil false env]))

(defn- pop-and-intern-instance
  "An instance is built in stages, the partial object is stored in a stack.
   Once an instance is realized, pop it from the stack and bind it to the environment."
  [env record-name alias]
  (if-let [xs (env/pop-obj env)]
    (let [[env single? [_ x]] xs
          objs (if single? [x] x)
          final-objs (map #(assoc-computed-attributes env record-name %) objs)
          insts (map (partial validated-instance record-name) final-objs)
          env (env/bind-instances env record-name insts)
          bindable (if single? (first insts) insts)
          final-env (if alias (env/bind-instance-to-alias env alias bindable) env)]
      [bindable final-env])
    [nil env]))

(defn- pack-results [local-result resolver-results]
  [local-result resolver-results])

(defn- ok-result [r]
  (when (i/ok? r)
    (:result r)))

(defn- extract-local-result [r]
  (when (i/ok? r)
    (first (:result r))))

(defn- bind-result-to-alias [result-alias result]
  (if result-alias
    (let [env (:env result)
          r (ffirst (:result result))
          new-env (env/bind-instance-to-alias env result-alias r)]
      (assoc result :env new-env))
    result))

(defn- eval-opcode-list [evaluator env eval-opcode opcode-list]
  (loop [opcode-list opcode-list, env env result nil]
    (if-let [opcode (first opcode-list)]
      (let [result (eval-opcode evaluator env opcode)]
        (if (ok-result result)
          (recur (rest opcode-list)
                 (:env result)
                 result)
          result))
      result)))

(defn- eval-cases [evaluator env eval-opcode match-obj cases-code alternative-code result-alias]
  (bind-result-to-alias
   result-alias
   (loop [cases-code cases-code, env env]
     (if-let [[condition consequent] (first cases-code)]
       (let [result (eval-opcode evaluator env condition)
             r (ok-result result)]
         (if (not (nil? r))
           (if (= r match-obj)
             (eval-opcode-list evaluator (:env result) eval-opcode consequent)
             (recur (rest cases-code) (:env result)))
           result))
       (if (first alternative-code)
         (eval-opcode-list evaluator env eval-opcode alternative-code)
         (i/ok false env))))))

(defn- eval-for-each-body [evaluator env eval-opcode body-code element]
  (when (cn/entity-instance? element)
    (let [entity-name (cn/instance-name element)
          new-env (env/bind-instance env entity-name element)]
      (loop [body-code body-code, env new-env result nil]
        (if-let [opcode (first body-code)]
          (let [result (eval-opcode evaluator env opcode)]
            (if (ok-result result)
              (recur (rest body-code)
                     (:env result)
                     result)
              result))
          result)))))

(defn- eval-for-each [evaluator env eval-opcode collection body-code result-alias]
  (let [eval-body (partial eval-for-each-body evaluator env eval-opcode body-code)
        results (doall (map eval-body collection))]
    (if-let [failure (first (filter #(not (ok-result %)) results))]
      failure
      (let [eval-output (doall (map #(first (ok-result %)) results))
            result-insts (reduce concat eval-output)]
        (if result-alias
          (let [new-env (env/bind-to-alias env result-alias result-insts)]
            (i/ok result-insts new-env))
          (i/ok result-insts env))))))

(defn- opcode-data? [x]
  (if (vector? x)
    (opcode-data? (first x))
    (and (map? x) (:opcode x))))

(defn- set-quoted-list [opcode-eval elements-opcode]
  (first
   (w/prewalk
    #(if (opcode-data? %)
       (let [result (opcode-eval %)]
         (or (ok-result result)
             (u/throw-ex result)))
       %)
    elements-opcode)))

(defn- set-flat-list [opcode-eval elements-opcode]
  (loop [results (map opcode-eval elements-opcode), final-list []]
    (if-let [result (first results)]
      (if-let [r (ok-result result)]
        (recur (rest results) (conj final-list r))
        result)
      final-list)))

(defn- normalize-transitions [xs]
  (map #(if-let [t (:transition %)]
          (:to t)
          %)
       xs))

(defn make-root-vm
  "Make a VM for running compiled opcode. The is given a handle each to,
     - a store implementation
     - a evaluator for dataflows attached to an event
     - an evaluator for standalone opcode, required for constructs like :match"
  [eval-event-dataflows eval-opcode eval-dataflow]
  (reify opc/VM
    (do-match-instance [_ env [pattern instance]]
      (if-let [updated-env (parser/match-pattern env pattern instance)]
        (i/ok true updated-env)
        (i/ok false env)))

    (do-load-literal [_ env x]
      (i/ok x env))

    (do-load-instance [_ env [record-name alias]]
      (if-let [inst (if alias
                      (env/lookup-by-alias env alias)
                      (env/lookup-instance env record-name))]
        (i/ok inst env)
        (i/not-found record-name env)))

    (do-load-references [self env [[record-name alias] refs]]
      (if-let [[path v] (env/instance-ref-path env record-name alias refs)]
        (if (cn/an-instance? v)
          (let [final-inst (assoc-computed-attributes env (cn/instance-name v) v)
                event-evaluator (partial eval-event-dataflows self env)
                [env [local-result resolver-results :as r]]
                (bind-and-persist env event-evaluator final-inst)]
            (i/ok (if r (pack-results local-result resolver-results) final-inst) env))
          (if-let [store (env/get-store env)]
            (if (store/reactive? store)
              (i/ok (store/get-reference store path refs) env)
              (i/ok v env))
            (i/ok v env)))
        (i/not-found record-name env)))

    (do-new-instance [_ env record-name]
      (let [env (env/push-obj env record-name)]
        (i/ok record-name env)))

    (do-query-instances [_ env [entity-name queries]]
      (if-let [store (env/get-store env)]
        (if-let [[insts env] (find-instances env store entity-name queries)]
          (if (seq insts)
            (i/ok insts (env/mark-all-mint
                         (env/push-obj env entity-name insts)
                         insts))
            (i/not-found entity-name env))
          (i/not-found entity-name env))
        (i/error (str "Invalid query request for " entity-name " - no store specified"))))

    (do-set-literal-attribute [_ env [attr-name attr-value]]
      (set-obj-attr env attr-name attr-value))

    (do-set-list-attribute [self env [attr-name elements-opcode quoted?]]
      (try
        (let [opcode-eval (partial eval-opcode self env)
              final-list ((if quoted? set-quoted-list set-flat-list)
                          opcode-eval elements-opcode)]
          (set-obj-attr env attr-name (vec final-list)))
        #?(:clj
           (catch Exception e
             (or (ex-data e) (i/error (.getMessage e))))
           :cljs
           (catch js/Error e
             (or (.-ex-data e) (i/error e))))))

    (do-set-ref-attribute [_ env [attr-name attr-ref]]
      (let [[obj env] (env/follow-reference env attr-ref)]
        (set-obj-attr env attr-name obj)))

    (do-set-compound-attribute [_ env [attr-name f]]
      (set-obj-attr env attr-name f))

    (do-intern-instance [self env [record-name alias]]
      (let [[insts single? env] (pop-instance env record-name)]
        (if insts
          (let [event-eval (partial eval-event-dataflows self env)
                [local-result resolver-results] (chained-upsert env event-eval record-name insts)
                lr (normalize-transitions local-result)]
            (if-let [bindable (if single? (first lr) lr)]
              (let [env-with-inst (env/bind-instances env record-name lr)
                    final-env (if alias
                                (env/bind-instance-to-alias env-with-inst alias bindable)
                                env-with-inst)]
                (i/ok (pack-results local-result resolver-results) final-env))
              (i/ok (pack-results local-result resolver-results) env)))
          (i/not-found record-name env))))

    (do-intern-event-instance [self env [record-name alias timeout-ms]]
      (let [[inst env] (pop-and-intern-instance env record-name alias)
            resolver (resolver-for-instance (env/get-resolver env) inst)
            composed? (rg/composed? resolver)
            eval-env (env/make (env/get-store env) (env/get-resolver env))
            local-result (when (or (not resolver) composed?)
                           (async-invoke
                            timeout-ms
                            #(doall (extract-local-result (first (eval-event-dataflows self eval-env inst))))))
            resolver-results (when resolver
                               (call-resolver-eval resolver composed? env inst))]
        (i/ok (pack-results local-result resolver-results) env)))

    (do-delete-instance [self env [record-name id-pattern-code]]
      (let [result (eval-opcode self env id-pattern-code)]
        (if-let [id (ok-result result)]
          (let [[local-result resolver-results] (chained-delete env record-name id)]
            (i/ok (pack-results local-result resolver-results)
                  (env/purge-instance env record-name id)))
          result)))

    (do-call-function [_ env fnobj]
      (call-function env fnobj))

    (do-match [self env [match-pattern-code cases-code alternative-code result-alias]]
      (let [result (eval-opcode self env match-pattern-code)
            r (ok-result result)]
        (if (nil? r)
          result
          (eval-cases self (:env result) eval-opcode r cases-code alternative-code result-alias))))

    (do-eval-on [self env [evt-name df-code]]
      (let [df-eval (partial eval-dataflow self env)]
        (i/ok [:dispatch-on
               evt-name
               (fn [evt-name evt-body]
                (let [evt (cn/make-instance evt-name evt-body)]
                  (df-eval evt (list evt-name {:opcode (atom df-code)}))))]
              env)))

    (do-for-each [self env [bind-pattern-code body-code result-alias]]
      (let [result (eval-opcode self env bind-pattern-code)]
        (if-let [r (ok-result result)]
          (eval-for-each self (:env result) eval-opcode r body-code result-alias)
          result)))

    (do-entity-def [_ env schema]
      (let [n (li/record-name schema)
            [c _] (li/split-path n)
            old-c (cn/switch-component c)
            r (ln/entity schema)]
        (store/create-table (env/get-store env) n)
        (cn/switch-component old-c)
        (i/ok r env)))

    (do-pull [_ env options]
      (if-let [store (env/get-store env)]
        (i/ok (store/pull store (first options)) env)
        (i/error (str "pull failed - store not set in environment"))))

    (do-push [_ env options]
      (if-let [store (env/get-store env)]
        (i/ok (store/push store (first options)) env)
        (i/error (str "push failed - store not set in environment"))))))

(def ^:private default-evaluator (u/make-cell))

(defn get-default-evaluator [eval-event-dataflows eval-opcode eval-dataflow]
  (u/safe-set-once
   default-evaluator
   #(make-root-vm eval-event-dataflows eval-opcode eval-dataflow)))
