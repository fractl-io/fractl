(ns fractl.evaluator.root
  "The default evaluator implementation"
  (:require [clojure.walk :as w]
            [clojure.set :as set]
            [fractl.env :as env]
            [fractl.async :as a]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.store :as store]
            [fractl.store.util :as stu]
            [fractl.resolver.core :as r]
            [fractl.resolver.registry :as rg]
            [fractl.evaluator.parser :as parser]
            [fractl.evaluator.internal :as i]
            [fractl.lang :as ln]
            [fractl.lang.opcode :as opc]
            [fractl.lang.internal :as li]
            #?(:clj [clojure.core.async :refer [go <!]])
            #?(:cljs [cljs.core.async :refer [<!]]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

(defn- make-query-for-ref [env attr-schema ref-val]
  (let [r (:ref attr-schema)
        rec-name [(:component r) (:record r)]]
    [{:from rec-name
      :where [:= (first (:refs r)) ref-val]}
     [rec-name ref-val]]))

(defn- enrich-environment-with-refs
  "Find all entity instances referenced (via :ref attribute property)
  from this instance. Load them into the local environment so that
  compound attributes of this instance do not see broken reference values."
  [env record-name inst]
  (let [qs (mapv (fn [[k v]]
                   (make-query-for-ref
                    env (cn/find-attribute-schema v) (k inst)))
                 (cn/ref-attribute-schemas
                  (cn/fetch-schema record-name)))
        store (env/get-store env)]
    (loop [qs qs, env env]
      (if-let [q (first qs)]
        (let [c (store/compile-query store (first q))
              [rec-name ref-val] (second q)
              rs (store/do-query store (first c) [ref-val])]
          (recur (rest qs) (env/bind-instances env (stu/results-as-instances rec-name rs))))
        env))))

(defn- assoc-fn-attributes [env raw-obj fns]
  (loop [fns fns, raw-obj raw-obj]
    (if-let [[a f] (first fns)]
      (recur (rest fns) (assoc raw-obj a (f env raw-obj)))
      raw-obj)))

(defn- assoc-computed-attributes [env record-name raw-obj]
  (let [env (enrich-environment-with-refs env record-name raw-obj)
        [efns qfns] (cn/all-computed-attribute-fns record-name)
        f (partial assoc-fn-attributes env)]
    (f (f raw-obj efns) qfns)))

(defn- set-obj-attr [env attr-name attr-value]
  (if-let [xs (env/pop-obj env)]
    (let [[env single? [n x]] xs
          objs (if single? [x] x)
          new-objs (map
                    #(assoc
                      % attr-name
                      (if (fn? attr-value)
                        (attr-value env %)
                        attr-value))
                    objs)
          elem (if single? (first new-objs) new-objs)
          env (env/push-obj env n elem)]
      (i/ok elem (env/mark-all-dirty env new-objs)))
    (i/error (str "cannot set attribute value, invalid object state - " [attr-name attr-value]))))

(defn- call-function [env f]
  (if-let [xs (env/pop-obj env)]
    (let [[env single? [n x]] xs
          inst (if single? x (first x))]
      (i/ok (f env inst) env))
    (i/ok (f env nil) env)))

(defn- on-inst [f xs]
  (f (if (map? xs) xs (first xs))))

(defn- need-storage? [xs]
  (on-inst cn/entity-instance? xs))

(defn- resolver-for-instance [resolver insts]
  (let [path (on-inst cn/instance-name insts)]
    (rg/resolver-for-path resolver path)))

(def ^:private async-result-key :-*async-result*-)

(defn- merge-async-result [inst async-result]
  (assoc inst async-result-key async-result))

(defn- realize-async-result [eval-opcode env code result]
  (go
    (let [final-result
          (cond
            (map? result)
            (when-let [a (async-result-key result)]
              [(merge (dissoc result async-result-key) (<! a))])

            (seqable? result)
            (loop [xs result, r []]
              (if-let [x (first xs)]
                (recur
                 (rest xs)
                 (if-let [a (async-result-key x)]
                   (let [ar (<! a)]
                     (conj r (if (i/ok? (first ar))
                               (merge (dissoc x async-result-key) (:result ar))
                               (dissoc x async-result-key))))
                   (conj r x)))))
            :else (<! result))

          updated-env (env/bind-instances env final-result)]
      (eval-opcode updated-env code))))

(defn- process-resolver-upsert [resolver method env inst]
  (if-let [result (:result (method resolver env inst))]
    (if (map? result)
      (merge inst result)
      (merge-async-result inst result))
    inst))

(defn- call-resolver-upsert [f env resolver composed? data]
  (let [rs (if composed? resolver [resolver])
        insts (if (map? data) [data] data)]
    (mapv (fn [r]
            (mapv (partial process-resolver-upsert r f env) insts))
          rs)))

(defn- call-resolver-delete [f env resolver composed? insts]
  (let [rs (if composed? resolver [resolver])]
    (mapv (fn [r]
            (:result (f r env insts)))
          rs)))

(defn- async-invoke [timeout-ms f]
  (cn/make-future (a/async-invoke f) timeout-ms))

(def ^:private resolver-upsert (partial call-resolver-upsert r/call-resolver-upsert))
(def ^:private resolver-delete (partial call-resolver-delete r/call-resolver-delete))

(defn- call-resolver-eval [resolver composed? env inst]
  (let [rs (if composed? resolver [resolver])]
    (mapv #(r/call-resolver-eval % env inst) rs)))

(declare find-instances)

(defn- load-instances-for-conditional-event [env store where-clause
                                             records-to-load loaded-instances]
  (loop [wcs where-clause, rs records-to-load, env env, ls loaded-instances]
    (if-let [wc (first wcs)]
      (let [p (cn/parse-where-clause wc ls)
            [result env] (find-instances env store (:from p) p)]
        (recur (rest wcs) rs env (conj ls (first result))))
      [ls env])))

(defn- fire-conditional-event [event-evaluator env store event-info instance]
  (let [[_ event-name [where-clause records-to-load]] event-info
        upserted-inst (if-let [t (:transition instance)]
                        (:from t)
                        instance)
        new-inst (when-let [t (:transition instance)] (:to t))
        env (env/bind-instance env (or new-inst upserted-inst))
        [all-insts env] (load-instances-for-conditional-event
                         env store where-clause
                         records-to-load #{new-inst})]
    (when (cn/fire-event? event-info all-insts)
      (let [[_ n] (li/split-path (cn/instance-name upserted-inst))
            upserted-n (li/upserted-instance-attribute n)
            evt (cn/make-instance
                 event-name
                 {n new-inst
                  upserted-n upserted-inst})]
        (event-evaluator env evt)))))

(defn- fire-all-conditional-events [event-evaluator env store insts]
  (let [f (partial fire-conditional-event event-evaluator env store)]
    (filter
     identity
     (mapv #(seq (mapv (fn [e] (f e %)) (cn/conditional-events %)))
           insts))))

(def ^:private inited-components (u/make-cell [:Kernel]))

(defn- maybe-init-schema! [store component-name]
  (when-not (some #{component-name} @inited-components)
    (u/safe-set
     inited-components
     (do (store/create-schema store component-name)
         (conj @inited-components component-name))
     component-name)))

(defn- perform-rbac! [env opr recname data]
  (when-let [f (env/rbac-check env)]
    (when-not (f opr recname
                 (if (or (map? data) (string? data))
                   data
                   (first data)))
      (u/throw-ex (str "no permission to execute " opr " on " recname)))))

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
        final-result (if (and crud? store-f
                              (or single-arg-path (need-storage? resolved-insts)))
                       (store-f resolved-insts)
                       resolved-insts)]
    final-result))

(defn- chained-upsert [env event-evaluator record-name insts]
  (let [store (env/get-store env)
        resolver (env/get-resolver env)]
    (when store
      (maybe-init-schema! store (first record-name)))
    (if (env/any-dirty? env insts)
      (do
        (perform-rbac! env :Upsert record-name insts)
        (let [result
              (chained-crud
               (when store (partial store/upsert-instances store record-name))
               resolver (partial resolver-upsert env) nil insts)
              conditional-event-results
              (fire-all-conditional-events
               event-evaluator env store result)]
          (concat result conditional-event-results)))
      insts)))

(defn- delete-by-id [store record-name del-list]
  [record-name (store/delete-by-id store record-name (second (first del-list)))])

(defn- chained-delete [env record-name id]
  (perform-rbac! env :Delete record-name id)
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

(defn- evaluate-id-result [env rs]
  (loop [rs (if (or (not (seqable? rs)) (string? rs))
              [rs]
              rs)
         env env, values []]
    (if-let [r (first rs)]
      (cond
        (fn? r)
        (let [x (r env nil)
              [v new-env]
              (if (vector? x)
                x
                [x env])]
          (recur
           (rest rs)
           new-env
           (conj values v)))

        (vector? r)
        (let [[v new-env] (evaluate-id-result env r)]
          (recur (rest rs) new-env (conj values v)))

        :else
        (recur (rest rs) env (conj values r)))
      [values env])))

(defn- evaluate-id-query [env store query params running-result merge-operator]
  (let [[p env] (evaluate-id-result env params)
        rs (store/do-query store query p)
        all-ids (mapv su/first-val rs)]
    (if (and (seq running-result) (= :and merge-operator))
      [(set/intersection (set running-result) (set all-ids)) env]
      [(concat running-result all-ids) env])))

(defn- evaluate-id-queries
  "Evaluate unique IDs from queries into index tables. Each entry in the sequence id-queries will
   be a map with two possible keys - :result and :query. If there is a :result, that will be
   bound to an ID statically evaluated by the compiler. Otherwise, execute the query and find the ID.
   Return the final sequence of IDs. Merge operator is either :and or :or. This is used to build intersections
   or unions of ids."
  [env store id-queries merge-operator]
  (loop [idqs id-queries, env env, result []]
    (if-let [idq (first idqs)]
      (if-let [r (:result idq)]
        (let [[obj env] (evaluate-id-result env [r])]
          (recur (rest idqs) env (conj result (first obj))))
        (let [query (:query idq)
              [q p] [(first query) (seq (rest query))]
              [rs env] (evaluate-id-query env store q p result merge-operator)]
          (recur (rest idqs) env rs)))
      [result env])))

(defn- normalize-raw-query [env q]
  (let [[wc env] (let [where-clause (:where q)]
                   (if (seqable? where-clause)
                     (evaluate-id-result env where-clause)
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

(defn- filter-query-result [rule env result]
  (when (seq result)
    (let [predic #(rule
                   (fn [x]
                     (let [r (% x)]
                       (if (nil? r)
                         (env/lookup-instance (env/bind-instance env %) x)
                         r))))]
      (filter predic result))))

(defn- query-all [env store entity-name query]
  (cond
    (vector? query)
    (let [[params env] (evaluate-id-result env (rest query))]
      [(stu/results-as-instances
        entity-name
        (store/do-query store (first query) params))
       env])

    (or (string? query) (map? query))
    [(store/query-all store entity-name query) env]

    :else
    (u/throw-ex (str "invalid query object - " query))))

(defn- find-instances-in-store [env store entity-name full-query]
  (let [q (or (:compiled-query full-query)
              (store/compile-query store full-query))]
    (if (cn/relational-schema?)
      (query-all env store entity-name q)
      (let [rule (:filter full-query)
            filter-results
            (if rule
              (partial filter-query-result rule env)
              identity)]
        (if (:query-direct q)
          [(filter-results
            (store/do-query store (:raw-query full-query)
                            {:lookup-fn-params [env nil]}))
           env]
          (let [idqs (seq (:id-queries q))
                [id-results env]
                (if idqs
                  (evaluate-id-queries env store idqs (:merge-opr q))
                  [nil env])]
            [(if (seq id-results)
               (filter-results
                (store/query-by-id store entity-name (:query q) id-results))
               (when-not idqs
                 (filter-results
                  (query-all env store entity-name (:query q)))))
             env]))))))

(defn- maybe-async-channel? [x]
  (and x (not (seqable? x))))

(defn find-instances [env store entity-name full-query]
  (let [[r env] (find-instances-via-resolvers env entity-name full-query)
        x (:result r)
        ch? (maybe-async-channel? x)
        resolver-result (if ch? x (seq x))
        [result env] (if resolver-result
                       [resolver-result env]
                       (find-instances-in-store env store entity-name full-query))]
    (if ch?
      [result env]
      (do
        (perform-rbac! env :Lookup entity-name result)
        [result (env/bind-instances env entity-name result)]))))

(defn- require-validation? [n]
  (if (or (cn/find-entity-schema n)
          (cn/find-event-schema n)
          (cn/find-record-schema n))
    true
    false))

(defn- validated-instance [record-name obj]
  (if (cn/an-instance? obj)
    (if-not (cn/entity-instance? obj)
      (cn/validate-instance obj)
      obj)
    (cn/make-instance
     record-name obj
     (require-validation? (li/make-path record-name)))))

(defn- pop-instance
  "An instance is built in stages, the partial object is stored in a stack.
   Once an instance is realized, pop it from the stack and bind it to the environment."
  [env record-name]
  (if-let [xs (env/pop-obj env)]
    (let [[env single? [_ x]] xs]
      (if (maybe-async-channel? x)
        [x single? env]
        (let [objs (if single? [x] x)
              final-objs (mapv #(assoc-computed-attributes env record-name %) objs)
              insts (mapv (partial validated-instance record-name) final-objs)
              bindable (if single? (first insts) insts)]
          [bindable single? env])))
    [nil false env]))

(defn- pop-and-intern-instance
  "An instance is built in stages, the partial object is stored in a stack.
   Once an instance is realized, pop it from the stack and bind it to the environment."
  [env record-name alias]
  (if-let [xs (env/pop-obj env)]
    (let [[env single? [_ x]] xs
          objs (if single? [x] x)
          final-objs (mapv #(assoc-computed-attributes env record-name %) objs)
          insts (mapv (partial validated-instance record-name) final-objs)
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
          r (if (false? (:result result))
              result
              (:result result))
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

(defn- eval-condition [evaluator env eval-opcode conds alternative result-alias]
  (bind-result-to-alias
   result-alias
   (let [arg (partial env/lookup-instance env)]
     (loop [main-clauses conds]
       (if-let [[condition body] (first main-clauses)]
         (if (condition arg)
           (eval-opcode evaluator env body)
           (recur (rest main-clauses)))
         (if alternative
           (eval-opcode evaluator env alternative)
           (i/ok false env)))))))

(defn- bind-for-each-element [env element elem-alias]
  (cond
    elem-alias
    (env/bind-to-alias env elem-alias element)

    (cn/an-instance? element)
    (env/bind-instance env (cn/instance-name element) element)

    :else
    (env/bind-to-alias env :% element)))

(defn- eval-for-each-body [evaluator env eval-opcode body-code elem-alias element]
  (let [new-env (bind-for-each-element env element elem-alias)]
    (loop [body-code body-code, env new-env result nil]
      (if-let [opcode (first body-code)]
        (let [result (eval-opcode evaluator env opcode)]
          (if (ok-result result)
            (recur (rest body-code)
                   (:env result)
                   result)
            result))
        result))))

(defn- eval-for-each [evaluator env eval-opcode collection body-code elem-alias result-alias]
  (let [eval-body (partial eval-for-each-body evaluator env eval-opcode body-code elem-alias)
        results (mapv eval-body collection)]
    (if (every? #(ok-result %) results)
      (let [eval-output (mapv #(ok-result %) results)
            result-insts (if (seq? (first eval-output))
                           (reduce concat eval-output)
                           eval-output)]
        (if result-alias
          (let [new-env (env/bind-to-alias env result-alias result-insts)]
            (i/ok result-insts new-env))
          (i/ok result-insts env)))
      (first (filter #(not (ok-result %)) results)))))

(defn- opcode-data? [x]
  (if (vector? x)
    (opcode-data? (first x))
    (and (map? x) (:opcode x))))

(defn- set-quoted-list [opcode-eval elements-opcode]
  (w/prewalk
   #(if (opcode-data? %)
      (let [result (opcode-eval %)]
        (or (ok-result result)
            (u/throw-ex result)))
      %)
   elements-opcode))

(defn- set-flat-list [opcode-eval elements-opcode]
  (loop [results (mapv opcode-eval elements-opcode), final-list []]
    (if-let [result (first results)]
      (if-let [r (ok-result result)]
        (recur (rest results) (conj final-list r))
        result)
      final-list)))

(defn- normalize-transitions [xs]
  (mapv #(if-let [t (:transition %)]
           (:to t)
           %)
        xs))

(defn- call-with-exception-as-error [f]
  (try
    (f)
    #?(:clj
       (catch Exception e
         (or (ex-data e) (i/error (.getMessage e))))
       :cljs
       (catch js/Error e
         (or (.-ex-data e) (i/error e))))))

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
                event-evaluator (partial eval-event-dataflows self)
                [env r]
                (bind-and-persist env event-evaluator final-inst)]
            (i/ok (if r r final-inst) env))
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
          (do
            (cond
              (maybe-async-channel? insts)
              (i/ok insts (env/push-obj env entity-name insts))

              (seq insts)
              (i/ok insts (env/mark-all-mint
                           (env/push-obj env entity-name insts)
                           insts))

              :else
              (i/not-found entity-name env)))
          (i/not-found entity-name env))
        (i/error (str "Invalid query request for " entity-name " - no store specified"))))

    (do-set-literal-attribute [_ env [attr-name attr-value]]
      (set-obj-attr env attr-name attr-value))

    (do-set-list-attribute [self env [attr-name elements-opcode quoted?]]
      (call-with-exception-as-error
       #(let [opcode-eval (partial eval-opcode self env)
              final-list ((if quoted? set-quoted-list set-flat-list)
                          opcode-eval elements-opcode)]
          (set-obj-attr env attr-name final-list))))

    (do-set-ref-attribute [_ env [attr-name attr-ref]]
      (let [[obj env] (env/follow-reference env attr-ref)]
        (set-obj-attr env attr-name obj)))

    (do-set-compound-attribute [_ env [attr-name f]]
      (set-obj-attr env attr-name f))

    (do-intern-instance [self env [record-name alias]]
      (let [[insts single? env] (pop-instance env record-name)
            scm (cn/ensure-schema record-name)]
        (doseq [inst insts]
          (when-let [attrs (cn/instance-attributes inst)]
            (cn/validate-record-attributes
             record-name attrs scm)))
        (cond
          (maybe-async-channel? insts)
          (i/ok insts env)

          insts
          (let [event-eval (partial eval-event-dataflows self)
                local-result (chained-upsert env event-eval record-name insts)
                lr (normalize-transitions local-result)]
            (if-let [bindable (if single? (first lr) lr)]
              (let [env-with-inst (env/bind-instances env record-name lr)
                    final-env (if alias
                                (env/bind-instance-to-alias env-with-inst alias bindable)
                                env-with-inst)]
                (i/ok local-result final-env))
              (i/ok local-result env)))

          :else
          (i/not-found record-name env))))

    (do-intern-event-instance [self env [record-name alias timeout-ms]]
      (let [[inst env] (pop-and-intern-instance env record-name alias)
            resolver (resolver-for-instance (env/get-resolver env) inst)
            composed? (rg/composed? resolver)
            eval-env (env/make (env/get-store env) (env/get-resolver env))
            local-result (when (or (not resolver) composed?)
                           (async-invoke
                            timeout-ms
                            #(doall
                              (extract-local-result
                               (first (eval-event-dataflows self eval-env inst))))))
            resolver-results (when resolver
                               (call-resolver-eval resolver composed? env inst))]
        (i/ok (pack-results local-result resolver-results) env)))

    (do-delete-instance [self env [record-name id-pattern-code]]
      (let [result (eval-opcode self env id-pattern-code)]
        (if-let [id (ok-result result)]
          (let [local-result (chained-delete env record-name id)]
            (i/ok local-result
                  (env/purge-instance env record-name id)))
          result)))

    (do-call-function [_ env fnobj]
      (call-function env fnobj))

    (do-match [self env [match-pattern-code cases-code alternative-code result-alias]]
      (if match-pattern-code
        (let [result (eval-opcode self env match-pattern-code)
              r (ok-result result)]
          (if (nil? r)
            result
            (eval-cases self (:env result) eval-opcode r cases-code alternative-code result-alias)))
        (eval-condition self env eval-opcode cases-code alternative-code result-alias)))

    (do-try_ [self env [body handlers]]
      (let [result (call-with-exception-as-error
                    #(eval-opcode self env body))
            h ((:status result) handlers)]
        (if h
          (eval-opcode self (or (:env result) env) h)
          result)))

    (do-await_ [self env [body continuation]]
      (do
        (go
          (let [result (call-with-exception-as-error
                        #(eval-opcode self env body))
                h ((:status result) continuation)]
            (realize-async-result
             (partial eval-opcode self)
             (:env result) h (:result result))))
        (i/ok [:await :ok] env)))

    (do-for-each [self env [bind-pattern-code elem-alias body-code result-alias]]
      (let [result (eval-opcode self env bind-pattern-code)]
        (if-let [r (ok-result result)]
          (eval-for-each self (:env result) eval-opcode r body-code elem-alias result-alias)
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
