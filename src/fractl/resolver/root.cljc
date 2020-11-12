(ns fractl.resolver.root
  "The default resolver implementation, the root of a normal resolver sequence."
  (:require [fractl.resolver.internal :as i]
            [fractl.env :as env]
            [fractl.component :as cn]
            [fractl.resolver.parser :as parser]
            [fractl.store :as store]
            [fractl.util :as u]
            [fractl.util.seq :as su]
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

(defn- set-obj-attr
  ([env attr-name attr-value stk]
   (let [[env [n x]] (or stk (env/pop-obj env))
         single? (map? x)
         objs (if single? [x] x)
         new-objs (map #(assoc % attr-name attr-value) objs)
         env (env/push-objs env n new-objs)]
     (i/ok (if single? (first new-objs) new-objs) env)))
  ([env attr-name attr-value]
   (set-obj-attr env attr-name attr-value nil)))

(defn- bind-and-persist [env store x]
  (if (cn/an-instance? x)
    (let [n (li/split-path (cn/instance-name x))]
      (when (and store (cn/entity-instance? x))
        (store/upsert-instance store n x))
      (env/bind-instance env n x))
    env))

(defn- id-attribute [query-attrs]
  (first (filter #(= :Id (first %)) query-attrs)))

(defn- bind-query-results [env results]
  ;; TODO: bind query result objects to env
  env)

(defn- resolve-id-result [env r]
  (if (fn? r)
    (r env nil)
    [r env]))

(defn- resolve-id-query [env store query param running-result]
  (let [[p env] (resolve-id-result env param)
        rs (store/do-query store query [p])]
    [(bind-query-results env rs)
     (concat running-result (map su/first-val rs))]))

(defn- resolve-id-queries [env store id-queries]
  (loop [idqs id-queries, env env, result []]
    (if-let [idq (first idqs)]
      (if-let [r (:result idq)]
        (let [[obj new-env] (resolve-id-result env r)]
          (recur (rest idqs) new-env (conj result obj)))
        (let [[q p] (:query idq)
              [new-env rs] (resolve-id-query env store q p result)]
          (recur (rest idqs) new-env rs)))
      result)))

(defn- find-instance [env store entity-name queries]
  (when-let [id-results (seq (resolve-id-queries env store (:id-queries queries)))]
    (let [result (store/query-by-id store entity-name (:query queries) id-results)]
      [(if (= (count result) 1)
         (first result)
         result)
       (env/bind-instances env entity-name result)])))

(defn- pop-and-intern-instance [env store record-name]
  (let [[env [_ obj]] (env/pop-obj env)
        final-obj (assoc-computed-attributes env record-name obj)
        inst (if (cn/an-instance? final-obj)
               final-obj
               (cn/make-instance (li/make-path record-name) final-obj))
        env (env/bind-instance env record-name
                               (if store
                                 (store/upsert-instance store record-name inst)
                                 inst))]
    [inst env]))

(defn make-root-vm [store eval-event-dataflows]
  (reify opc/VM
    (do-match-instance [_ env [pattern instance]]
      (if-let [updated-env (parser/match-pattern env pattern instance)]
        (i/ok true updated-env)
        (i/ok false env)))

    (do-load-instance [_ env record-name]
      (if-let [inst (env/lookup-instance env record-name)]
        (i/ok inst env)
        i/not-found))

    (do-load-references [_ env [record-name refs]]
      (let [inst (env/lookup-instance env record-name)]
        (if-let [v (get (cn/instance-attributes inst) (first refs))]
          (i/ok v (bind-and-persist env store v))
          i/not-found)))

    (do-new-instance [_ env record-name]
      (let [env (env/push-obj env record-name)]
        (i/ok record-name env)))

    (do-query-instance [_ env [entity-name queries]]
      (if-let [[inst env] (find-instance env store entity-name queries)]
        (i/ok inst (env/push-obj env entity-name inst))
        i/not-found))

    (do-set-literal-attribute [_ env [attr-name attr-value]]
      (set-obj-attr env attr-name attr-value))

    (do-set-ref-attribute [_ env [attr-name attr-ref]]
      (let [[obj env] (env/follow-reference env attr-ref)]
        (set-obj-attr env attr-name obj)))

    (do-set-compound-attribute [_ env [attr-name f]]
      (let [[_ obj :as stk] (env/pop-obj env)]
        (set-obj-attr env attr-name (f env obj) stk)))

    (do-intern-instance [_ env record-name]
      (let [[inst env] (pop-and-intern-instance env store record-name)]
        (i/ok inst env)))

    (do-intern-event-instance [_ env record-name]
      (let [[inst env] (pop-and-intern-instance env nil record-name)]
        (i/ok (eval-event-dataflows inst) env)))))

(defn make [store eval-event-dataflows]
  (i/make (make-root-vm store eval-event-dataflows) store))

(def ^:private default-resolver (u/make-cell))

(defn get-default-resolver
  ([eval-event-dataflows store-config]
   (u/safe-set-once
    default-resolver
    #(let [store (store/open-default-store store-config)]
       (make store eval-event-dataflows))))
  ([eval-event-dataflows]
   (get-default-resolver eval-event-dataflows nil)))
