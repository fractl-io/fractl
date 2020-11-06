(ns fractl.resolver.root
  "The default resolver implementation, the root of a normal resolver sequence."
  (:require [fractl.resolver.internal :as i]
            [fractl.env :as env]
            [fractl.component :as cn]
            [fractl.resolver.parser :as parser]
            [fractl.store :as store]
            [fractl.util :as u]
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
   (let [[env [n obj]] (or stk (env/pop-obj env))
         newobj (assoc obj attr-name attr-value)
         env (env/push-obj env n newobj)]
     (i/ok newobj env)))
  ([env attr-name attr-value]
   (set-obj-attr env attr-name attr-value nil)))

(defn- bind-if-instance [env x]
  (if (cn/an-instance? x)
    (env/bind-instance env (li/split-path (cn/instance-name x)) x)
    env))

(defn- id-attribute [query-attrs]
  (first (filter #(= :Id (first %)) query-attrs)))

(defn- find-instance [env store entity-name query-attrs]
  (if-let [inst (first (env/lookup-instances-by-attributes env entity-name query-attrs))]
    [inst env]
    ;; TODO: the current store lookup is limited to the `:Id` attribute, extend this to
    ;; support complex queries on all the given attributes.
    (when-let [inst (store/find-by-id store entity-name (id-attribute query-attrs))]
      [inst (env/bind-instance env entity-name inst)])))

(defn- pop-and-intern-instance [env store record-name]
  (let [[env [_ obj]] (env/pop-obj env)
        final-obj (assoc-computed-attributes env record-name obj)
        inst (cn/make-instance (li/make-path record-name) final-obj)
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
          (i/ok v (bind-if-instance env v))
          i/not-found)))

    (do-new-instance [_ env record-name]
      (let [env (env/push-obj env record-name)]
        (i/ok record-name env)))

    (do-query-instance [_ env [entity-name query-attrs]]
      (if-let [[inst env] (find-instance env store entity-name query-attrs)]
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

(defn get-default-resolver [eval-event-dataflows]
  (u/safe-set-once
   default-resolver
   #(let [store (store/get-default-store)]
      (make store eval-event-dataflows))))
