(ns fractl.resolver.root
  "The default resolver implementation, the root of a normal resolver sequence."
  (:require [fractl.resolver.protocol :as p]
            [fractl.env :as env]
            [fractl.namespace :as n]
            [fractl.resolver.parser :as parser]
            [fractl.store :as store]
            [fractl.util :as u]
            [fractl.lang.internal :as li]))

(defn- assoc-fn-attributes [env raw-obj fns]
  (loop [fns fns, raw-obj raw-obj]
    (if-let [[a f] (first fns)]
      (recur (rest fns) (assoc raw-obj a (f env raw-obj)))
      raw-obj)))

(defn- assoc-computed-attributes [env record-name raw-obj]
  (let [[efns qfns] (n/all-computed-attribute-fns record-name)
        f (partial assoc-fn-attributes env)]
    (f (f raw-obj efns) qfns)))

(defn- set-obj-attr
  ([env attr-name attr-value stk]
   (let [[env [n obj]] (or stk (env/pop-obj env))
         newobj (assoc obj attr-name attr-value)
         env (env/push-obj env n newobj)]
     (p/ok newobj env)))
  ([env attr-name attr-value]
   (set-obj-attr env attr-name attr-value nil)))

(defn- bind-if-instance [env x]
  (if (n/an-instance? x)
    (env/bind-instance env (li/split-path (n/instance-name x)) x)
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
        inst (n/make-instance (li/make-path record-name) final-obj)
        env (env/bind-instance env record-name
                               (if store
                                 (store/upsert-instance store record-name inst)
                                 inst))]
    [inst env]))

(defn make [store df-eval]
  (reify p/Resolver
    (do-match-instance [_ env [pattern instance]]
      (if-let [updated-env (parser/match-pattern env pattern instance)]
        (p/ok true updated-env)
        (p/ok false env)))

    (do-load-instance [_ env record-name]
      (if-let [inst (env/lookup-instance env record-name)]
        (p/ok inst env)
        p/not-found))

    (do-load-references [_ env [record-name refs]]
      (let [inst (env/lookup-instance env record-name)]
        (if-let [v (get (n/instance-attributes inst) (first refs))]
          (p/ok v (bind-if-instance env v))
          p/not-found)))

    (do-new-instance [_ env record-name]
      (let [env (env/push-obj env record-name)]
        (p/ok record-name env)))

    (do-query-instance [_ env [entity-name query-attrs]]
      (if-let [[inst env] (find-instance env store entity-name query-attrs)]
        (p/ok inst (env/push-obj env entity-name inst))
        p/not-found))

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
        (p/ok inst env)))

    (do-intern-event-instance [_ env record-name]
      (let [[inst env] (pop-and-intern-instance env nil record-name)]
        (p/ok (df-eval inst) env)))))

(def ^:private default-resolver (u/make-cell))

(defn get-default-resolver [df-eval]
  (u/safe-set-once
   default-resolver
   #(let [store (store/get-default-store)]
      (make store df-eval))))
