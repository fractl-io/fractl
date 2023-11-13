(ns fractl.lang.tools.repl
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.string :as s]
            [fractl.lang :as ln]
            [fractl.lang.internal :as li]
            [fractl.lang.name-util :as nu]
            [fractl.lang.tools.loader :as loader]
            [fractl.component :as cn]
            [fractl.evaluator :as ev]
            [fractl.store :as store]
            [fractl.util :as u]
            [fractl.util.logger :as log]
            [fractl.env :as env]
            [fractl.global-state :as gs]))

(def ^:private repl-component :Fractl.Kernel.Repl)
(def ^:private components (atom {}))
(def ^:private active-component (atom nil))

(defn- set-declared-names! [cn decl-names]
  (swap! components assoc cn decl-names)
  (reset! active-component cn))

(defn- add-declared-name! [cn n]
  (let [names (get @components cn #{})]
    (swap! components assoc cn (conj names n))))

(defn- declared-names [cn]
  (when-let [decl-ns (get @components cn)]
    {:component cn :records decl-ns}))

(defn- invoke-eval [env evaluator evobj]
  (evaluator
   (if env
     (cn/assoc-event-context-env env evobj)
     evobj)))

(defn- event-instance? [obj]
  (and (map? obj)
       (cn/event? (li/record-name obj))))

(defn- eval-pattern-as-dataflow [env evaluator pat]
  (if (event-instance? pat)
    (invoke-eval env evaluator pat)
    (let [evt-name (li/make-path repl-component (li/unq-name))]
      (ln/event evt-name {})
      (try
        (let [evobj {evt-name {}}]
          (ln/dataflow evt-name pat)
          (invoke-eval env evaluator evobj))
        (finally
          (cn/remove-event evt-name))))))

(defn- evaluate-pattern [env-handle evaluator pat]
  (let [env @env-handle]
    (if (keyword? pat)
      (or (env/lookup env pat) pat)
      (try
        (let [res (eval-pattern-as-dataflow env evaluator pat)
              {env :env status :status result :result}
              (if (map? res) res (first res))]
          (if (= :ok status)
            (do (ev/fire-post-events env)
                (reset!
                 env-handle
                 (env/disable-post-event-triggers env))
                result)
            (or status pat)))
        (catch Exception ex
          (println (str "WARN - " (.getMessage ex)))
          pat)))))

(defn- proc-event-pattern [pat]
  (when (:as pat)
    (u/throw-ex "alias not supported for event-pattern"))
  pat)

(defn- reldef [exp]
  (if (map? (second exp))
    (li/record-attributes (second exp))
    (nth exp 2)))

(defn- need-schema-init? [exp]
  (let [n (first exp)]
    (or (= n 'entity)
        (= n 'relationship))))

(defn- maybe-reinit-schema [store exp]
  (when (and (seqable? exp) (need-schema-init? exp))
    (let [n (let [r (second exp)]
              (if (map? r)
                (li/record-name r)
                r))
          [c _] (li/split-path n)
          is-rel (= (first exp) 'relationship)
          is-ent (not is-rel)
          rd (when is-rel (reldef exp))]
      (when (or is-ent (:between (:meta rd)))
        (store/drop-entity store n))
      (when is-rel
        (when-let [spec (:contains (:meta rd))]
          (store/drop-entity store (second spec))))
      (store/force-init-schema store c)))
  exp)

(def ^:private lang-fns #{'entity 'relationship 'event 'record 'dataflow})

(defn- lang-defn? [exp]
  (and (seqable? exp)
       (some #{(first exp)} lang-fns)))

(defn- fetch-def-name [exp]
  (let [d (second exp)]
    (when-not (vector? d)
      (let [n (li/split-path
               (if (map? d)
                 (li/record-name d)
                 d))]
        (if (= 1 (count n))
          [nil (first n)]
          n)))))

(defn- maybe-preproc-expression [exp]
  (cond
    (lang-defn? exp)
    (if-let [[c n] (fetch-def-name exp)]
      (let [cn (or c @active-component)]
        (add-declared-name! cn n)
        (nu/fully-qualified-names (declared-names cn) exp))
      (nu/fully-qualified-names (declared-names @active-component) exp))

    (and (seqable? exp) (= 'component (first exp)))
    (do (reset! active-component (second exp)) exp)

    (or (map? exp) (vector? exp))
    (if-let [names (declared-names @active-component)]
      (nu/generic-fully-qualified-names names exp)
      exp)

    :else exp))

(defn- repl-eval [store env-handle evaluator exp]
  (try
    (let [exp (maybe-preproc-expression exp)
          r (eval exp)]
      (when (maybe-reinit-schema store exp)
        (cond
          (lang-defn? exp) r
          (or (map? r) (vector? r) (keyword? r))
          (evaluate-pattern env-handle evaluator r)
          :else r)))
    (catch Exception ex
      (println (str "ERROR - " (.getMessage ex))))))

(defn- infer-model-name []
  (try
    (:name (loader/load-default-model-info))
    (catch Exception ex
      (println (str "WARN - " (.getMessage ex)))
      :fractl)))

(defn- maybe-change-model-name [model-name exp]
  (if (and (seqable? exp) (= 'component (first exp)))
    (let [n (second exp)
          s (s/split (subs (str n) 1) #"\.")]
      (s/lower-case (first s)))
    model-name))

(defn run [model-name store evaluator]
  (let [model-name (or model-name (infer-model-name))
        reval (partial repl-eval store (atom nil) evaluator)
        current-cn (cn/get-current-component)
        decl-names (cn/declared-names current-cn)]
    (when decl-names
      (set-declared-names! current-cn decl-names))
    (use '[fractl.lang])
    (use '[fractl.lang.tools.replcmds])
    (ln/component repl-component)
    (when current-cn
      (reset! active-component current-cn)
      (cn/switch-component current-cn))
    (loop [model-name model-name]
      (print (str (name model-name) "> ")) (flush)
      (let [exp (try
                  (edn/read (java.io.PushbackReader.
                             (java.io.BufferedReader. *in*)))
                  (catch Exception ex
                    (println (str "ERROR in input - " (.getMessage ex)))))]
        (if exp
          (cond
            (= exp :quit)
            (do
              (println 'bye)
              (System/exit 0))

            (= exp '?)
            (do (pp/pprint
                 {:fractl-version (gs/fractl-version)
                  :components (vec (cn/component-names))})
                (recur model-name))

            :else
            (do
              (when-let [r (reval exp)]
                (pp/pprint r)
                (flush))
              (recur (maybe-change-model-name model-name exp))))
          (recur model-name))))))
