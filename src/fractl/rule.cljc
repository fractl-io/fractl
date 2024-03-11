(ns fractl.rule
  (:require [fractl.util :as u]
            [fractl.env :as env]
            [fractl.component :as cn]
            [fractl.compiler.rule :as cr]
            [fractl.lang.internal :as li]))

(defn- opr-vect? [obj]
  (and (vector? obj)
       (cr/operator? (first obj))))

(defn- compile-opr-vect [v]
  (let [opr (first v)
        expr
        (case opr
          (:and :or)
          `(~(symbol (name opr)) ~@(map compile-opr-vect (rest v)))
          `(~(cr/operator-name opr) ~@(rest v) ~(symbol "arg")))]
    expr))

(defn- compile-opr-vect-as-fn [v]
  (li/evaluate `(fn [~(symbol "arg")] ~(compile-opr-vect v))))

(defn- predicated-attributes [attrs]
  (loop [attrs attrs, pattrs []]
    (if-let [[k v] (first attrs)]
      (let [pv (cond
                 (opr-vect? v) (compile-opr-vect-as-fn v)
                 (number? v) #(= v %)
                 :else #(= 0 (compare v %)))]
        (recur (rest attrs) (conj pattrs [k pv])))
      (into {} pattrs))))

(defn- compile-cond-pat [pat]
  ;; TODO: handle [:delete ...] pattern.
  (if (map? pat)
    (let [alias (:as pat)
          pat (if alias (dissoc pat :as) pat)
          recname (li/record-name pat)
          attrs (li/record-attributes pat)]          
      (when-not (cn/entity? recname)
        (u/throw-ex (str "undefined entity " recname " referenced in the rule " pat)))
      (when-not (map? attrs)
        (u/throw-ex (str "invalid attributes in pattern " attrs)))
      (let [pattrs (predicated-attributes attrs)]
        [recname
         (fn [env inst]
           (when (and (= recname (cn/instance-type-kw inst))
                      (every? (fn [[k v]] (v (k inst))) pattrs))
             (let [env (env/bind-instance env (li/split-path recname) inst)]
               (if alias
                 (env/bind-instance-to-alias env alias inst)
                 env))))]))
    (u/throw-ex (str "invalid conditional-pattern - " pat))))

(defn compile-conditionals [pats]
  (mapv compile-cond-pat pats))

(defn unify [compiled-cond-pats env inst]
  (loop [pats compiled-cond-pats, env env]
    (if-let [pat (first pats)]
      (when-let [current-env (pat env inst)]
        (recur (rest pats) current-env))
      env)))
