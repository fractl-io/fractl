(ns fractl.resolver.parser
  "Parsing and pattern matching during runtime."
  (:require [fractl.component :as cn]
            [fractl.lang.internal :as li]
            [fractl.env :as env]))

(defn- match-attribute-pattern [attrs env [pn pv]]
  (cond
    (li/literal? pv) (and (= (get attrs pn) pv) env)
    (symbol? pv) (if-let [[_ v] (env/lookup-variable env pv)]
                   (and (= (get attrs pn) v) env)
                   (env/bind-variable env pn (get attrs pn)))
    (li/name? pv) (and (= (get attrs pn) (env/lookup-instance env pv))
                               env)))

(defn- match-structure [env inst-name pattern instance]
  (when (and (map? pattern)
             (= inst-name (first (keys pattern))))
    (let [p (partial match-attribute-pattern (cn/instance-attributes instance))]
      (loop [pattrs (first (vals pattern)), result-env env]
        (if-let [pattr (first pattrs)]
          (when-let [updated-env (p result-env pattr)]
            (recur (rest pattrs) updated-env))
          result-env)))))

(defn match-pattern [env pattern instance]
  (let [n (li/split-path (cn/instance-name instance))
        updated-env (if (= n pattern)
                      env
                      (match-structure env n pattern instance))]
    (when updated-env
      (env/bind-instance updated-env n instance))))
