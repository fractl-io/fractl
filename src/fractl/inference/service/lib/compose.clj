(ns fractl.inference.service.lib.compose
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [fractl.util :as u]
            [stringer.core :as stringer]))

(def verbose-flag false)

(defn verbose [context-name msg & more]
  (when verbose-flag
    (let [thread-id (.threadId (Thread/currentThread))
          context (stringer/nrender "\n[{context-name}-{thread-id}]"
                                    {:context-name context-name
                                     :thread-id thread-id})]
      (apply println context msg more))))

(defn applyk [f & context-keys]
  (fn [context]
    (->> context-keys
         (map #(get context %))
         (apply f))))

(defn fnk-argcheck [argmap keywords]
  (when-not (map? argmap)
    (throw (ex-info (str "Expected a map as argument, got " (class argmap))
                    {:found argmap})))
  (doseq [each-key keywords]
    (when-not (contains? argmap each-key)
      (throw (ex-info (str "Keyword " each-key " not present in args")
                      {:missing-key each-key
                       :found-keys (keys argmap)})))))

(defmacro fnk [fn-name? symvec & body]
  (let [[fn-name
         symvec
         body] (if (symbol? fn-name?)
                 [(with-meta fn-name? {:type fn-name?}) symvec body]
                 [(gensym "fnk-") fn-name? (cons symvec body)])]
    (assert (vector? symvec))
    (assert (every? symbol? symvec))
    (let [keywords (mapv keyword symvec)]
      `(fn ~fn-name [argmap#]
         (fnk-argcheck argmap# ~keywords)
         (let [{:keys ~symvec} argmap#]
           ~@body)))))

(defn assok
  "Return a function that accepts a map and key/function pairs. Invoking the
  returned function assoc's the K/(f m) pairs into the map argument."
  ([k f] (fn [m]
           (assoc m k (f m))))
  ([k f & kfs] (fn [m]
                 (let [kfs-count (count kfs)
                       assok* (fn [m k f] (assoc m k (f m)))]
                   (when (odd? kfs-count)
                     (throw (IllegalArgumentException.
                              "assok expects even number of arguments, found odd number")))
                   (loop [result (assok* m k f)
                          remain (seq kfs)]
                     (if (nil? remain)
                       result
                       (recur (assok* result (first remain) (second remain))
                              (nnext remain))))))))

(defn assok-in
  ([ks f] (fn [m]
            (assoc-in m ks (f m))))
  ([ks f & more] (fn [m]
                   (let [more-count (count more)
                         assok-in* (fn [m ks f] (assoc-in m ks (f m)))]
                     (when (odd? more-count)
                       (throw (IllegalArgumentException.
                                "assok-in expects even number of arguments, found odd number")))
                     (loop [result (assok-in* m ks f)
                            remain (seq more)]
                       (if (nil? remain)
                         result
                         (recur (assok-in* result (first remain) (second remain))
                                (nnext remain))))))))

(defn report-chain-progress [chain-name verbose-label old-result new-result]
  (verbose chain-name
           "Result of" verbose-label
           (str "\n\n"
                (cond
                  ;; old and new result are maps
                  (and (map? old-result) (map? new-result))
                  (let [old-keys (set (keys old-result))
                        new-keys (set (keys new-result))
                        ks-added   (set/difference new-keys old-keys)
                        ks-removed (set/difference old-keys new-keys)
                        ks-common  (-> (concat ks-added ks-removed)
                                       set
                                       (remove (keys new-result)))
                        ks-updated (remove #(= (get old-result %)
                                               (get new-result %))
                                           ks-common)
                        ks->report (fn [label ks]
                                     (map (fn [k]
                                            (->> (get new-result k)
                                                 u/pretty-str
                                                 (format "(%s) %s => %s\n" label k)))
                                          ks))]
                    (->> (concat (ks->report "Updated" ks-updated)
                                 (ks->report "Added" ks-added))
                         (cons (when (seq ks-removed)
                                 ["Removed keys: " ks-removed "\n"]))
                         (remove nil?)
                         (map #(apply str %))
                         (s/join "\n")))
                  :else
                  (if (or (map? new-result) (vector? new-result))
                    (u/pretty-str new-result)
                    new-result)))))

(defn make-chain [chain-options expr-fn-pairs]
  (let [{:keys [chain-name
                max-retries]
         :or {max-retries 0}} chain-options
        verbose-chain-name (volatile! chain-name)
        chain-function (fn [args]
                         (verbose @verbose-chain-name "Entered chain with" expr-fn-pairs)
                         (loop [all-pairs expr-fn-pairs
                                last-result args]
                           (if (empty? all-pairs)
                             last-result
                             (let [[expr func] (first all-pairs)
                                   verbose-label (or (:type (meta func)) expr)]
                               (verbose @verbose-chain-name "Executing" verbose-label)
                               (let [new-result (func last-result)]
                                 (report-chain-progress @verbose-chain-name verbose-label last-result new-result)
                                 (recur (next all-pairs)
                                        new-result))))))
        retry-wrapped (if (pos? max-retries)
                        (fn [args]
                          (loop [retry-count 0]
                            (vreset! verbose-chain-name (str chain-name "-retry-" retry-count "/" max-retries))
                            (let [[value error] (try [(chain-function args) nil]
                                                      (catch Exception e
                                                        [nil e]))]
                              (if (some? error)
                                (if (> retry-count max-retries)
                                  (throw error)
                                  (recur (inc retry-count)))
                                value))))
                        chain-function)]
    (with-meta
      retry-wrapped
      {:type chain-name})))

(defmacro chain
  "Like clojure.core/comp, but accepts functions in natural (i.e. not reverse) order.
  Optional verbose logging of execution and result of intermediate steps."
  [chain-options expr & more]
  (let [all-exprs (vec (cons expr more))]
    `(let [all-fns# ~all-exprs
           expr-fn-pairs# (mapv vector '~all-exprs all-fns#)]
       (make-chain ~chain-options expr-fn-pairs#))))

(defn mpartial
  "Given a function (that accepts all arguments as a map) and arguments, return a partially applied function."
  [f argmap]
  (with-meta
    (fn [override-argmap]
      (let [final-argmap (merge argmap override-argmap)]
        (f final-argmap)))
    (meta f)))
