(ns fractl.util.seq
  "Utilities for sequences.")

(defn truths
  "Return all truth values returned by f when applied to each element of xs."
  [f xs]
  (when (seq xs)
    (if-let [v (f (first xs))]
      (lazy-seq (cons v (truths f (rest xs))))
      (lazy-seq (truths f (rest xs))))))

(defn map-while
  "Apply f to each element in xs.
   Return false, if any of (f x) returns false for predic.
   Otherwise, return the value of (f x'), where x' is the last element of xs."
  [predic f xs]
  (loop [xs xs, r nil]
    (if (seq xs)
      (let [r (f (first xs))]
        (if (predic r)
          (recur (rest xs) r)
          false))
      r)))

(defn third [xs] (nth xs 2))

(defn- pick-by [posf xs]
  (loop [xs xs, result []]
    (if-let [x (posf xs)]
      (recur (nthrest xs 2) (conj result x))
      result)))

(def odds "Return the values at the odd positions in a sequence."
  (partial pick-by first))

(def evens "Return the values at the even positions in a sequence."
  (partial pick-by second))

(defn values [f xs]
  (loop [xs xs, result []]
    (if-let [x (first xs)]
      (if-let [r (f x)]
        (recur (rest xs) (conj result r))
        (recur (rest xs) result))
      result)))

(defn key-vals [m]
  [(keys m) (vals m)])

(defn unflat-map [xs]
  (into {} (map vec (partition 2 xs))))

(defn aconj [m tag x]
  (assoc m tag (conj (get m tag []) x)))

(defn seqs [xs]
  (filter seq xs))

(defn nonils [xs]
  (filter identity xs))

(defn conj-if [xs x]
  (if x (conj xs x) xs))

(defn vec-add-first [x vec]
  (apply conj [x] vec))

(defn first-val [m]
  (first (vals m)))

(defn move-all [xs target f]
  (loop [xs xs, target target]
    (if-let [x (first xs)]
      (recur (rest xs)
             (f target x))
      target)))

(defn map-mirror [m]
  (let [mm (map (fn [[k v]] [v k]) m)]
    (into {} mm)))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn contains-any [xs ys]
  "Return the first element from xs that exists also in ys.
   If no element from xs is found in ys, return nil."
  (loop [xs xs]
    (when-let [x (first xs)]
      (if (some #{x} ys)
        x
        (recur (rest xs))))))

(defn maybe-assoc [m k v]
  (if (contains? m k)
    m
    (assoc m k v)))

(defn list-or-cons? [x]
  (or (= (type x) clojure.lang.Cons)
      (list? x)))

(defn join-as-string [xs delim]
  (loop [xs xs, s ""]
    (if-let [x (first xs)]
      (recur (rest xs)
             (str s x (when (seq (rest xs))
                        delim)))
      s)))

(defn index-of [needle haystack]
  (loop [xs haystack, i 0]
    (when-let [x (first xs)]
      (if (= needle x)
        i
        (recur (rest xs) (inc i))))))
