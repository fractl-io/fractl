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
