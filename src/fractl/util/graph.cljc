(ns fractl.util.graph
  "Graph data structure and algorithms like topological sort."
  #?(:clj
     (:import [java.util Queue LinkedList])))

(def EMPTY {})

(defn- adj
  "Return the adjacency list associated with the key."
  [dg v]
  (get dg v (list)))

(defn add-edge [dg v w]
  (let [ws (adj dg v)]
    (assoc dg v (conj ws w))))

(defn add-edges [dg v ws]
  (loop [ws ws, dg dg]
    (if-let [w (first ws)]
      (recur (rest ws) (add-edge dg v w))
      dg)))

(defn seq->dg
  "Create a new directed graph from a flat sequence.
  The sequence should have an even number of elements.
  The sequence is considered in pairs, with the first element
  becoming a node and an edge is added from it to the second element."
  [xs]
  (loop [xs xs, dg EMPTY]
    (let [v (first xs) w (second xs)]
      (if (and v w)
        (recur (nthrest xs 2) (add-edge dg v w))
        dg))))

(defn dg-reverse [dg]
  (loop [new-dg EMPTY
         vs (keys dg)]
    (if-let [v (first vs)]
      (recur (loop [dg2 new-dg
                    ws (adj dg v)]
               (if-let [w (first ws)]
                 (recur (add-edge dg2 w v)
                        (rest ws))
                 dg2))
             (rest vs))
      new-dg)))

(defn reachable?
  "Return true if the node is reachable by a depth-first-search,
  return false otherwise. Query is done on a search-structure initialized
  by a graph-dfs function."
  [dfs v]
  (get dfs v false))

(defn- mkdfs
  ([dg v marked pre-f post-f]
   (if (reachable? marked v)
     marked
     (do (when pre-f (pre-f v))
         (loop [ws (adj dg v)
                mks (assoc marked v true)]
           (if-let [w (first ws)]
             (if-not (reachable? mks w)
               (recur (rest ws) (mkdfs dg w mks pre-f post-f))
               (recur (rest ws) mks))
             (do (when post-f (post-f v)) mks))))))
  ([dg v marked] (mkdfs dg v marked nil nil)))

(defn cycdfs [marked stack path cycle?]
  {:dfs marked :stk stack :path path :cycle? cycle?})

(defn- mkdfs-with-cycles
  ([dg v marked stack path]
   (cond
     (get stack v false) (cycdfs marked stack (conj path v) true)
     (reachable? marked v) (cycdfs marked (dissoc stack v) (pop path) false)
     :else (loop [ws (adj dg v)
                  mks (assoc marked v true)
                  stk (assoc stack v true)
                  p (conj path v)]
             (if-let [w (first ws)]
               (cond
                 (get stk w false)
                 (cycdfs mks stk (conj p w) true)

                 (not (reachable? mks w))
                 (let [{m :dfs s :stk c? :cycle? p2 :path :as r}
                       (mkdfs-with-cycles dg w mks stk p)]
                   (if c? r (recur (rest ws) m s p2)))

                 :else (recur (rest ws) mks stk p))
               (cycdfs mks (dissoc stk v) (pop p) false)))))
  ([dg v] (mkdfs-with-cycles dg v {} {} (list))))

(defn dg-dfs
  "Initialize a depth-first-search query structure for a directed-graph.
  The search starts from the source, which should be a single node or
  a sequence of nodes."
  [dg src]
  (if (seqable? src)
    (loop [vs src, marked {}]
      (if-let [v (first vs)]
        (recur (rest vs) (mkdfs dg v marked))
        marked))
    (mkdfs dg src {})))

(defn detect-cycle [dg src]
  (let [r (mkdfs-with-cycles dg src)]
    (if (:cycle? r)
      {:dfs (:dfs r)
       :path (reverse (:path r))
       :cycle true}
      {:dfs (:dfs r)})))

(defn has-cycle? [dg src]
  (or (:cycle (detect-cycle dg src)) false))

(defn has-non-self-cycle? [dg src]
  (let [cycinfo (detect-cycle dg src)]
    (if (:cycle cycinfo)
      (not (every? #(= (first (:path cycinfo)) %) (:path cycinfo)))
      false)))

(defn edges [dg v] (vec (adj dg v)))
(defn nodes [dg] (set (keys dg)))
(defn all-edges [dg] (vals dg))

(defn- traverse [dg v pre?]
  (let [dfs (partial mkdfs dg v {})]
    #?(:clj
       (let [^Queue q (LinkedList.)
             f (fn [v] (.add q v))
             _ (if pre?
                 (dfs f nil)
                 (dfs nil f))]
         (seq q))
       :cljs
       (let [q (atom  #queue [])
             f (fn [v] (reset! q (conj @q v)))
             _ (if pre?
                 (dfs f nil)
                 (dfs nil f))]
         (seq @q)))))

(defn preorder [dg v] (traverse dg v true))
(defn postorder [dg v] (traverse dg v false))

(defn topological [dg v]
  (when-not (has-non-self-cycle? dg v)
    (postorder dg v)))

(defn topological-all [dg]
  (apply merge (map (fn [v] {v (topological dg v)}) (nodes dg))))
