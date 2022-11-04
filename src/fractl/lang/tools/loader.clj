(ns fractl.lang.tools.loader
  "Component script loading with pre-processing."
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.util.logger :as log]
            [fractl.lang.name-util :as nu]
            [fractl.lang.internal :as li]
            [fractl.component :as cn])
  (:import [java.io FileInputStream InputStreamReader PushbackReader]))

(defn- record-name [obj]
  (let [n (cond
            (keyword? obj)
            obj

            (map? obj)
            (first (keys obj))

            :else
            (first obj))
        [a b] (li/split-path n)]
    (or b a)))

(defn- fetch-declared-names [script-file]
  (loop [exps (read-string (str "(do" (slurp script-file) ")"))
         result {}]
    (if-let [exp (first exps)]
      (recur
       (rest exps)
       (if (seqable? exp)
         (case (first exp)
           component
           (assoc result :component (second exp))

           (entity record event dataflow)
           (assoc
            result :records
            (conj
             (:records result)
             (record-name (second exp))))

           result)))
      result)))

(defn read-expressions
  "Read expressions in sequence from a fractl component file. Each expression read
   is preprocessed to add component-name prefixes to names. Then the expression is evaluated.
   Return a list with the results of evaluations."
  ([file-name-or-input-stream declared-names]
   (let [reader (PushbackReader.
                 (InputStreamReader.
                  (if (string? file-name-or-input-stream)
                    (FileInputStream. file-name-or-input-stream)
                    (io/input-stream file-name-or-input-stream))))
         rdf #(read reader nil :done)
         fqn (if declared-names
               (partial nu/fully-qualified-names declared-names)
               identity)]
     (try
       (loop [exp (rdf), exps nil]
         (if (= exp :done)
           (reverse exps)
           (recur (rdf) (conj exps (eval (fqn exp))))))
       (finally
         (u/safe-close reader)))))
  ([file-name-or-input-stream]
   (read-expressions
    file-name-or-input-stream
    (fetch-declared-names file-name-or-input-stream))))

(defn load-script
  "Load, complile and intern the component from a script file."
  ([^String component-root-path file-name-or-input-stream]
   (log/info (str "Component root path: " component-root-path))
   (log/info (str "File name: " file-name-or-input-stream))
   (let [input-reader? (not (string? file-name-or-input-stream))
        file-ident
         (if input-reader?
           (InputStreamReader. (io/input-stream file-name-or-input-stream))
           (if (and
                component-root-path
                (not (.startsWith
                      file-name-or-input-stream
                      component-root-path)))
             (str component-root-path u/path-sep file-name-or-input-stream)
             file-name-or-input-stream))
         names (fetch-declared-names file-ident)
         component-name (:component names)]
     (when component-name
       (cn/remove-component component-name))
     (binding [*ns* *ns*]
       (read-expressions (if input-reader? file-name-or-input-stream file-ident) names))
     (when (and component-name (cn/component-exists? component-name))
       component-name)))
  ([file-name-or-input-stream]
   (load-script nil file-name-or-input-stream)))

(defn load-expressions
  "Load, complile and intern the component from a namespace expressions."
  ([mns mns-exps convert-fq?]
   (use 'fractl.lang)
   (cn/remove-component mns)
   (binding [*ns* *ns*]
     (into
      '()
      (mapv
       #(eval
         (if convert-fq?
           (nu/fully-qualified-names %)
           %))
       mns-exps)))
   (when (cn/component-exists? mns)
     mns))
  ([mns mns-exps]
   (load-expressions mns mns-exps true)))

(defn read-model-expressions [model-file]
  (try
    (binding [*ns* *ns*]
      (last (read-expressions model-file nil)))
    (catch Exception ex
      (.printStackTrace ex))))

(defn read-model
  ([model-paths model-name]
   (let [s (s/lower-case (name model-name))]
     (loop [mps model-paths]
       (if-let [mp (first mps)]
         (let [p (str mp u/path-sep s u/path-sep (u/get-model-script-name))]
           (if (.exists (java.io.File. p))
             (read-model p)
             (recur (rest mps))))
         (u/throw-ex
          (str model-name " - model not found in any of "
               model-paths))))))
  ([model-file]
   (let [model (read-model-expressions model-file)
         root (java.io.File. (.getParent (java.io.File. model-file)))]
     [model (str root)])))

(defn load-components
  ([component-scripts model-root load-from-resource]
   (when (seq (su/nonils component-scripts))
     (mapv
      #(load-script
        model-root
        (if load-from-resource
          (io/resource (str "model/" model-root "/" %))
          %))
      component-scripts)))
  ([component-scripts model-root]
   (load-components component-scripts model-root false)))

(defn- script-name-from-component-name [component-name]
  (loop [s (subs (str component-name) 1), sep "", result []]
    (if-let [c (first s)]
      (cond
        (Character/isUpperCase c) (recur (rest s) "_" (conj result sep (Character/toLowerCase c)))
        (or (= \/ c) (= \. c)) (recur (rest s) "" (conj result java.io.File/separator))
        :else (recur (rest s) sep (conj result c)))
      (str (s/join result) (u/get-script-extn)))))

(defn load-components-from-model
  ([model model-root load-from-resource]
   (load-components
    (mapv script-name-from-component-name (:components model))
    model-root load-from-resource))
  ([model model-root]
   (load-components-from-model model model-root false)))

(defn dependency-model-name [dep]
  (cond
    (keyword? dep) dep
    (vector? dep) (first dep)))

(defn dependency-model-version [dep]
  (when (vector? dep)
    (second dep)))
