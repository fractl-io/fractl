(ns fractl.lang.loader
  "Component script loading with pre-processing."
  (:require [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.lang.name-util :as nu]
            [fractl.component :as cn])
  (:import [java.io FileInputStream InputStreamReader PushbackReader]))

(defn read-expressions
  "Read expressions in sequence from a fractl component file. Each expression read
   is preprocessed to add component-name prefixes to names. Then the expression is evaluated.
   Return a list with the results of evaluations."
  [file-name]
  (let [reader (PushbackReader.
                (InputStreamReader.
                 (FileInputStream. file-name)))
        rdf #(read reader nil :done)]
    (loop [exp (rdf), exps nil]
      (if (= exp :done)
        exps
        (recur (rdf) (conj exps (eval (nu/fully-qualified-names exp))))))))

(defn- maybe-fetch-component-name [file-name]
  (loop [exps (read-string (str "(do" (slurp file-name) ")"))]
    (when-let [exp (first exps)]
      (if (and (seqable? exp) (= 'component (first exp)))
        (second exp)
        (recur (rest exps))))))

(defn load-script
  "Load, complile and intern the component from a script file."
  [^String component-root-path ^String file-name]
  (let [crp (or component-root-path "./")
        full-file-name
        (if (and component-root-path (not (.startsWith file-name component-root-path)))
          (str component-root-path u/path-sep file-name)
          file-name)
        component-name (maybe-fetch-component-name full-file-name)]
    (when component-name
      (cn/remove-component component-name))
    (binding [*ns* *ns*]
      (read-expressions full-file-name))
    (when (and component-name (cn/component-exists? component-name))
      component-name)))

(defn load-expressions
  "Load, complile and intern the component from a namespace expressions."
  ([mns mns-exps convert-fq?]
   (use 'fractl.lang)
   (cn/remove-component mns)
   (binding [*ns* *ns*]
     (into
      '()
      (map
       #(eval
         (if convert-fq?
           (nu/fully-qualified-names %)
           %))
       mns-exps)))
   (when (cn/component-exists? mns)
     mns))
  ([mns mns-exps]
   (load-expressions mns mns-exps true)))
