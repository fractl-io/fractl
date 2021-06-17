(ns fractl.lang.loader
  "Component script loading with pre-processing."
  (:require [fractl.util.seq :as su]
            [fractl.lang.name-util :as nu]
            [fractl.lang.internal :as li]
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

(defn load-script
  "Load, complile and intern the component from a script file."
  [component-root-path file-name]
  (let [crp (or component-root-path "./")
        component-name (li/component-from-filename crp file-name)]
    (cn/remove-component component-name)
    (binding [*ns* *ns*] (read-expressions file-name))
    (when (cn/component-exists? component-name)
      component-name)))

(defn load-expressions
  "Load, complile and intern the component from a namespace expressions."
  ([mns mns-exps convert-fq?]
   (use 'fractl.lang)
   (cn/remove-component mns)
   (binding [*ns* *ns*] (into '() (map #(eval (if convert-fq?
                                                (nu/fully-qualified-names %)
                                                %))
                                       mns-exps)))
   (when (cn/component-exists? mns)
     mns))
  ([mns mns-exps]
   (load-expressions mns mns-exps true)))
