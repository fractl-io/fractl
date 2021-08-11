(ns fractl.lang.loader
  "Component script loading with pre-processing."
  (:require [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.lang.name-util :as nu]
            [fractl.component :as cn])
  (:import [java.io FileInputStream InputStreamReader PushbackReader]))

(defn- record-name [obj]
  (if (keyword? obj)
    obj
    (first (keys obj))))

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

           (entity record event)
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
  ([file-name declared-names]
   (let [reader (PushbackReader.
                 (InputStreamReader.
                  (FileInputStream. file-name)))
         rdf #(read reader nil :done)
         fqn (partial nu/fully-qualified-names declared-names)]
     (loop [exp (rdf), exps nil]
       (if (= exp :done)
         exps
         (recur (rdf) (conj exps (eval (fqn exp))))))))
  ([file-name]
   (read-expressions file-name (fetch-declared-names file-name))))

(defn load-script
  "Load, complile and intern the component from a script file."
  [^String component-root-path ^String file-name]
  (let [crp (or component-root-path "./")
        full-file-name
        (if (and component-root-path (not (.startsWith file-name component-root-path)))
          (str component-root-path u/path-sep file-name)
          file-name)
        names (fetch-declared-names full-file-name)
        component-name (:component names)]
    (when component-name
      (cn/remove-component component-name))
    (binding [*ns* *ns*]
      (read-expressions full-file-name names))
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
