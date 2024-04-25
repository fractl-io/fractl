(ns fractl.lang.tools.loader
  "Component script loading with pre-processing."
  (:require #?(:clj [clojure.java.io :as io])
            [clojure.string :as s]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.util.seq :as su]
            [fractl.util.logger :as log]
            [fractl.lang :as ln]
            [fractl.lang.raw :as raw]
            [fractl.lang.name-util :as nu]
            [fractl.lang.internal :as li]
            [fractl.lang.tools.util :as tu]
            [fractl.evaluator.state :as es])
  #?(:clj
     (:import [java.io FileInputStream InputStreamReader PushbackReader])))

(defn- record-name [obj]
  (let [n (cond
            (keyword? obj) obj

            (map? obj) (first (keys obj))

            :else (first obj))
        [a b] (li/split-path n)]
    (or b a)))

(defn- fetch-declared-names [spec-or-script]
  (loop [exps #?(:clj (if-not (string? spec-or-script)
                        spec-or-script
                        (read-string (str "(do" (slurp spec-or-script) "\n)")))
                 :cljs spec-or-script)
         result {}]
    (if-let [exp (first exps)]
      (recur
       (rest exps)
       (if (seqable? exp)
         (case (first exp)
           component
           (assoc result :component (second exp))

           (entity record event rule dataflow relationship view attribute inference)
           (assoc
            result :records
            (conj
             (or (:records result) #{})
             (record-name (second exp))))

           result)))
      result)))

(defn dependency-model-name [dep]
  (cond
    (keyword? dep) dep
    (vector? dep) (first dep)
    (map? dep) dep))

(defn dependency-model-version [dep]
  (when (vector? dep)
    (let [xs (s/split (second dep) #" ")]
      (second xs))))

(declare read-model)

(defn load-all-model-info [model-paths model-name model-info]
  (let [model-paths (or model-paths (tu/get-system-model-paths))
        [model model-root] (or model-info (read-model model-paths model-name))
        model-name (or model-name (:name model))]
    (when-not model-name
      (u/throw-ex "model-name is required"))
    (let [model-name (if (keyword? model-name)
                       (s/lower-case (name model-name))
                       model-name)]
      {:paths model-paths
       :model model
       :root model-root
       :name model-name})))

(defn load-default-model-info []
  (load-all-model-info nil nil nil))

#?(:clj
   (do
     (def ^:dynamic *parse-expressions* true)

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
                    identity)
              parser (if *parse-expressions* eval identity)]
          (use '[fractl.lang])
          (try
            (loop [exp (rdf), raw-exps [], exps []]
              (if (= exp :done)
                (do (raw/maybe-intern-component raw-exps) exps)
                (let [exp (fqn exp)]
                  (recur (rdf) (conj raw-exps exp) (conj exps (parser exp))))))
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
          (when (and component-name (cn/component-exists? component-name))
            (cn/remove-component component-name))
          (let [exprs (binding [*ns* *ns*]
                        (read-expressions
                         (if input-reader?
                           file-name-or-input-stream
                           file-ident)
                         names))]
            (if *parse-expressions*
              (when (and component-name (cn/component-exists? component-name))
                component-name)
              (vec exprs)))))
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

     (defn- verified-model-file-path
       ([model-script-name root-dir model-dir]
        (let [p (str root-dir u/path-sep
                     (when model-dir
                       (str model-dir u/path-sep))
                     model-script-name)]
          (and (.exists (java.io.File. p)) p)))
       ([model-script-name root-dir]
        (verified-model-file-path
         model-script-name root-dir nil)))

     (defn read-model
       ([model-paths model-name]
        (let [fpath (partial verified-model-file-path
                             (u/get-model-script-name))]
          (if-let [p (fpath ".")]
            (read-model p)
            (let [s (if (keyword? model-name)
                      (s/lower-case (name model-name))
                      model-name)]
              (loop [mps model-paths]
                (if-let [mp (first mps)]
                  (if-let [p (fpath mp s)]
                    (read-model p)
                    (recur (rest mps)))
                  (u/throw-ex
                   (str model-name " - model not found in any of "
                        model-paths))))))))
       ([model-file]
        (let [model (read-model-expressions model-file)
              root (java.io.File. (.getParent (java.io.File. model-file)))]
          (when (map? model)
            (ln/model model))
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

     (defn read-components-from-model [model model-root]
       (binding [*parse-expressions* false]
         (load-components-from-model model model-root)))

     (declare load-model)

     (defn load-model-dependencies [model model-paths from-resource]
       (when-let [deps (:dependencies model)]
         (let [rdm (partial read-model model-paths)]
           (doseq [d deps]
             (tu/maybe-clone-model d model-paths)
             (let [[m mr] (rdm (dependency-model-name d))]
               (load-model m mr model-paths from-resource))))))

     (defn load-model
       ([model model-root model-paths from-resource]
        (load-model-dependencies model model-paths from-resource)
        (load-components-from-model model model-root from-resource))
       ([model-name model-paths]
        (when-let [[model model-root] (read-model model-paths model-name )]
          (load-model model model-root model-paths false)))
       ([model-name]
        (load-model model-name (tu/get-system-model-paths)))))

   :cljs
   (do
     (defn get-corrupted-entity-form [entity-name t]
       `(~'entity
         ~entity-name
         {:meta {:corrupt true, :type ~t}}))

     (def call-intern {'component ln/component
                       'record ln/record
                       'entity ln/entity
                       'event ln/event
                       'view ln/view
                       'rule 'ln/rule
                       'relationship ln/relationship
                       'inference ln/inference
                       'dataflow ln/dataflow})

     (defn intern-component [component-spec]
       (let [component-spec (if (= 'do (first component-spec))
                              (rest component-spec)
                              component-spec)
             cspec (when (= 'component (ffirst component-spec)) (first component-spec))
             cname (if cspec
                     (second cspec)
                     (u/throw-ex (str "expected a component declaration, not " (first component-spec))))
             fqn (partial nu/fully-qualified-names (fetch-declared-names component-spec))]
         (doseq [exp component-spec]
           (when-let [intern (get call-intern (first exp))]
             (try
               (when-not (apply intern (rest (fqn exp)))
                 (u/throw-ex (str "failed to intern " exp)))
               (catch js/Object _
                 (let [corrupted-exp (get-corrupted-entity-form (second exp) (first exp))]
                   (apply intern (rest (fqn corrupted-exp))))))))))

     (defn load-components-from-model [model callback]
       (doseq [c (:components model)]
         (callback intern-component c)))

     (defn load-model-dependencies [model callback]
       (let [deps (mapv dependency-model-name (:dependencies model))]
         (callback deps)))

     (defn load-model [model callback]
       (cn/register-model (:name model) model)
       (let [continuation (fn [_] (load-components-from-model model (partial callback :comp)))]
         (load-model-dependencies model (partial callback :deps continuation))))))

