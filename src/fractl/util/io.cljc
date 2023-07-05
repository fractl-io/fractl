(ns fractl.util.io
  (:require [clojure.string :as s]
            [clojure.pprint :as pp]
            [fractl.util.common :as uc]
            #?(:clj [clojure.java.io :as io]))
    #?(:clj
     (:import [java.io File]
              [org.apache.commons.io FilenameUtils]
              [org.apache.commons.exec CommandLine Executor DefaultExecutor])))

(def path-sep
  #?(:clj java.io.File/separator
     :cljs "/"))

(def ^:private script-extn (atom ".fractl"))

(defn set-script-extn! [extn]
  (reset! script-extn extn))

(defn get-script-extn []
  @script-extn)

(def ^:private model-script-name (atom ""))

(defn get-model-script-name []
  (str "model" @script-extn))

(defn file-extension [s]
  #?(:clj
     (str "." (FilenameUtils/getExtension s))
     :cljs
     (second (re-find #"(\.[a-zA-Z0-9]+)$" s))))

(defn remove-extension [^String filename]
  (let [i (.lastIndexOf filename ".")]
    (if (>= i 0)
      (.substring filename 0 i)
      filename)))

(defn fractl-script? [f]
  (= @script-extn (file-extension (str f))))

(defn safe-close [obj]
  #?(:clj
     (try
       (.close obj)
       true
       (catch Exception ex
         false))
     :cljs true))

(defn pretty-spit
  [file-name edn]
  #?(:clj
     (spit file-name
           (with-out-str
             (pp/write edn :dispatch pp/code-dispatch)))
     :cljs edn))

(defn trace
  "Prints `msg` and `x`. Returns `x`."
  [msg x]
  (println msg (pr-str x))
  x)

(defn trace-with-fn
  "Prints `msg`, `x` and the result of `(f x)`. Returns `x`."
  [msg f x]
  (println msg (pr-str x) (pr-str (f x)))
  x)

#?(:clj
   (do
     (defn exec-in-directory [path cmd]
       (let [^CommandLine cmd-line (CommandLine/parse cmd)
             ^Executor executor (DefaultExecutor.)]
         (.setWorkingDirectory executor (if (string? path) (File. path) path))
         (zero? (.execute executor cmd-line))))

     (defn read-env-var [x]
       (cond
         (symbol? x)
         (when-let [v (System/getenv (name x))]
           (let [s (try
                     (read-string v)
                     (catch Exception _e v))]
             (cond
               (not= (str s) v) v
               (symbol? s) (str s)
               :else s)))

         (vector? x)
         (first (filter identity (mapv read-env-var x)))

         :else x))

     (defn read-config-file [config-file]
       (let [f (io/file config-file)]
         (when-not (.exists f)
           (with-open [out (io/writer f)]
             (binding [*out* out]
               (print {:service {:port 8080}})))))
       (binding [*data-readers* {'$ read-env-var}]
         (read-string (slurp config-file))))))

(defn get-parent [file-name]
  #?(:clj (when-let [p (.getParent (java.io.File. file-name))]
            (java.io.File. p))
     :cljs (when-let [i (s/last-index-of file-name path-sep)]
             (subs file-name 0 i))))

(defn resource [path]
  #?(:clj (io/resource path)
     :cljs (uc/throw-ex "load-from-resource: not supported in cljs")))
