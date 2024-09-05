(ns agentlang.lang.tools.replcmds
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.pprint :as pp]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang.raw :as raw]
            [agentlang.global-state :as gs])
  (:import [java.io File]
           [org.apache.commons.io FileUtils]))  

(defn- maybe-dump-model! [component-name outdir]
  (let [model-file (File.
                    (s/lower-case (str outdir u/path-sep "model.agentlang")))
        model (or (when (.exists model-file)
                    (try
                      (read-string (slurp model-file))
                      (catch Exception ex
                        (println (str "WARN - failed to read model.agentlang - " (.getMessage ex))))))
                  {:name (keyword (s/lower-case outdir))
                   :version "0.0.1"
                   :agentlang-version (gs/agentlang-version)})
        cns (vec (set (conj (:components model) component-name)))]
    (with-open [w (io/writer model-file)]
      (pp/pprint (assoc model :components cns) w))))

(defn dump [component-name]
  (when-let [contents (raw/as-edn component-name)]
    (let [s (subs (str component-name) 1)
          outdir (first (s/split s #"\."))
          full-path (s/lower-case
                     (str outdir u/path-sep (s/replace s "." u/path-sep)
                          (u/get-script-extn)))
          f (File. full-path)]
      (FileUtils/createParentDirectories f)
      (maybe-dump-model! component-name outdir)
      (with-open [w (io/writer f)]
        (doseq [exp (rest contents)]
          (pp/pprint exp w)))
      full-path)))

(def ^:private active-component (atom nil))

(defn switch [component-name]
  (cn/switch-component component-name)
  (reset! active-component component-name))

(defn get-active-component [] @active-component)
