(ns fractl.ui.meta)

(defn view-event [meta tag]
  (get-in meta (concat [:views] tag)))

(defn authorize? [meta]
  (= :authorize
     (get-in
      meta
      [:views :create-button :on-success])))

(defn contains [meta]
  (seq (get-in meta [:views :contains])))

(defn attribute-view-spec [meta field-name]
  (get-in meta [:views :attributes field-name :input]))

(defn create-button-label [meta]
  (get-in meta [:views :create-button :label]))
