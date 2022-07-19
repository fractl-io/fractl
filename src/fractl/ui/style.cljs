(ns fractl.ui.style)

(def ^:private styles-db
  (atom
   {:instance
    {:card {:style {:min-width 275}}
     :title {:variant "h5" :component "div"}
     :entry {:style {:font-size 20}
             :color "text.secondary"}}
    :input-form
    {:card {:variant "" :padding "20px" }
     :title {:gutterBottom true :variant "h5" :component "div" :style {:text-align "center"}}}}))

(defn set-user-style! [path style]
  (swap! styles-db assoc-in path style))

(defn fetch-style [path user-styles]
  (or (get-in user-styles path)
      (get-in @styles-db path)))

(def instance-card (partial fetch-style [:instance :card]))
(def instance-title (partial fetch-style [:instance :title]))
(def instance-entry (partial fetch-style [:instance :entry]))

(def input-form-card (partial fetch-style [:input-form :card]))
(def input-form-title (partial fetch-style [:input-form :title]))

(def table (partial fetch-style [:table :main]))
(def table-head (partial fetch-style [:table :head]))
(def table-head-row (partial fetch-style [:table :head-row]))
(def table-head-cell (partial fetch-style [:table :head-cell]))
(def table-body (partial fetch-style [:table :body]))
(def table-body-row (partial fetch-style [:table :body-row]))
(def table-body-cell (partial fetch-style [:table :body-cell]))
