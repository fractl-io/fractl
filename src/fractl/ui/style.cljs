(ns fractl.ui.style)

(def ^:private styles-db
  (atom
   {:instance
    {:card {:style {:min-width 275}}
     :title {:variant "h5" :component "div"}
     :entry {:style {:font-size 14}
             :color "text.secondary"}}
    :input-form
    {:card {:variant "outlined"}
     :title {:gutterBottom true :variant "h5" :component "div"}}}))

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

(def table (partial fetch-style [:table]))
