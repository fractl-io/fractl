(ns fractl.resolver.ui.instance
  (:require [fractl.resolver.core :as rc]
            [fractl.component :as cn]
            [fractl.util :as u]
            [fractl.lang.internal :as li]
            [fractl.ui.context :as ctx]
            [fractl.ui.util :as vu]
            [fractl.ui.views :as v]
            ["@material-ui/core"
             :refer [Card CardActions CardContent
                     Typography Button]]))

(defn- fetch-entity-instance [event-inst cb]
  (if-let [obj (:Instance event-inst)]
    (if (map? obj)
      obj
      (if-let [obj (ctx/lookup-active-instance)]
        obj
        (do (u/throw-ex (str "no active instances - " event-inst))
            nil)))
    (do (vu/query-instance event-inst cb) nil)))

(defn- field-view [inst schema nav-links attr-name]
  (let [attr-scm (cn/find-attribute-schema (attr-name schema))
        rf-paths (:ref attr-scm)
        v (str (attr-name inst))
        label (name attr-name)]
    (if rf-paths
      (do (swap!
           nav-links conj
           [:a {:href (vu/make-link
                       vu/make-instance-view-route
                       (:record rf-paths) (first (:refs rf-paths))
                       v)}
            label])
          nil)
      [label v])))

(defn- make-menu [links]
  (vec
   (concat
    [:span "[ "]
    (interpose " | " links)
    ["]"])))

(defn- make-card-view [title fields navigation-links]
  (let [fs (mapv (fn [[lbl val]]
                   [:> Typography {:style {:font-size 14}
                                   :color "text.secondary"}
                    (str lbl ": " val)])
                 fields)]
    `[:> ~Card {:style {:min-width 275}}
      [:> ~CardContent
       [:> ~Typography {:variant "h5" :component "div"} ~title]
       ~@fs]
      [:> ~CardActions ~(make-menu navigation-links)]]))

(defn- make-instance-view [inst]
  (let [n (cn/instance-name inst)
        schema (cn/fetch-schema n)
        meta (cn/fetch-meta n)
        nav (atom [])
        fields
        (filter
         identity
         (mapv
          (partial field-view inst schema nav)
          (or (:Fields meta) (cn/attribute-names schema))))
        contains
        (v/make-list-refs-view inst)
        edit-btn [:> Button
                  {:on-click #(v/render-view
                               (v/make-input-view inst))}
                  "Edit"]]
    `[:div
      ~(make-card-view (cn/instance-str inst) fields @nav)
      ~edit-btn
      ~@contains]))

(defn- render-instance [inst]
  (let [view (make-instance-view inst)]
    (v/render-view view)))

(defn- upsert-with-lookup [instance]
  (let [cb (fn [inst]
             (if inst
               (render-instance inst)
               (v/render-view [:div "error: failed to load instance"])))
        obj (fetch-entity-instance instance cb)
        view (when obj (make-instance-view obj))]
    (v/finalize-view (or view [:div "loading instance ..."]) instance)))

(defn- upsert-with-embedded-instance [einst instance]
  (v/finalize-view (make-instance-view einst)  instance))

(defn- upsert-ui [instance]
  (if-let [inst (:Instance instance)]
    (upsert-with-embedded-instance inst instance)
    (upsert-with-lookup instance)))

(defn make [n _]
  (rc/make-resolver
   n {:upsert {:handler upsert-ui}}))
