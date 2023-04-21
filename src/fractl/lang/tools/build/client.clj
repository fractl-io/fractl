(ns fractl.lang.tools.build.client
  (:require [selmer.parser :as sp]
            [fractl.util :as u]
            [fractl.global-state :as gs]
            [fractl.datafmt.json :as json]))

(def package_json
  (json/encode
   {"name" "client-integ-demo",
    "version" "0.0.1",
    "private" true,
    "devDependencies" {"shadow-cljs" "2.22.2"},
    "dependencies"
    {"@emotion/react" "^11.10.4",
     "@emotion/styled" "^11.10.4",
     "@js-joda/core" "3.2.0",
     "@js-joda/locale_en-us" "3.1.1",
     "@js-joda/timezone" "2.5.0",
     "@mui/icons-material" "^5.11.11",
     "@mui/material" "^5.10.6",
     "react" "^18.2.0",
     "react-beautiful-dnd" "^13.1.1",
     "react-dom" "^18.2.0",
     "react-icons" "^4.8.0"}}))

(def shadow-cljs_edn
  '{:source-paths
    ["src/main"]
    :dependencies
    [[reagent "1.2.0"]
     [cljs-ajax "0.7.5"]
     [fractl-io/fx "0.1.2"]]
    :builds
    {:app {:target :browser
           :modules {:main {:entries [core]}}}}
    :dev-http {8000 "public"}})

(defn update-shadow-cljs [model-dep]
  (let [deps (:dependencies shadow-cljs_edn)]
    (assoc shadow-cljs_edn :dependencies (conj deps model-dep))))

(def public_index_html
  "<!DOCTYPE html>
  <html lang=\"\">
  <head>
    <title>{{title}}</title>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">
  </head>
  <body>
    <div id=\"app\" style=\"width: 800px;height: 800px;position: absolute;top:0;bottom: 0;left: 0;right: 0;margin: auto;\"></div>
    <script src=\"js/main.js\" type=\"text/javascript\"></script>
  </body>
  </html>")

(def src_main_core_cljs
  "(do
    (ns core
    (:require [reagent.dom :as rdom]
              [fractl.model.fractl.kernel.identity :as identity]
              [fractl.resolver.registry :as rr]
              [fractl.lang :refer [dataflow]]
              {{model-require}}
              [fractl.fx.view.util :as vu]
              [fractl.fx.view.state :as vs]
              [fractl.fx.core :as fx]))
  
  (vs/prod-build!)

  (def app-name \"{{app-name}}\")
  (def app-root {{app-root}})
  (def api-host \"{{api-host}}\")

  (defn app []
  (let [view (fx/dashboard
              app-root
              nil
              {:app
               {:name app-name
                :api-host (str api-host \"/_e/\")}})]
    [:div {:id vu/entity-dashboard-id} (or view \"failed to generate dashboard\")]))

  (defn main []
  (fx/on-properties-change! #(println \" >>>>\" %))
  (let [app-node (.getElementById js/document \"app\")]
    (rdom/render [app] app-node)))

(main))")

(defn build-project [model-name model-version model-ns writer]
  (let [mn (name model-name)
        build-config (:client (:build (gs/get-app-config)))
        app-root (:root-entity build-config)
        api-host (:api-host build-config)]
    (when-not app-root
      (u/throw-ex "required configuration not found - build -> client -> root-entity"))
    (when-not app-root
      (u/throw-ex "required configuration not found - build -> client -> api-host"))
    (let [params {:app-name mn
                  :app-root (:root-entity build-config) ; TODO: load from config or model-info
                  :api-host (:api-host build-config); TODO: load from config or model-info
                  :title (str "Welcome to " mn)
                  :model-require [model-ns :as 'model]}
          shadow-cljs (update-shadow-cljs [(symbol mn) model-version])
          index-html (sp/render public_index_html params)
          core (rest (read-string (sp/render src_main_core_cljs params)))]
      (writer "package.json" package_json :spit)
      (writer "shadow-cljs.edn" shadow-cljs)
      (writer (str "public" u/path-sep "index.html") index-html :spit)
      (writer (str "src" u/path-sep "main" u/path-sep "core.cljs") core :write-each)
      model-name)))
