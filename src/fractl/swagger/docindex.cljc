(ns fractl.swagger.docindex
  (:require [clojure.string :as s]))

(def index-html
  "<!DOCTYPE html>
<html>
  <head>
    <title>Fractl Docs Index</title>
    <link href=\"https://fonts.googleapis.com/css2?family=Montserrat:wght@400;700&display=swap\" rel=\"stylesheet\">
    <link href=\"https://fonts.googleapis.com/css2?family=Roboto&display=swap\" rel=\"stylesheet\">
    <style>
      body {
        font-family: 'Roboto', sans-serif;
        background-color: #f2f2f2;
        margin: 0;
        padding: 0;
      }
      .header {
        background-color: rgb(38, 50, 56);
        color: white;
        padding: 20px;
        text-align: center;
      }
      .container {
        margin: 20px auto;
        max-width: 600px;
        padding: 20px;
        background-color: white;
        border-radius: 10px;
        box-shadow: 0 2px 5px rgba(0, 0, 0, 0.1);
      }
      .container h1 {
        font-size: 36px;
        margin-top: 0;
        font-family: 'Montserrat', sans-serif;
        font-weight: 300;
        color: rgb(51, 51, 51);
      }
      .links {
        display: flex;
        flex-direction: column;
        align-items: center;
      }
      .links a {
        display: block;
        padding: 10px 20px;
        margin: 10px 0;
        font-size: 24px;
        text-align: center;
        background-color: rgb(38, 50, 56);
        color: white;
        border-radius: 5px;
        text-decoration: none;
        transition: background-color 0.3s ease;
        font-family: 'Montserrat', sans-serif;
        font-weight: 400;
      }
      .links a:hover {
        background-color: #2E8B57;
      }
    </style>
  </head>
  <body>
    <div class=\"header\">
      <h1>Model: $model-name</h1>
    </div>
    <div class=\"container\">
      <div class=\"links\">
        $components
      </div>
    </div>
  </body>
</html>")

(defn gen-index-file [model-name components]
  (let [model-name (name model-name) 
        components (apply str
                          (mapv
                           (fn [cn]
                             (let [cn (clojure.string/replace (name cn) "." "")]
                               (str "<a href= \"" cn ".html \">" cn "</a>\n")))
                           components))
        html (-> index-html
                 (clojure.string/replace #"\$model-name" model-name)
                 (clojure.string/replace #"\$components" components))]
    #?(:clj
       (spit "doc/api/index.html" html))))