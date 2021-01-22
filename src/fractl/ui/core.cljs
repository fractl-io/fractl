(ns fractl.ui.core)

(-> (.getElementById js/document "app")
    (.-innerHTML)
    (set! "Hello World!"))
