;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns pamela.web
  "PAMELA web functions."
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.pprint :as pp]
            [environ.core :refer [env]]
            [net.cgrand.enlive-html :refer :all]
            [ring.util.response :refer [response content-type]]
            [pamela.mode :refer :all]))

(def top "../../")

(def public "resources/public")

(def downloads (str public "/downloads"))

(def app-css-uri "css/pamela.css")

(def app-css (str top public app-css-uri))

(def phantomjs-shims (str top "/src/test/phantomjs/phantomjs-shims.js"))

(def app "js/compiled/app.js")

(def title "pamela")

(def lf "\n")

(def basic-page
  [{:tag :html
    :attrs nil
    :content
    [lf {:tag :head
         :attrs nil
         :content
         [lf {:tag :title
              :attrs nil
              :content [ title ]}
          lf]}
     lf {:tag :body
         :attrs nil
         :content
         [lf]}
     lf]}])

(defn html5dtd
  "Return an HTML5 DTD"
  {:added "0.2.0"}
  [nodes]
  (if (= (:type (first nodes)) :dtd)
    nodes
    (cons {:type :dtd :data ["html" nil nil]} nodes)))

(defn add-meta
  "Add meta"
  {:added "0.2.0"}
  [nodes meta]
  (at nodes
    [:head] (append
              (html [:meta meta]) lf)))

(defn charset-utf-8
  "Add charset utf-8"
  {:added "0.2.0"}
  [nodes]
  (add-meta nodes {:charset "utf-8"}))
  ;; (at nodes
  ;;   [:head] (append
  ;;             (html [:meta {:charset "utf-8"}]) lf)))

(defn viewport
  "Define viewport"
  {:added "0.2.0"}
  [nodes]
  (add-meta nodes {:name "viewport"
                   :content "width=device-width,initial-scale=1.0,maximum-scale=1.0,minimum-scale=1.0,user-scalable=0"}))

(defn html-lang
  "Define lang"
  {:added "0.2.0"}
  [nodes lang]
  (at nodes
    [:head] (set-attr :lang lang)))

(defn append-at
  "Append new-nodes at nodes"
  {:added "0.2.0"}
  [selector nodes new-nodes]
  (at nodes
    [selector] (append new-nodes lf)))

(defn append-head
  "Append to head"
  {:added "0.2.0"}
  [nodes new-head-nodes]
  (append-at :head nodes new-head-nodes))

(defn append-body
  "Append to body"
  {:added "0.2.0"}
  [nodes new-body-nodes]
  (append-at :body nodes new-body-nodes))

(defn add-favicon
  "Add favicon"
  {:added "0.2.0"}
  [nodes]
  (append-head nodes
    (html [:link {:rel "icon" :href "/favicon.ico" :type "image/x-icon"}])))

(defn add-css
  "Add css"
  {:added "0.2.0"}
  [nodes style-uri]
  (append-head nodes
    (html [:link {:rel "stylesheet" :href style-uri :type "text/css"}])))

(defn add-js
  "Add js"
  {:added "0.2.0"}
  [nodes script-uri]
  (append-head nodes
    (html [:script {:type "text/javascript" :src script-uri}])))

(defn add-h1
  "Add h1"
  {:added "0.2.0"}
  [nodes heading]
  (append-body nodes
    (html [:h1 heading])))

(defn add-div
  "Add div"
  {:added "0.2.0"}
  [nodes id]
  (append-body nodes
    (html [:div {:id id }])))

(defn add-textarea
  "Add textarea"
  {:added "0.2.0"}
  [nodes id]
  (append-body nodes
    (html [:textarea {:id id }])))

(defn render-snippet
  "Convert enlive nodes to html"
  {:added "0.2.0"}
  [s]
  (-> (apply str (emit* s))
    response
    (content-type "text/html")))

(def basic-html5 (-> basic-page
                   html5dtd
                   (html-lang "en")
                   charset-utf-8
                   viewport))

(defn create-dev-html
  "Create development mode HTML"
  {:added "0.2.0"}
  []
  (-> basic-html5
    ;; (add-favicon)
    (add-css app-css-uri)
    (add-js app)
    (add-div "app")
    render-snippet))

(defn create-html
  "Create production mode HTML"
  {:added "0.2.0"}
  []
  (-> basic-html5
    ;; (add-favicon)
    (add-css app-css-uri)
    (add-js app)
    (add-div "app")
    render-snippet))

(defn create-test-html
  "Create test mode HTML"
  {:added "0.2.0"}
  []
  (-> basic-html5
    ;; (add-favicon)
    (add-css app-css)
    (add-js phantomjs-shims)
    (add-js app)
    (add-div "app")
    (add-textarea "out")
    (render-snippet)
    :body))

(defn write-html
  "Write test-html to a file"
  {:added "0.2.0"}
  [html-file]
  (let [html-str (create-test-html)]
    (println "writing" app "into" html-file)
    (spit html-file html-str)))
