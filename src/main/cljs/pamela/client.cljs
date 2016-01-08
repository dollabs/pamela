;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns ^:figwheel-always pamela.client
    (:require-macros [cljs.core.async.macros :refer [go go-loop]])
    (:require [clojure.set :refer [difference]]
              [cljs.pprint :refer [pprint]]
              [cljs.core.async :as async
               :refer [<! >! chan pub put! sub take! timeout sliding-buffer dropping-buffer]]
              [om.core :as om :include-macros true]
              [om.dom :as dom :include-macros true]
              [goog.string :as gstring]
              [goog.dom :as gdom]
              [sablono.core :as html :refer-macros [html]]
              [cljs-http.client :as http]
              [pamela.utils :refer [remove-fn assoc-if vec-index-of program-mode make-url]]
              [pamela.server :as server]))

(enable-console-print!)

;; Om application ===========================

(defonce app-state (atom nil))
(defonce e-types (atom nil))

(defn pad [i n]
  (let [istr (if (string? i) i (str i))
        len (count istr)]
    (if (>= len n)
      istr
      (recur (str "0" istr) n))))

;; debugging helper to inspect app-state on the REPL
(defn ppstate [& [ks]]
  (let [state (if ks
                (get-in @app-state ks)
                @app-state)]
    (pprint (remove-fn state))
    nil))

;; core.async events -----------------------------------------

;; return the events channel (for publishing events)
;; each value published should be a hash map which contains
;; the :id of the component
(defn get-events []
  (get-in @app-state [:channels :events]))

;; this is the pub channel for id events
;; which can be used to subscribe to a specific id
(defn get-id-events []
  (get-in @app-state [:channels :id-events]))

;; put the hashmap m on the events channel with component id
(defn put-event! [cursor]
  (put! (get-events) cursor))

;; global functions -----------------------------------------------

;; set font size
(defn set-size! [size & [e]]
  (put-event! {:id :size :value size})
  (swap! app-state
    #(assoc % :style (str (if (:subdued %) "subdued." "") (name size))))
  (when e
    (.preventDefault e)
    (.stopPropagation e)))

;; components -----------------------------------------------------

;; cursor here is assumed to have a key
;; :checkbox for checkbox components and
;; :value for other components
;; Optional callback fn will be passed the cursor
(defn input-change!
  "update input"
  [callback cursor e]
  (let [target (.. e -currentTarget)
        input-type (.-type target)]
    (.preventDefault e)
    (.stopPropagation e)
    (if (= input-type "checkbox")
      (let [checked (.-checked target)
            cursor (assoc cursor :checked checked)]
        ;; instead of putting the event here, do it in the callback
        ;; (put-event! cursor)
        (if callback (callback cursor)))
      (let [v (gstring/trim (.-value target))
            value (if (= input-type "radio") (keyword v) v)
            cursor (assoc cursor :value value)]
        ;; assume callback will do (om/update! cursor [:value] value)
        (if callback (callback cursor))))))

;; on-change is an optional function callback which takes the cursor
;; DEFAULT's to publishing the event on the id channel
(defn checkbox-data [& {:keys [id style label disabled checked on-change] :as opts}]
  (-> {:e-type :checkbox}
    (assoc-if :id id)
    (assoc-if :style style)
    (assoc-if :label label)
    (assoc-if :disabled disabled)
    (assoc-if :checked checked)
    (assoc :on-change
      (if on-change on-change
          (fn [cursor] (put-event! cursor))))))

(defn checkbox [cursor owner {:keys [opts-id] :as opts}]
  (reify
    om/IInitState
    (init-state [_]
      {:is-checked (:checked cursor)})
    om/IWillMount
    (will-mount [_]
      (let [{:keys [id]} cursor
            id (keyword (or id opts-id))
            checkbox-events (chan)]
        (sub (get-id-events) id checkbox-events)
        (go-loop []
          (let [{:keys [id checked]} (<! checkbox-events)]
            (om/set-state! owner [:is-checked] checked)
            (recur)))))
    om/IRenderState
    (render-state [_ {:keys [is-checked]}]
      (let [{:keys [id style label disabled checked on-change]} cursor
            id (or id (do (om/update! cursor [:id] opts-id) opts-id))
            style (or style "checkbox")
            tag (keyword (str "input#" (name id) "." style))
            checked-now (if (= checked is-checked)
                          checked
                          (do
                            (om/update! cursor [:checked] is-checked)
                            is-checked))
            basic-attrs {:type :checkbox
                         :checked checked-now}
            attrs (if disabled basic-attrs
                      (assoc basic-attrs
                        :on-change (partial input-change! on-change cursor)))]
        (html [tag attrs label])))))

(defn button-click!
  "button click"
  [callback cursor e]
  (if callback (callback cursor))
  (.preventDefault e)
  (.stopPropagation e))

(defn button-data [& {:keys [id style type label disabled on-click] :as opts}]
  (-> {:e-type :button}
    (assoc-if :id id)
    (assoc-if :style style)
    (assoc-if :type type)
    (assoc-if :label label)
    (assoc-if :disabled disabled)
    (assoc-if :on-click on-click)))

(defn button [cursor owner {:keys [opts-id] :as opts}]
  (reify
    om/IRender
    (render [_]
      (let [{:keys [label disabled on-click id style type]} cursor
            id (or id (do (om/update! cursor [:id] opts-id) opts-id))
            style (or style "button")
            type (if type (name type))
            tag (keyword (str "button#" (name id) "." style))
            on-click (if-not (= type "submit")
                       (partial button-click! on-click cursor))
            attrs (assoc-if
                    (assoc-if
                      {:disabled disabled}
                      :on-click on-click)
                    :type type)]
        (html [tag attrs label])))))

(defn clickable-data [& {:keys [id style label data disabled on-click on-page] :as opts}]
  (-> {:e-type :clickable}
    (assoc-if :id id)
    (assoc-if :style style)
    (assoc-if :label label)
    (assoc-if :data data)
    (assoc-if :disabled disabled)
    (assoc-if :on-click on-click)
    (assoc-if :on-page on-page)))

(defn clickable [cursor owner {:keys [opts-id] :as opts}]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [root-events (chan)
            on-page (:on-page cursor)]
        (sub (get-id-events) :root root-events)
        (go-loop []
          (let [{:keys [id page prev-page]} (<! root-events)]
            (when on-page
              (on-page cursor page))
            (recur)))))
    om/IRender
    (render [_]
      (let [{:keys [label data disabled on-click id style]} cursor
            id (or id (do (om/update! cursor [:id] opts-id) opts-id))
            style (if style (str "clickable." style) "clickable")
            tag (keyword (str "div#" (name id) "." style))
            attrs (if disabled
                    {}
                    {:on-click (partial button-click! on-click cursor)})
            label (if label
                    (if (string? label)
                      label
                      (vec (cons :span (om/value label)))))]
        (html [tag attrs label])))))

(defn radio-data [& {:keys [id style label disabled value options on-change] :as opts}]
  (-> {:e-type :radio}
    (assoc-if :id id)
    (assoc-if :style style)
    (assoc-if :label label)
    (assoc-if :disabled disabled)
    (assoc-if :value value)
    (assoc-if :options options)
    (assoc :on-change
      (if on-change on-change
          (fn [cursor] (put-event! cursor))))))

;; Om radio component
(defn radio [cursor owner {:keys [opts-id] :as opts}]
  (reify
    om/IInitState
    (init-state [_]
      {:is-value (:value cursor)})
    om/IWillMount
    (will-mount [_]
      (let [{:keys [id]} cursor
            ;; NOTE: may be an integer id! ;; id (keyword (or id opts-id))
            id (or id opts-id)
            radio-events (chan)]
        (sub (get-id-events) id radio-events)
        (go-loop []
          (let [{:keys [id value]} (<! radio-events)]
            (om/set-state! owner [:is-value] value)
            (recur)))))
    om/IRenderState
    (render-state [_ {:keys [is-value]}]
      (let [{:keys [id style label disabled value options on-change]} cursor
            id (or id (do (om/update! cursor [:id] opts-id) opts-id))
            style (or style "radio")
            tag (keyword (str "div#radiodiv-" id "." style))
            value-now (if (= value is-value)
                        value
                        (do
                          (om/update! cursor [:value] is-value)
                          is-value))]
        (html
          [tag [:span (or label "")]
               (apply vector :ul
                 (for [i (range 0 (count options) 2)]
                   (let [v (nth options i)
                         radio-tag (keyword (str "input#radio-" id "_" (name v)))
                         checked (= value-now v)
                         basic-attrs {:type :radio
                                      :name (pad (str id) 3)
                                      :value (name v)
                                      :checked checked
                                      :disabled disabled}
                         attrs (if disabled basic-attrs
                                   (assoc basic-attrs
                                     :on-change (partial input-change! on-change cursor)))
                         choice (nth options (inc i))
                         c (if (string? choice) choice
                               (om/value choice))]
                     [:li [radio-tag attrs] c])))])))))

(defn set-type! [type & [e]]
  (let [[value i] (gstring/splitLimit (name type) ":" 2)]
    ;; (println "set-type!" type "i" i "value" value)
    (put-event! {:id (int i) :value type})
    ;; Since we are publishing the event we don't need to mutate here..
    ;; (swap! app-state assoc-in [:elements :pamelad :args (int i) :value] type)
    (when e
      (.preventDefault e)
      (.stopPropagation e))))

(defn pamelad-data [& {:keys [id style clear add run args] :as opts}]
  (-> {:e-type :pamelad}
    (assoc-if :id id)
    (assoc-if :style style)
    (assoc-if :clear clear)
    (assoc-if :add add)
    (assoc-if :run run)
    (assoc-if :args args)))

(defn pamelad-args [n args]
  (if (empty? args)
    [] ;; return empty vector
    (vec
      (for [i (range 1 (inc (count args)))
            :let [argid i
                  arg (get args argid)
                  value (:value arg)
                  [v i] (gstring/splitLimit (name value) ":" 2)
                  tr-tag (keyword
                           (str "tr#tr-" i
                             (if (> i n) ".hidden")))
                  iii (pad i 3)
                  iiif (str iii ".f")
                  iiis (str iii ".s")
                  iiit (str iii ".t")]]
        [tr-tag
         [:td i]
         [:td (om/build radio arg)]
         [:td
          [(if (= v "file") :div.left :div.hidden)
           [:input {:type "file" :name iiif :size "50"}]]
          [(if (= v "select") :div.left :div.hidden)
           [:select {:name iiis}
            [:option "--help"]
            [:option "--version"]
            [:option "--verbose"]
            [:option "--construct-tpn"]
            [:option "--database"]
            [:option "--format"]
            [:option "--input"]
            [:option "--load"]
            [:option "--model"]
            [:option "--recursive"]
            [:option "--simple"]
            [:option "--visualize"]
            [:option "cat"]
            [:option "delete"]
            [:option "describe"]
            [:option "export"]
            [:option "import"]
            [:option "list"]
            [:option "load"]
            [:option "tpn"]
            ]]
          [(if (= v "text") :div.left :div.hidden)
           [:input
            (assoc-if {:type "text" :name iiit
                       :size "40"}
              :value (if (> i n) ""))]]]]))))

(defn pamelad [cursor owner {:keys [opts-id] :as opts}]
  (reify
    om/IRender
    (render [_]
      (let [{:keys [id style clear add run args]} cursor
            id (or id (do (om/update! cursor [:id] opts-id) opts-id))
            style (or style "pamelad")
            tag (keyword (str "div#" (name id) "." style))
            action (make-url :uri "/pamela")
            enctype "multipart/form-data"
            method "POST"
            form-attrs {:action action :encType enctype :method method}
            n (:data add)
            file (keyword (str "file:" n))
            select (keyword (str "select:" n))
            text (keyword (str "text:" n))
            newargid n
            newarg (radio-data
                     :id newargid
                     :value select
                     :options [file [:span {:on-click (partial set-type! file)} "File"]
                               select [:span {:on-click (partial set-type! select)} "Option"]
                               text [:span {:on-click (partial set-type! text)} "String"]]
                     :on-change (fn [cursor]
                                  (set-type! (:value cursor))))
            args (if (>= (count args) n)
                   args
                   (let [newargs (if args (assoc args newargid newarg)
                                     {newargid newarg})]
                     (om/update! cursor [:args] newargs)
                     args ;; avoid blinking/flashing -- use cursor structure
                     ;; NOT newargs here
                     ))]
        (html [tag
               [:h2 "pamelad"]
               [:span "Action: one of "
                [:b "delete, describe, export, import, list"]]
               [:br]
               [:form form-attrs
                [:table#args-table
                 [:tbody#args-body
                     (cons
                       [:tr#args-clear-row
                        [:td.noborder.left {:colSpan 2} "Arguments:"]
                        [:td.noborder.right {:colSpan 3}
                         (om/build button clear
                           {:react-key :clear
                            :opts {:opts-id :clear}})]]
                       (cons
                         [:tr#args-header-row
                          [:th "Arg#"]
                          [:th "Type"]
                          [:th "Value"]]
                         (conj
                           (pamelad-args n args)
                           [:tr#args-footer-row.first
                            [:td.button ;;"➕"
                             (om/build clickable add
                               {:react-key :add
                                :opts {:opts-id :add}})]
                            [:td.noborder.right {:colSpan 4}
                             (om/build button run
                               {:react-key :run
                                :opts {:opts-id :run}})]])))
                  ]
                 ]]])))))

(declare render-html-elements)

(defn element [cursor owner {:keys [opts-id] :as opts}]
  (reify om/IRender
    (render [_]
      (render-html-elements cursor opts-id))))

;; NOTE: Each element should have an unique :id
;; which will be used as the :react-key
;; FFI: https://github.com/omcljs/om/wiki/Internals:-Instances-are-getting-reused.-How%3F
(defn render-html-elements [cursor opts-id]
  (html
    (let [{:keys [e-order id style debug]} cursor
          id (or id (do (om/update! cursor [:id] opts-id) opts-id))
          style (or style "element")
          tag (keyword (str "div." style))
          self (if debug
                 (if (string? debug)
                   [:span debug]
                   (om/value debug)))
          elements (if (pos? (count e-order))
                     (for [e e-order]
                       (let [data (get cursor e)
                             e-type (get data :e-type :element)
                             view (get @e-types e-type element)]
                         (om/build view data {:react-key
                                              (cond
                                                (keyword? e) (name e)
                                                (string? e) e
                                                :else (str e))
                                              :opts {:opts-id e}}))))]
      (vec (cons tag (if self (cons self elements) elements))))))

(defn header-data []
  {:style "header"})

(defn elements-data []
  {:e-order [:header :pamelad]
   :style "none"
   :header (header-data)
   :pamelad (pamelad-data
              :clear (button-data
                       :type :reset
                       :label "Clear"
                       :on-click
                       #(let [args (rest (sort (keys (get-in @app-state [:elements :pamelad :args]))))]
                          (swap! app-state assoc-in [:elements :pamelad :add :data] 1)
                          (doseq [i args]
                            (let [type (keyword (str "text:" i))]
                              (set-type! type)))))
              :add (clickable-data
                     :label "➕"
                     :data 1
                     :on-click #(om/transact! % [:data] inc))
              :run (button-data
                     :type :submit
                     :label "Run"
                     )
              )})

(defn app [cursor owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (let [root-events (chan)]
        (sub (get-id-events) :root root-events)
        (go-loop []
          (let [{:keys [id]} (<! root-events)]
            ;; (println "app EVENT" id )
            (recur)))))
    om/IRender
    (render [_]
      (render-html-elements cursor :root))))

;; initialization =============================

;; add element type
(defn add-e-type! [e-type view]
  (swap! e-types assoc e-type view))

;; add element at ks
(defn add-e! [ks data]
  (if (pos? (count ks))
    (let [parents (vec (butlast ks))
          parent-e-order (conj parents :e-order)
          k (last ks)]
      (swap! app-state update-in parent-e-order conj k)
      (swap! app-state assoc-in ks data)
      )))

(defn initialize
  "Called to initialize the app when the DOM is ready or to re-initialize during development"
  []
  (let [prev-size (get-in @app-state [:style])
        events (chan)
        buf-fn (fn [id]
                 (case id
                   :root (dropping-buffer 4)
                   (sliding-buffer 3)))
        id-events (pub events :id buf-fn)]
    (println "program-mode:" program-mode)
    (reset! app-state
      {:e-order []
       :channels {:events events :id-events id-events}})
    (reset! e-types nil)
    (add-e-type! :element element)
    (add-e-type! :button button)
    (add-e-type! :checkbox checkbox)
    (add-e-type! :radio radio)
    (add-e-type! :clickable clickable)
    (add-e-type! :pamelad pamelad)
    (add-e! [:elements] (elements-data))
    (om/root app app-state {:target (gdom/getElement "app")})
    (set-size! (or prev-size :medium))
    ))

;; Normally we'll call initialize as soon as the JavaScript
;; window.onload function fires... For testing with PhantomJS however
;; we'll call initialize from testing/runner.cljs
(set! (.-onload js/window) initialize)

(defn figwheel-reload []
  (println "Figwheel reload...")
  (initialize))
