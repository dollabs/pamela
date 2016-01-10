;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;;; Acknowledgement and Disclaimer:
;;; This material is based upon work supported by the Army Contracting
;;; and DARPA under contract No. W911NF-15-C-0005.
;;; Any opinions, findings and conclusions or recommendations expressed
;;; in this material are those of the author(s) and do necessarily reflect the
;;; views of the Army Contracting Command and DARPA.

(ns pamela.utils
  (:require [goog.string :as gstring]
            [goog.object :as gobject]
            [goog.array :as garray])
  (:import [goog.string StringBuffer]))

;; program-mode --------------------------------------------

;; http://www.martinklepsch.org/posts/parameterizing-clojurescript-builds.html
(goog-define program-mode "uninitialized")

(defn development?
  "Returns true if in :dev mode"
  []
  (= program-mode "dev"))

(defn testing?
  "Returns true if in :test mode"
  []
  (= program-mode "test"))

(defn production?
  "Returns true if in :prod mode"
  []
  (= program-mode "prod"))

;; printing helpers ---------------------------------

;; replaces any (fn? v) in the map m with "<fn>", recursively
(defn remove-fn [ & args ]
  (case (count args)
    1 (let [v (first args)]
        (cond
          (fn? v) "<fn>"
          (vector? v) (mapv remove-fn v)
          (map? v) (reduce-kv remove-fn {} v)
          :else v))
    3 (let [[m k v] args]
        (assoc m k
          (cond
            (fn? v) "<fn>"
            (vector? v) (mapv remove-fn v)
            (map? v) (reduce-kv remove-fn {} v)
            :else v)))
    "<unknown>"))

;; associate k v in m iff v
(defn assoc-if [m k v]
  (if v
    (assoc m k v)
    m))

;; find the index of o in v
(defn vec-index-of [v o]
  (first
    (remove nil?
      (for [i (range (count v))]
        (if (= (get v i) o) i)))))

;; web -------------------------------------------------------

(def secure-protocols {"https" true
                       "wss" true})

(defn protocol-secure [protocol secure]
  (if secure
    (if (get secure-protocols protocol false)
      protocol ;; already secure
      (str protocol "s")) ;; add the "s"
    (if (get secure-protocols protocol false)
      (subs protocol 0 (dec (count protocol))) ;; remove the "s"
      protocol)))

;; without any opts make-url returns the "base-url" for this site
(defn make-url [& {:keys [protocol hostname port uri secure] :as opts}]
  (let [location (.-location js/document)
        href (.-href location)
        [base anchor] (gstring/splitLimit href "#" 2)
        p (let [p (.-protocol location)] (subs p 0 (dec (count p)))) ;; no ":"
        secure (or secure (get secure-protocols p false))
        protocol (or protocol p)
        protocol (protocol-secure protocol secure)
        hostname (or hostname (.-hostname location))
        port (str (or port (.-port location)))
        server (str hostname (if-not (empty? port) (str ":" port)))
        uri (or uri "/")
        url (str protocol "://" server uri)]
    (if (and (empty? opts) (= protocol "file"))
      base
      url)))
