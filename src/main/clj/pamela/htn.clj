;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;; Acknowledgement and Disclaimer:
;; This material is based upon work supported by the Army Contracting
;; and DARPA under contract No. W911NF-15-C-0005.
;; Any opinions, findings and conclusions or recommendations expressed
;; in this material are those of the author(s) and do necessarily reflect the
;; views of the Army Contracting Command and DARPA.

(ns pamela.htn
  "HTN functions."
  (:require [clojure.java.io :refer :all] ;; for as-file
            ;; [clojure.data :refer [diff]]
            ;; [clojure.java.shell :refer [sh]]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            ;; [clojure.walk :refer [prewalk postwalk]]
            [avenir.utils :refer [concatv assoc-if keywordize]]
             [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [clojure.data.json :as json]
            [instaparse.core :as insta]
            [pamela.parser :as parser]
            ))

;; -----------------------------------------------------------------------

(defn ir-root-task [& args]
  (vec args))

(def root-task-ir {
                :argval identity
                :boolean parser/ir-boolean
                ;; :float handled in :ir-number
                :integer parser/ir-integer
                :keyword keyword
                :natural parser/ir-integer
                :number parser/ir-number
                :root-task ir-root-task
                :safe-keyword identity
                :string identity
                :symbol symbol
                })

;; return [pclass method args]
(defn identify-root-task [ir root-task]
  (log/warn "ID RT" root-task)
  (let [parser (parser/build-parser "root-task.ebnf")
        tree (if root-task
               (insta/parses parser root-task))
        [pclass method & args]
        (cond
          (nil? tree)
          nil ;; FIND main in ir w/ no args
          (insta/failure? tree)
          (do
            (log/errorf "parse: invalid root-task: %s" root-task)
            (log/errorf (with-out-str (pprint (insta/get-failure tree))))
            false)
          (not= 1 (count tree))
          (do
            (log/errorf "parse: grammar is ambiguous for root-task: %s" root-task)
            (log/errorf (with-out-str (pprint tree)))
            false)
          :else
          (insta/transform root-task-ir (first tree)))
        args (vec args)
        ir-syms (keys ir)]
    (if (and pclass method) ;; verify
      (let [pclass-def (get ir pclass)
            method-def (if pclass-def (get-in pclass-def [:methods method]))
            method-args (if method-def (get method-def :args))]
        (if-not pclass-def
          (let [msg (str "root-task pclass not found: " pclass)]
            (log/error msg)
            (throw (AssertionError. msg)))
          (if-not method-def
            (let [msg (str "root-task pclass " pclass " does not have a method " method)]
              (log/error msg)
              (throw (AssertionError. msg)))
            (if (or (not method-args) (not= (count args) (count method-args)))
              (let [msg (str "root-task args \"" args "\" does not match arity of " pclass "." method " " method-args)]
                (log/error msg)
                (throw (AssertionError. msg)))
              [pclass method args]))))
      (loop [pclass nil method nil k (first ir-syms) more (rest ir-syms)] ;; find main
        (if (or (and pclass method) (not k))
          (if-not (and pclass method)
            (let [msg "root-task pclass with a zero arg main not found"]
              (log/error msg)
              (throw (AssertionError. msg)))
            [pclass method []])
          (let [k-def (get ir k)
                {:keys [type methods]} k-def
                main (if (and (= type :pclass) methods)
                       (get methods 'main))
                main-args (if main (get main :args))
                pclass (if (and main (empty? main-args)) k pclass)
                method (if (and main (empty? main-args)) 'main method)]
            (recur pclass method (first more) (rest more))))))))

;; if the root-task is nil
;;   look for htn-pclass with a "main" pmethod
;;   if main takes arguments throw exception
;; else
;;   expect that the htn-pclass and method are given as well as
;;   any literal args for that method (number of args should match)
;;   NOTE: argval = ( symbol | boolean | string | number | safe-keyword )
;;   -- here symbol would not be supported
;;   NOTE: may need equivalent of the "?variable", perhaps
;; "-t" "(isr-htn.main \"A\" \"B\" \"C\" {:type :lvar, :name \"last-location\"})"
;;
;;
;; RETURNS htn data structure in Clojure (optionally converted to JSON in cli.clj)
(defn plan-htn [ir root-task]
  (let [[pclass method args] (identify-root-task ir root-task)
        rt {:type :root-task
            :pclass pclass
            :method method
            :args args}]
    (assoc ir 'root-task rt)))
