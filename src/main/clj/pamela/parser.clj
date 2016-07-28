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

(ns pamela.parser
  "The parser for the PAMELA language"
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            [environ.core :refer [env]]
            [clojure.java.io :refer :all] ;; for as-file
            [pamela.utils :refer [make-url get-url]]
            [avenir.utils :refer [and-fn assoc-if vec-index-of concatv]]
            [instaparse.core :as insta]
            ))

(defn pamela-filename? [filename]
  (string/ends-with? filename ".pamela"))

(defn build-parser []
  (let [ebnf (slurp (resource "data/pamela.ebnf"))
        whitespace (insta/parser "whitespace = #'([,\\s]+|;.*\\n)+'")
        parser (insta/parser ebnf
                 :input-format :ebnf
                 :auto-whitespace whitespace)]
    parser))

;; return PIR
(defn parse [cwd filename]
  (let [file (as-file filename)
        file (if (.exists file) file
                 (as-file (str cwd "/" filename)))]
    (if (.exists file)
      (let [parser (build-parser)
            tree (insta/parses parser (slurp file))]
        (if (insta/failure? tree)
          (do
            (println "PARSE FAILURE")
            (pprint (insta/get-failure tree))
            false)
          (with-out-str (pprint (first tree))))))))
