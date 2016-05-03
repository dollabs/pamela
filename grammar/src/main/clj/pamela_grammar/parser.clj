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

(ns pamela-grammar.parser
  "The PAMELA parser."
  (:require [clojure.java.io :refer :all]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [instaparse.core :as insta]
            [environ.core :refer [env]])
  )

(defn build-parser []
  (let [ebnf (slurp (resource "data/pamela.ebnf"))
        whitespace (insta/parser "whitespace = #'([,\\s]+|;.*\\n)+'")
        parser (insta/parser ebnf
                 :input-format :ebnf
                 :auto-whitespace whitespace)]
    parser))

(defn test-grammar [parser src results-dir]
  (let [testname (string/replace (.getName src) ".pamela" "")
        _ (println "Checking grammar for:" (.getName src))
        tree (parser (slurp src))]
    (if (insta/failure? tree)
      (do
        (pp/pprint (insta/get-failure tree))
        false)
      (let [txt (as-file (str (.getPath results-dir) "/" testname ".txt"))
            png (as-file (str (.getPath results-dir) "/" testname ".png"))]
        (println "Success! parse tree in: " (.getPath txt))
        (spit txt (with-out-str (pp/pprint tree)))
        (println "parse diagram in: " (.getPath png))
        (insta/visualize tree :output-file (.getPath png) :options {:dpi 72})
        true))))

(defn test-grammar-all []
  (let [regression (as-file "../src/test/pamela/regression")
        srcs (and (.exists regression) (.listFiles regression))
        analysis (as-file "target/analysis")
        parser (build-parser)]
    (when srcs
      (if-not (.exists analysis)
        (.mkdirs analysis))
      (doseq [src srcs]
        (test-grammar parser src analysis)))))

(defn test-grammar-one [filename]
  (let [file (as-file filename)
        cwd (or (:pamela-cwd env) (:user-dir env))
        file (if (.exists file) file
                 (as-file (str cwd "/" filename)))]
    (if-not (.exists file)
      (println "ERROR: input file does not exist: " filename)
      (let [parser (build-parser)]
        (test-grammar parser file (.getParentFile file))))))

(defn exit
  "Exit PAMELA grammar with given status code (and optional messages)."
  {:added "0.2.0"}
  [status & msgs]
  (if msgs
    (binding [*out* *err*]
      (println (string/join \newline msgs))))
  (flush) ;; ensure all pending output has been flushed
  (shutdown-agents)
  (System/exit status))

(defn -main
  "PAMELA grammar entry point"
  {:added "0.2.0"}
  [& args]
  (try
    (case (count args)
      0 (test-grammar-all)
      1 (if-not (test-grammar-one (first args)) (exit 1 "Failure!"))
      (println "analyze: usage analyze [pamela-file]"))
    (catch Throwable e ;; note AssertionError not derived from Exception
      (exit 1 "ERROR caught exception:" (.getMessage e))))
  (exit 0))
