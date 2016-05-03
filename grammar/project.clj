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

(defproject pamela-grammar "0.2.2-SNAPSHOT"
  :description "Probabalistic Advanced Modeling and Execution Learning Architecture (PAMELA)"
  :url "https://github.com/dollabs/pamela"
  :scm {:url "https://github.com/dollabs/pamela.git"}
  :license {:name "Apache License, Version 2.0"
            :url "http://opensource.org/licenses/Apache-2.0"}
  :dependencies [[org.clojure/clojure "1.8.0" :scope "provided"]
                 [instaparse "1.4.2"]
                 [rhizome "0.2.5"]
                 [environ "1.0.2"]]

  :plugins [[lein-environ "1.0.2"]]

  :main pamela-grammar.parser

  :source-paths ["src/main/clj"]
  :profiles
  {:dev {:env {:program-mode "dev"}
         :test-paths ["src/test/clj"]}})
