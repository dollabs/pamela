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

(ns pamela.core
  "The PAMELA command line utility.

  This can be run during development with:
  lein run arg1 arg2 [...]

  Or after the uberjar has been generated (via 'lein prod'):
  java -jar target/uberjar/pamela.jar  arg1 arg2 [...]"
  (:require [pamela.cli :as cli])
  (:gen-class)) ;; required for uberjar

(defn -main
  "PAMELA main entry point (see pamela)"
  {:added "0.2.0"}
  [& args]
  (apply cli/pamela args))
