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
            [clojure.pprint :as pp]
            ;; [clojure.walk :refer [prewalk postwalk]]
            [avenir.utils :refer [concatv assoc-if keywordize]]
             [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [clojure.data.json :as json]))

;; -----------------------------------------------------------------------

;; if the root-task is nil
;;   look for htn-pclass with a zero arg main pmethod
;;   then look for a pclass that has a :pclass-ctor for the htn-pclass with zero args
;;   then we know how to construct pclass, htn-class and handle the main method
;; else
;;   do as above AND find the root-task label within the body of main, then handle
;;
;; RETURNS htn data structure in Clojure (optionally converted to JSON in cli.clj)
(defn plan-htn [ir root-task]
  ;; DEBUGGING
  (assoc ir 'root-task root-task)
  )
