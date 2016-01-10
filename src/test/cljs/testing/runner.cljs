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

(ns testing.runner
  (:require-macros [cljs.test :refer (run-tests)])
  (:require [goog.dom :as gdom]
            [cljs.test]
            [pamela.utils :as utils]
            [pamela.client :refer (initialize)]
            [testing.pamela.client]))

;; pending the resolution of
;; CLJS-1217 cljs.test/run-tests with default env has no way to access summary
;; http://dev.clojure.org/jira/browse/CLJS-1217

(def summary (atom nil))

(defmethod cljs.test/report [::testing :pass] [m]
  ((get-method cljs.test/report [:cljs.test/default :pass]) m))

(defmethod cljs.test/report [::testing :begin-test-ns] [m]
  ((get-method cljs.test/report [:cljs.test/default :begin-test-ns]) m))

(defmethod cljs.test/report [::testing :error] [m]
  ((get-method cljs.test/report [:cljs.test/default :error]) m))

(defmethod cljs.test/report [::testing :fail] [m]
  ((get-method cljs.test/report [:cljs.test/default :fail]) m))

(defmethod cljs.test/report [::testing :summary] [m]
  (reset! summary m)
  ((get-method cljs.test/report [:cljs.test/default :summary]) m))

;; --------------------------------------------------------------

(defn printfn [& args]
  (let [out (gdom/getElement "out")
        output (.-value out)]
    (set! (.-value out) (str output (apply println-str args)))
    nil))

(defn flushout []
  (let [out (gdom/getElement "out")
        output (.-value out)]
    (set! (.-value out) "")
    output))

;; the runtests function will be called
;; by phantomjs from src/test/phantomjs/unit-test.js
;; by selenium from run-firefox in src/test/clj/testing/selenium.clj
(defn ^:export runtests [console?]
  (let [old-print-fn *print-fn*]
    (flushout)
    (set! *print-newline* false)
    (if console?
      (enable-console-print!)
      (set-print-fn! printfn))
    (println "Running cljs test...")
    (initialize)
    (let [env (cljs.test/empty-env ::testing)
          old-summary (run-tests env 'testing.pamela.client)
          passed? (cljs.test/successful? @summary)]
      (println @summary)
      (if passed?
        (println "Test succeeded.")
        (println "--- Test failed! ---"))
      (set-print-fn! old-print-fn)
      (if passed? 0 1))))
