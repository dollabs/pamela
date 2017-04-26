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

(ns testing.pamela.cli
  (:require [clojure.test :refer :all]
            [me.raynes.fs :as fs]
            [testing.pamela.parser :refer [fs-get-path]]
            [pamela.cli :refer :all]))

(deftest testing-pamela-cli
  (testing "testing-pamela-cli"
    (is (= log-levels #{"trace" "debug" "info" "warn" "error" "fatal" "report"}))
    (is (= output-formats #{"edn" "json"}))
    (is (= (sort (keys actions)) '("build" "check" "htn" "tpn")))
    (is (= (base-64-decode "KGNvaW4uZmxpcC0zKQ==") "(coin.flip-3)"))

    (set-test-mode! true) ;; no need to set repl-mode

    (binding [*out* (new java.io.StringWriter)] ;; discard *out*
      ;; check PASS
      (is (= 0 (pamela "-i" "test/pamela/circuit.pamela" "check" )))
      ;; check FAIL
      (is (= 1 (pamela "-i" "bogus.pamela" "check")))
      ;; build PASS
      (is (= 0 (pamela "-i" "test/pamela/circuit.pamela" "build" )))
      ;; build FAIL
      (is (= 1 (pamela "-i" "test/pamela/errors/IR/issue-86b.pamela" "build")))
      ;; tpn PASS
      (is (= 0 (pamela "-i" "test/pamela/plant.pamela"
                 "-i" "test/pamela/parallel-choice.tpn.pamela"
                 "tpn" )))
      ;; tpn FAIL
      (is (= 1 (pamela "-i" "test/pamela/parallel-choice.tpn.pamela" "tpn")))
      ;; htn PASS
      (is (= 0 (pamela "-i" "test/pamela/lvar-examples.pamela"
                 "-t" "(example.imager.main)"
                 "htn")))
      ;; htn FAIL
      (is (= 1 (pamela "-i" "test/pamela/tpn-demo.pamela"
                 "-t" "(tpn.elephant)"
                 "htn")))
      )
    ))
