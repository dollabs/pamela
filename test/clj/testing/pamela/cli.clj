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
            [clojure.string :as string]
            [me.raynes.fs :as fs]
            [pamela.cli :refer :all]
            [plan-schema.utils :refer [fs-get-path re-pattern-opts
                                       match-eval-out-err stderr-no-errors
                                       stdout-ignore stderr-ignore
                                       stdout-empty stderr-empty]]))

(deftest testing-pamela-cli
  (testing "testing-pamela-cli"
    (is (= log-levels #{"trace" "debug" "info" "warn" "error" "fatal" "report"}))
    (is (= output-formats #{"edn" "json"}))
    (is (= (sort (keys actions)) '("build" "check" "htn" "tpn" "unparse")))
    (is (= (base-64-decode "KGNvaW4uZmxpcC0zKQ==") "(coin.flip-3)"))

    (set-test-mode! true) ;; no need to set repl-mode

    ;; check PASS
    (is (= [0 true true]
          (match-eval-out-err
            #"^\[:pamela\n \[:defpclass\n  \[:symbol \"pwrvals\"\]"
            stderr-no-errors
            (pamela "-i" "test/pamela/circuit.pamela" "check" )
            )))
    ;; check FAIL
    (is (= [1 true true]
          (match-eval-out-err stdout-ignore
            #"bogus.pamela.*INPUT file does not exist"
            (pamela "-i" "bogus.pamela" "check")
            )))

    ;; NOTE: comprehensive build coverage tests have moved to parser.clj
    ;; build PASS
    (is (= [0 true true]
          (match-eval-out-err
            (re-pattern-opts "^\\{bulb.*\\{:args.*\\[vcc vdd\\]," :dotall)
            stderr-no-errors
            (pamela "-i" "test/pamela/circuit.pamela" "build" )
            )))
    ;; build FAIL
    (is (= [1 true true]
          (match-eval-out-err stdout-ignore #"unable to parse:.*issue-86b.pamela"
            (pamela "-i" "test/pamela/errors/IR/issue-86b.pamela" "build")
            )))

    ;; tpn PASS
    (is (= [0 true true]
          (match-eval-out-err "tpn-type.*activity" stderr-no-errors
            (pamela "-i" "test/pamela/plant.pamela"
              "-i" "test/pamela/parallel-choice.tpn.pamela"
              "tpn" ))))
    ;; tpn FAIL
    (is (= [1 true true]
          (match-eval-out-err stdout-ignore
            "unable to find the default TPN method: please specify --construct-tpn"
            (pamela "-i" "test/pamela/parallel-choice.tpn.pamela" "tpn")
            )))

    ;; NOTE: comprehensive htn tests are in htn.clj
    ;; htn PASS
    (is (= [0 true true]
          (match-eval-out-err "type.*htn-expanded-method" stderr-no-errors
            (pamela "-i" "test/pamela/lvar-examples.pamela"
              "-t" "(example.imager.main)"
              "htn")
            )))
    ;; htn FAIL
    (is (= [1 true true]
          (match-eval-out-err stdout-ignore
            "Embedding a :parallel is not supported within a defpmethod when used for HTN generation"
            (pamela "-i" "test/pamela/tpn-demo.pamela"
              "-t" "(tpn.elephant)"
              "htn")
            )))

    ;; basic unparsing test
    (is (= [0 true true]
          (match-eval-out-err
            #"defpclass one"
            stderr-no-errors
            (pamela "-i" "test/pamela/IR/one.ir.edn" "unparse")
            )))
    ))
