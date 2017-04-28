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
            [testing.pamela.parser :refer [fs-get-path]]
            [pamela.cli :refer :all]))

;; out-expect should
;;   nil if we don't care about matching stdout
;;   #"regex" -- a string (converted to regex) or regex that stdout should match
;;     NOTE newlines are stripped from stdout before the match such that
;;     the expression '.*' will match across lines
;; err-expect is analogous to out-expect, for stderr
;; the return value is [rv out-match err-match]
;; where
;;    rv is the value of the evaluated body
;;    out-match is true for a match (or if we don't care),
;;       else it is an error string explaining the match failed
;;    err-match is analogous to out-match, but for stderr
(defmacro match-eval-out-err
  "Evaluates exprs in a context in which *out* and *err* are bound to a fresh
  StringWriter.  Prints *out* and *err* after execution."
  [out-expect err-expect & body]
  `(let [out# (new java.io.StringWriter)
         err# (new java.io.StringWriter)
         [rv# out-str# err-str#]
         (binding [*out* out#
                   *err* err#]
           (let [exception# (atom nil)
                 raw-rv# (try
                           ~@body
                           (catch Throwable e#
                             (reset! exception# (.getMessage e#))
                             nil))
                 raw-out# (str out#)
                 raw-err# (str err#)
                 except-err# (if @exception#
                               (str "EXCEPTION:\n" @exception#
                                 "\nERROR:\n" raw-err#)
                               raw-err#)]
             [raw-rv# raw-out# except-err#]))
         out-pattern# (cond
                        (nil? ~out-expect)
                        nil
                        (string? ~out-expect)
                        (re-pattern ~out-expect) ;; promote to pattern
                        (instance? java.util.regex.Pattern ~out-expect)
                        ~out-expect
                        :else
                        nil)
         out-rv# (or (not out-pattern#) ;; we don't care about out
                   (if (re-find out-pattern#
                         (string/replace out-str# "\n" ""))
                     true ;; we have a match
                     (str "stdout did NOT match '" out-pattern# "' ===\n"
                       out-str# "\n===")))
         err-pattern# (cond
                        (nil? ~err-expect)
                        nil
                        (string? ~err-expect)
                        (re-pattern ~err-expect) ;; promote to pattern
                        (instance? java.util.regex.Pattern ~err-expect)
                        ~err-expect
                        :else
                        nil)
         err-rv# (or (not err-pattern#) ;; we don't care aberr err
                   (if (re-find err-pattern#
                         (string/replace err-str# "\n" ""))
                     true ;; we have a match
                     (str "stderr did NOT match '" err-pattern# "' ===\n"
                       err-str# "\n===")))]
     ;; DEBUG
     ;; (println "OUT=>" out-str# "<=OUT")
     ;; (println "ERR=>" err-str# "<=ERR")
     ;; (println "MATCH?" err-pattern# "="(re-find err-pattern# err-str#))
     [rv# out-rv# err-rv#]))

(deftest testing-pamela-cli
  (testing "testing-pamela-cli"
    (is (= log-levels #{"trace" "debug" "info" "warn" "error" "fatal" "report"}))
    (is (= output-formats #{"edn" "json"}))
    (is (= (sort (keys actions)) '("build" "check" "htn" "tpn")))
    (is (= (base-64-decode "KGNvaW4uZmxpcC0zKQ==") "(coin.flip-3)"))

    (set-test-mode! true) ;; no need to set repl-mode

    ;; check PASS
    (is (= [0 true true]
          (match-eval-out-err #":defpclass.*pwrvals" nil
            (pamela "-i" "test/pamela/circuit.pamela" "check" )
            )))
        ;; check FAIL
    (is (= [1 true true]
          (match-eval-out-err nil #"bogus.pamela.*INPUT file does not exist"
            (pamela "-i" "bogus.pamela" "check")
            )))

    ;; NOTE: comprehensive build coverage tests have moved to parser.clj
    ;; build PASS
    (is (= [0 true true]
          (match-eval-out-err "\\{bulb \\{:type :pclass" nil
            (pamela "-i" "test/pamela/circuit.pamela" "build" )
            )))
    ;; build FAIL
    (is (= [1 true true]
          (match-eval-out-err nil #"unable to parse:.*issue-86b.pamela"
            (pamela "-i" "test/pamela/errors/IR/issue-86b.pamela" "build")
            )))

    ;; tpn PASS
    (is (= [0 true true]
          (match-eval-out-err "tpn-type.*activity" nil
            (pamela "-i" "test/pamela/plant.pamela"
              "-i" "test/pamela/parallel-choice.tpn.pamela"
              "tpn" ))))
    ;; tpn FAIL
    (is (= [1 true true]
          (match-eval-out-err nil "unable to find the default TPN method: please specify --construct-tpn"
            (pamela "-i" "test/pamela/parallel-choice.tpn.pamela" "tpn")
            )))

    ;; NOTE: comprehensive htn tests are in htn.clj
    ;; htn PASS
    (is (= [0 true true]
          (match-eval-out-err "type.*htn-expanded-method" nil
          (pamela "-i" "test/pamela/lvar-examples.pamela"
            "-t" "(example.imager.main)"
            "htn")
          )))
    ;; htn FAIL
    (is (= [1 true true]
          (match-eval-out-err nil "Embedding a :parallel is not supported within a defpmethod when used for HTN generation"
          (pamela "-i" "test/pamela/tpn-demo.pamela"
            "-t" "(tpn.elephant)"
            "htn")
          )))
    ))
