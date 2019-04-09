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

(ns testing.pamela.htn
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [clojure.data :refer [diff]]
            [environ.core :refer [env]]
            [me.raynes.fs :as fs]
            [clojure.test :refer :all]
            [pamela.log :as plog]
            [pamela.htn :refer :all]
            [pamela.cli :refer [reset-gensym-generator htn]]
            [plan-schema.utils :refer [fs-get-path fs-basename
                                       match-eval-out-err stderr-no-errors
                                       stdout-ignore stderr-ignore
                                       stdout-empty stderr-empty]]))

(deftest testing-pamela-htn
  (testing "testing-pamela-htn"
    (plog/initialize :warn ["*test*"])
    (is (= true (htn-isa? :htn-primitive-task :htn-task)))
    (let [top (fs/file (:user-dir env))
          top-path (str (fs-get-path top) "/")
          test-pamela (fs/file top "test" "pamela")
          pamela (fs/file test-pamela "HTN")
          regression (fs/file test-pamela "regression" "HTN")
          errors (fs/file test-pamela "errors")
          errors-htn (fs/file errors "HTN")
          examples (filter #(string/ends-with? (fs-basename %) ".root-task")
                     (concat
                       (sort-by fs/base-name (fs/list-dir pamela))
                       (sort-by fs/base-name (fs/list-dir regression))))
          neg-examples (filter #(string/ends-with? (fs-basename %) ".root-task")
                         (sort-by fs/base-name (fs/list-dir errors-htn)))
          pamela-htn (fs/file top "target" "parser" "HTN")
          regression-htn (fs/file top "target" "parser" "regression" "HTN")]
      (if-not (fs/exists? pamela-htn)
        (fs/mkdirs pamela-htn))
      (if-not (fs/exists? regression-htn)
        (fs/mkdirs regression-htn))
      (doseq [example examples]
        (reset-gensym-generator)
        (let [example-name (fs-basename example)
              example-path (fs-get-path example top-path)
              regression? (string/includes? example-path "/regression/")
              root-task (slurp example-path)
              ;; OLD
              ;; base-name (string/replace example-name #"\.root-task$" "")
              [_ base-name tag] (re-find
                                  (re-matcher
                                    #"(.*)\.([^.]+)\.root-task"
                                    example-name))
              _ (println "HTN" example-name
                  ;; "BASE-NAME" base-name
                  "TAG" tag
                  "regression?" regression?)
              _ (is (not (empty? base-name)))
              _ (is (not (empty? tag)))
              pamela-src-name (str base-name ".pamela")
              pamela-src-file (fs/file (fs/parent (fs/parent example))
                                pamela-src-name)
              pamela-src-path (fs-get-path pamela-src-file top-path)
              file-format "edn"
              example-htn-name (str base-name "." tag ".htn." file-format)
              example-htn-file (fs/file (fs/parent example) example-htn-name)
              example-htn-path (fs-get-path example-htn-file top-path)
              example-htn (if (fs/exists? example-htn-file)
                           (read-string (slurp example-htn-path))
                           {:error (str "Rubrique does not exist: "
                                     example-htn-path)})
              example-tpn-name (str base-name "." tag ".tpn." file-format)
              example-tpn-file (fs/file (fs/parent example) example-tpn-name)
              example-tpn-path (fs-get-path example-tpn-file top-path)
              example-tpn (if (fs/exists? example-tpn-file)
                           (read-string (slurp example-tpn-path))
                           {:error (str "Rubrique does not exist: "
                                     example-tpn-path)})
              specimen-htn-file (fs/file
                                 (if regression? regression-htn pamela-htn)
                                 example-htn-name)
              specimen-htn-path (fs-get-path specimen-htn-file top-path)
              specimen-tpn-file (fs/file
                                 (if regression? regression-htn pamela-htn)
                                 example-tpn-name)
              specimen-tpn-path (fs-get-path specimen-tpn-file top-path)
              output (fs-get-path
                       (fs/file (fs/parent specimen-htn-file)
                         (str base-name "." tag))
                       top-path)
              options {:input [pamela-src-path] :output output
                       :file-format file-format :root-task root-task}
              ;; _ (println "  OPTIONS" (pr-str options))
              exit-code (htn options)
              specimen-htn (if (fs/exists? specimen-htn-file)
                            (read-string (slurp specimen-htn-path))
                            {:error (str "Specimen HTN does not exist: "
                                      specimen-htn-path)})
              specimen-tpn (if (fs/exists? specimen-tpn-file)
                            (read-string (slurp specimen-tpn-path))
                            {:error (str "Specimen TPN does not exist: "
                                      specimen-tpn-path)})
              ;; [example-htn-only specimen-htn-only htn-both]
              ;; (diff example-htn specimen-htn)
              ;; [example-tpn-only specimen-tpn-only tpn-both]
              ;; (diff example-tpn specimen-tpn)
              ]
             (println "comparing:" specimen-htn-path example-htn-path)
             (println "comparing:" specimen-tpn-path example-tpn-path)
          (is (= 0 exit-code))
          (is (= example-htn specimen-htn))
          (is (= example-tpn specimen-tpn))
          ;; (is (= nil example-htn-only))
          ;; (is (= nil specimen-htn-only))
          ;; (is (= nil example-tpn-only))
          ;; (is (= nil specimen-tpn-only))
          ))
      ;; Negative examples that are *expected* to FAIL
      ;; NOTE: negative HTN examples currently do not use TAGs
      (doseq [neg-example neg-examples]
        (reset-gensym-generator)
        (let [neg-example-name (fs-basename neg-example)
              neg-example-path (fs-get-path neg-example)
              root-task (slurp neg-example-path)
              _ (println "Negative HTN" neg-example-name)
              base-name (string/replace neg-example-name #"\.root-task$" "")
              pamela-src-name (str base-name ".pamela")
              pamela-src-file (fs/file (fs/parent neg-example) pamela-src-name)
              pamela-src-path (fs-get-path pamela-src-file top-path)
              file-format "edn"
              output (fs-get-path (fs/file pamela-htn base-name) top-path)
              options {:input [pamela-src-path] :output output
                       :file-format file-format :root-task root-task}]
          (is (= [nil true true]
                (match-eval-out-err nil #"Embedding a :parallel is not supported within a defpmethod when used for HTN generation"
                  (htn options)
                  )))
          ))
      )))
