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

(ns testing.pamela.parser
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [environ.core :refer [env]]
            [me.raynes.fs :as fs]
            [avenir.utils :refer [and-fn]]
            [pamela.parser :refer :all]
            [pamela.cli :refer [reset-gensym-generator]]
            [pamela.utils :refer [output-file]]
            [plan-schema.utils :refer [fs-get-path fs-basename]]))

(def update-mismatched-txt-rubrics?
  "Instead of signalling a test failure, any mismatches in the rubric for 'check' operations will
  cause the 'correct' rubric to be updated to the current value."
  false)

(deftest testing-pamela-parser
  (testing "testing-pamela-parser"
    (is (= [0 0] zero-bounds))
    (let [top (fs/file (:user-dir env))
          top-path (str (fs-get-path top) "/")
          pamela (fs/file top "test" "pamela")
          regression (fs/file pamela "regression")
          errors (fs/file pamela "errors")
          errors-ir (fs/file errors "IR")
          examples (filter #(string/ends-with? (fs-basename %) ".pamela")
                           (concat
                            (sort-by fs/base-name (fs/list-dir pamela))
                            (sort-by fs/base-name (fs/list-dir regression))))
          neg-examples (filter #(string/ends-with? (fs-basename %) ".pamela")
                               (sort-by fs/base-name (fs/list-dir errors-ir)))
          pamela-ir-dir (fs/file top "target" "parser" "IR")
          regression-ir-dir (fs/file top "target" "parser" "regression" "IR")]
      (if-not (fs/exists? pamela-ir-dir)
        (fs/mkdirs pamela-ir-dir))
      (if-not (fs/exists? regression-ir-dir)
        (fs/mkdirs regression-ir-dir))
      (doseq [example examples]
        (reset-gensym-generator)
        (let [example-name (fs-basename example)
              example-path (fs-get-path example)
              regression? (string/includes? example-path "/regression/")
              ;; Build IR testing
              example-ir-name (string/replace example-name
                                              #"\.pamela$" ".ir.edn")
              example-ir-file (fs/file (fs/parent example) "IR" example-ir-name)
              example-ir-path (fs-get-path example-ir-file top-path)
              example-ir (if (fs/exists? example-ir-file)
                           (read-string (slurp example-ir-path))
                           {:error (str "Rubrique does not exist: "
                                        example-ir-path)})
              specimen-ir-file (fs/file
                                (if regression? regression-ir-dir pamela-ir-dir)
                                example-ir-name)
              specimen-ir-path (fs-get-path specimen-ir-file top-path)
              options {:input [example-path]}
              specimen-ir (parse options)
              _ (output-file specimen-ir-path "edn" specimen-ir) ;; will sort
              specimen-ir (if (fs/exists? specimen-ir-file)
                            (read-string (slurp specimen-ir-path))
                            {:error (str "Specimen does not exist: "
                                         specimen-ir-path)})
              ;; Check Parse (TXT) testing
              example-txt-name (string/replace example-name
                                               #"\.pamela$" ".txt")
              example-txt-file (fs/file (fs/parent example) "IR" example-txt-name)
              example-txt-path (fs-get-path example-txt-file top-path)
              example-txt (if (fs/exists? example-txt-file)
                            (read-string (slurp example-txt-path))
                            {:error (str "Rubrique does not exist: "
                                         example-txt-path)})
              specimen-txt-file (fs/file
                                 (if regression? regression-ir-dir pamela-ir-dir)
                                 example-txt-name)
              specimen-txt-path (fs-get-path specimen-txt-file top-path)
              specimen-txt (parse-pamela-file example-path)
              _ (output-file specimen-txt-path "edn" specimen-txt)
              specimen-txt (if (fs/exists? specimen-txt-file)
                            (read-string (slurp specimen-txt-path))
                            {:error (str "Specimen does not exist: "
                                         specimen-txt-path)})]
          (println "CHECK" example-name
                   "\n  RUBRIC" example-txt-path
                   "\n  SPECIMEN" specimen-txt-path)
          (if update-mismatched-txt-rubrics?
            (when (not (= example-txt specimen-txt))
              (println "Updating the incorrect RUBRIC" example-txt-path)
              (output-file example-txt-path "edn" specimen-txt))
            (is (= example-txt specimen-txt)))
          (println "BUILD" example-name
                   "\n  RUBRIC" example-ir-path
                   "\n  SPECIMEN" specimen-ir-path)
          (is (= example-ir specimen-ir))))
      ;; Negative examples that are *expected* to FAIL
      (doseq [neg-example neg-examples]
        (reset-gensym-generator)
        (let [neg-example-name (fs-basename neg-example)
              neg-example-path (fs-get-path neg-example)
              options {:input [neg-example-path]}
              specimen-ir (parse options)]
          ;; (println "BUILD" neg-example-name)
          (is (not (nil? (:error specimen-ir))))))
      )))
