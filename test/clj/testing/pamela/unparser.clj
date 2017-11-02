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

(ns testing.pamela.unparser
  (:require [clojure.test :refer :all]
            [clojure.string :as string]
            [environ.core :refer [env]]
            [me.raynes.fs :as fs]
            [avenir.utils :refer [and-fn]]
            [pamela.unparser :refer :all]
            [pamela.parser :refer [parse]]
            [pamela.cli :refer [reset-gensym-generator]]
            [pamela.utils :refer [output-file]]
            [plan-schema.utils :refer [fs-get-path fs-basename]]))

(deftest testing-pamela-unparser
  (testing "testing-pamela-unparser"
    (let [excludes #{"biased-coin.pamela" ;; #127
                     "ir-test.pamela" ;; #127, note betweens OK
                     ;; regression/ ------------------------------------
                     "switch-bulb.example" ;; #127
                     "tpn-slack.pamela" ;; #127
                     }
          top (fs/file (:user-dir env))
          top-path (str (fs-get-path top) "/")
          pamela (fs/file top "test" "pamela")
          regression (fs/file pamela "regression")
          test-example? (fn [path]
                          (let [filename (fs-basename path)]
                            (and (string/ends-with? filename ".pamela")
                              (not (excludes filename)))))
          examples (filter test-example?
                     (concat
                       (sort-by fs/base-name (fs/list-dir pamela))
                       (sort-by fs/base-name (fs/list-dir regression))))
          pamela-unparse (fs/file top "target" "parser" "UNPARSE")
          regression-unparse (fs/file top "target" "parser" "regression" "UNPARSE")]
      (if-not (fs/exists? pamela-unparse)
        (fs/mkdirs pamela-unparse))
      (if-not (fs/exists? regression-unparse)
        (fs/mkdirs regression-unparse))
      (doseq [example examples]
        (let [example-name (fs-basename example)
              example-path (fs-get-path example)
              regression? (string/includes? example-path "/regression/")
              example-ir-name (string/replace example-name
                                #"\.pamela$" ".ir.edn")
              example-ir-file (fs/file (fs/parent example) "IR" example-ir-name)
              example-ir-path (fs-get-path example-ir-file top-path)
              example-ir (if (fs/exists? example-ir-file)
                           (read-string (slurp example-ir-path))
                           {:error (str "Rubrique does not exist: "
                                     example-ir-path)})
              specimen-ir-file (fs/file
                                 (if regression? regression-unparse pamela-unparse)
                                 example-ir-name)
              specimen-ir-path (fs-get-path specimen-ir-file top-path)
              specimen-src-file (fs/file
                                 (if regression? regression-unparse pamela-unparse)
                                 example-name)
              specimen-src-path (fs-get-path specimen-src-file top-path)
              _ (println "UNPARSE" example-name
                  ;; "\n  RUBRIC" example-ir-path
                  ;; "\n  SPECIMEN" specimen-ir-path
                  ;; "\n  KEYS" (keys example-ir)
                  )
              specimen-src (unparse example-ir)
              _ (output-file specimen-src-path "raw" specimen-src)
              options {:input [specimen-src-path]}
              specimen-ir (parse options)
              _ (output-file specimen-ir-path "edn" specimen-ir) ;; will sort
              specimen-ir (if (fs/exists? specimen-ir-file)
                            (read-string (slurp specimen-ir-path))
                            {:error (str "Specimen does not exist: "
                                      specimen-ir-path)})]
          ; example is expected, specimen is current.
          (is (= example-ir specimen-ir) (str "IR files should match\nExpected: " example-ir-path "\nCurrent: " specimen-ir-file))))
      )))
