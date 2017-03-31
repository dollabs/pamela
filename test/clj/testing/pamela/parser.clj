<;; Copyright © 2016 Dynamic Object Language Labs Inc.
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
            [pamela.utils :refer [output-file]]))

(defn fs-get-path [file & [prefix]]
  (let [path (.getPath file)]
    (if prefix
      (string/replace path prefix "")
      path)))

(deftest testing-pamela-parser
  (testing "testing-pamela-parser"
    (reset-gensym-generator)
    (is (= [0 0] zero-bounds))
    (let [top (fs/file (:user-dir env))
          top-path (str (fs-get-path top) "/")
          pamela (fs/file top "test" "pamela")
          regression (fs/file pamela "regression")
          examples (filter #(string/ends-with? (fs-file-name %) ".pamela")
                     (concat
                       (sort-by fs/base-name (fs/list-dir pamela))
                       (sort-by fs/base-name (fs/list-dir regression))))
          pamela-ir (fs/file top "target" "parser" "IR")
          regression-ir (fs/file top "target" "parser" "regression" "IR")]
      (if-not (fs/exists? pamela-ir)
        (fs/mkdirs pamela-ir))
      (if-not (fs/exists? regression-ir)
        (fs/mkdirs regression-ir))
      (doseq [example examples]
        (let [example-name (fs-file-name example)
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
                                 (if regression? regression-ir pamela-ir)
                                 example-ir-name)
              specimen-ir-path (fs-get-path specimen-ir-file top-path)
              options {:input [example-path]}
              specimen-ir (parse options)
              _ (output-file specimen-ir-path "edn" specimen-ir) ;; will sort
              specimen-ir (if (fs/exists? specimen-ir-file)
                            (read-string (slurp specimen-ir-path))
                            {:error (str "Specimen does not exist: "
                                      specimen-ir-path)})]
          ;; (println "BUILD" example-name "\n  RUBRIQUE" example-ir-path
          ;;   "\n  SPECIMEN" specimen-ir-path)
          (is (= example-ir specimen-ir)))))))
