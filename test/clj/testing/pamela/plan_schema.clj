;; Copyright © 2017 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns testing.pamela.plan-schema
  (:require [clojure.test :refer :all]
            [pamela.cli :as pcli]
            [plan-schema.cli :as scli]
            [plan-schema.core :as psc]

            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer :all])
  (:import [java.io.File]))

(def pamela-files [["test/pamela/cannon.pamela" "(game.main)"]])
(def outdir "test/gen-files/")
(def htn-edn-suffix ".htn.edn")
(def htn-json-suffix ".htn.json")
(def tpn-edn-suffix ".tpn.edn")
(def tpn-json-suffix ".tpn.json")

(defn get-pamela-name
  "Given a file-name ending with abc.pamela, return abc
  If the file-name does not end with pamela, return file-name"
  [file-name]
  (first (str/split file-name #"\.pamela")))

(defn create-artifacts
  "Create edn and json versions of HTN and TPN models for pamela"
  [pamela-file]
  (io/make-parents (str outdir "nothing.txt"))
  (let [[filename entrypoint] pamela-file
        fobj (io/as-file filename)
        pname-with-extension (.getName fobj)
        pname (get-pamela-name pname-with-extension)
        out-pname (str outdir pname)

        htn-edn (str out-pname htn-edn-suffix)
        htn-json (str out-pname htn-json-suffix)

        tpn-edn (str out-pname tpn-edn-suffix)
        tpn-json (str out-pname tpn-json-suffix)

        htn-edn-clj (str htn-edn ".clj")
        htn-json-clj (str htn-json ".clj")
        tpn-end-clj (str tpn-edn ".clj")
        tpn-json-clj (str tpn-json ".clj")
        ]
    (pcli/set-test-mode! true)
    (scli/set-test-mode! true)
    (println "Creating artifacts for " pname "from" pamela-file)

    (pcli/reset-gensym-generator)
    (println "Generating HTN and TPN in EDN")
    (pcli/pamela "-i" filename "-o" out-pname "-t" entrypoint "-f" "edn" "htn")
    ;(pcli/exec-action "htn" {:input [fobj] :output out-pname :root-task entrypoint :file-format "edn" :verbose 1})

    (pcli/reset-gensym-generator)
    (println "Generating HTN and TPN in JSON")
    (pcli/pamela "-i" filename "-o" out-pname "-t" entrypoint "-f" "json" "htn")
    ;(pcli/exec-action "htn" {:input [fobj] :output out-pname :root-task entrypoint :file-format "json" :verbose 1})

    ;(println "schema files" htn-edn htn-json tpn-edn tpn-json)

    (println "Reading htn-edn, writing htn-edn")
    (scli/plan-schema "-i" htn-edn "-o" htn-edn-clj "htn")

    (println "Reading htn-json, writing htn-edn")
    (scli/plan-schema "-i" htn-json "-o" htn-json-clj "htn")

    (println "Reading tpn-edn, writing tpn-edn")
    (scli/plan-schema "-i" tpn-edn "-o" tpn-end-clj "tpn")

    (println "Reading tpn-json, writing tpn-edn")
    (scli/plan-schema "-i" tpn-json "-o" tpn-json-clj "tpn")

    [htn-edn-clj htn-json-clj tpn-end-clj tpn-json-clj]
    ))

(defn read-clj [filename]
  (read-string (slurp filename)))

(defn compare-files [edn json]
  (= (read-clj edn) (read-clj json)))

(deftest testing-check-coercion
  (testing "HTN and TPN Json coercion"
    (doseq [file pamela-files]
      (let [[htn-edn-clj htn-json-clj tpn-edn-clj tpn-json-clj] (create-artifacts file)]


        (when-not (compare-files htn-edn-clj htn-json-clj)
          ; fake comparison to trigger unit test error
          (is (= htn-edn-clj htn-json-clj) (prn-str "files differ" htn-edn-clj htn-json-clj " Do diff for details")))

        (when-not (compare-files tpn-edn-clj tpn-json-clj)
          (is (= tpn-edn-clj tpn-json-clj) (prn-str "files differ" tpn-edn-clj tpn-json-clj " Do diff for details")))

        ))))
