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
            [plan-schema.utils :as putils]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer :all]
            [clojure.java.shell :as sh])
  (:import [java.io.File]))

;; NOTE ["test/pamela/cannon.pamela" "(game.main)"]
;; is NOT valid for generation via the "htn" method b/c
;; it contains more than one high level construct (parallel, sequence, etc.)
;; in a method (and it has the unsupported 'whenever' function)
;; NOTE: ["test/pamela/ir-test.pamela" "(game.main-test)"]
;; excluded as above
;; NOTE: ["test/pamela/tpn-between.pamela" "(tpn.elephant)"]
;; excluded  as above
(def pamela-files [["test/pamela/regression/isr-htn.pamela"
                    "(isr-htn-demo.htn.main \"A\" \"B\" \"C\" \"D\")"]])

(def outdir "target/gen-files/")
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
  (let [[filename root-task] pamela-file
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
        tpn-edn-clj (str tpn-edn ".clj")
        tpn-json-clj (str tpn-json ".clj")
        ]
    (pcli/set-test-mode! true)
    (scli/set-test-mode! true)
    (println "Creating artifacts for " pname "from" pamela-file)

    (pcli/reset-gensym-generator)
    (println "Generating HTN and TPN in EDN")
    (is (= 0 (pcli/pamela "-i" filename "-o" out-pname "-t" root-task "-f" "edn" "htn")))
    ;;(pcli/exec-action "htn" {:input [fobj] :output out-pname :root-task root-task :file-format "edn" :verbose 1})

    (pcli/reset-gensym-generator)
    (println "Generating HTN and TPN in JSON")
    (is (= 0 (pcli/pamela "-i" filename "-o" out-pname "-t" root-task "-f" "json" "htn")))
    ;;(pcli/exec-action "htn" {:input [fobj] :output out-pname :root-task root-task :file-format "json" :verbose 1})

    ;;(println "schema files" htn-edn htn-json tpn-edn tpn-json)

    (println "Reading htn-edn, writing htn-edn")
    (is (= 0 (scli/plan-schema "-i" htn-edn "-o" htn-edn-clj "htn")))

    (println "Reading htn-json, writing htn-edn")
    (is (= 0 (scli/plan-schema "-i" htn-json "-o" htn-json-clj "htn")))

    (println "Reading tpn-edn, writing tpn-edn")
    (is (= 0 (scli/plan-schema "-i" tpn-edn "-o" tpn-edn-clj "tpn")))

    (println "Reading tpn-json, writing tpn-edn")
    (is (= 0 (scli/plan-schema "-i" tpn-json "-o" tpn-json-clj "tpn")))

    [htn-edn-clj htn-json-clj tpn-edn-clj tpn-json-clj]
    ))

(defn read-clj [filename]
  (read-string (slurp filename)))

#_(defn json->edn
    "Perform basic HTN/TPN data conversion"
    {:added "0.6.0"}
    ([m]
     (cond
       (map? m) (reduce-kv json->edn {} m)
       :else m))
    ([m k v]
     (assoc m k
              (let [v2
                    (cond                                   ;; type coercion of v based on k
                      (map? v) (reduce-kv json->edn {} v)
                      ;; k that wants v to be a keyword
                      (#{:type :tpn-type :network :network-id :uid :htn-node :end-node} k)
                      (keyword v)
                      ;; k that wants v to be a set of keywords
                      (#{:incidence-set :rootnodes :constraints :activities} k)
                      (set (map keyword v))
                      ;; k that wants v to be a vector of keywords
                      (#{:edges} k)
                      (mapv keyword v)
                      :else v)]
                ;; DEBUGGING
                ;; (if (not= v2 v)
                ;;   (println "k:" k "v:" v " -> " v2))
                v2))))

; The output produced by plan-schema (edn and json) is being compared to find coercion issues.
; Note: plan-schema reads edn and json (produced by pamela) and spits out the version of the same after coercion.
; If the coercion is working correctly, output produced by plan-schema should be identical for both.
; If there coercion issues, the output will be different and that's exactly what we are looking for.

; Ex:
; This is output produced by plan-schema after coercion for edn file.
;<  {:uid :hem-100,
;<   :type :htn-expanded-method,
;---
; This is output produced by plan-schema after coercion for json file.
;>  {:uid "hem-100",
;    >   :type "htn-expanded-method",
(defn compare-files [edn json]
  (= (read-clj edn) (read-clj json))
  ; This is a bad test because it is actually performing the same coercion that plan-schema should be doing.
  #_(let [edn-sorted (into (sorted-map) (keywordize (read-clj edn)))
          json-sorted (into (sorted-map) (json->edn (read-clj json)))
          equal? (= edn-sorted json-sorted)]
      ;; DEBUGGING
      ;; (when-not equal?
      ;;   (spit (str edn ".sorted.edn") (with-out-str (pprint edn-sorted)))
      ;;   (spit (str json ".sorted.edn") (with-out-str (pprint json-sorted))))
      equal?))

(deftest testing-check-coercion
  (testing "HTN and TPN Json coercion"
    (doseq [file pamela-files]
      (let [[htn-edn-clj htn-json-clj tpn-edn-clj tpn-json-clj]
            (create-artifacts file)]

        (when-not (compare-files htn-edn-clj htn-json-clj)
          ;; fake comparison to trigger unit test error
          (is (= htn-edn-clj htn-json-clj) (prn-str "files differ" htn-edn-clj htn-json-clj " See diff for details"))
          (println (:out (sh/sh "diff" "-u" htn-edn-clj htn-json-clj))))

        (when-not (compare-files tpn-edn-clj tpn-json-clj)
          (is (= tpn-edn-clj tpn-json-clj) (prn-str "files differ" tpn-edn-clj tpn-json-clj " See diff for details"))
          (println (:out (sh/sh "diff" "-u" tpn-edn-clj tpn-json-clj))))

        ))))

(defn check-shell []
  (sh/sh "ls"))
