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

(ns pamela.cli
  "PAMELA command line interface."
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.data.json :as json]
            [clojure.data.codec.base64 :as base64]
            [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            [me.raynes.fs :as fs]
            [pamela.utils :refer [repl? set-cwd!
                                  set-dbg-println-level
                                  input-file output-file]]
            [plan-schema.utils :refer [fs-get-path]]
            [pamela.parser :as parser]
            [pamela.unparser :as unparser]
            [pamela.htn :as htn]
            [pamela.tpn :as tpn]
            [clojure.tools.logging :as log]
            [pamela.log :as plog]
            [environ.core :refer [env]])
  (:gen-class)) ;; required for uberjar

(defonce test-mode false)
(defn set-test-mode! [value]
  (def test-mode value))

;; actions ----------------------------------------------

(defn check-model
  "Perform syntatic check on a PAMELA model"
  {:added "0.4.4"}
  [options]
  (let [{:keys [input output magic output-magic]} options]
    (cond
      (not= 1 (count input))
      (do
        (log/error "check action only accepts one input file")
        1)
      magic
      (do
        (log/error "check action does not accept magic input")
        1)
      output-magic
      (do
        (log/error "check action does not accept magic output")
        1)
      :else
      (let [tree (parser/parse-pamela-file (first input))]
        (if (:error tree)
          (do
            (log/errorf "unable to parse: %s\nerror: %s" input (:error tree))
            1)
          (do
            (output-file output "edn" tree)
            0))))))

(defn build-model
  "Load model(s) and build intermediate representation (IR)"
  {:added "0.3.0"}
  [options]
  (let [{:keys [input output source json-ir]} options
        ir (parser/parse options)
        file-format (if-not json-ir "edn-mixed" "json-ir")] ; +++ undo this when we have a combined JSON format for Pamela
    (if (:error ir)
      (do
        (log/errorf "unable to parse: %s\nerror: %s" input (with-out-str (pprint (:error ir))))
        1)
      (do
        (output-file output file-format ir)
        (when source
          (output-file source "raw" (unparser/unparse ir)))
        0))))

(defn unparse-model
  "Convert intermediate representation (IR) back into Pamela source"
  {:added "0.6.2"}
  [options]
  (let [{:keys [input output source]} options
        ir (read-string (slurp (fs-get-path (first input))))]
    ;; assumes there is exactly one input file
    (if source
      (do
        (log/errorf "specifiy --output instead of --source for the unparse action")
        1)
      (do
        (output-file output "raw" (unparser/unparse ir))
        0))))

(defn parse-model
  "Load model(s) in memory, construct --model PCLASS, save as EDN"
  {:added "0.3.0"}
  [options]
  (log/error "The parse action is deprecated")
  1)

(defn tpn
  "Load model(s) and construct as a TPN"
  {:added "0.2.0"}
  [options]
  (let [{:keys [construct-tpn file-format input output visualize]} options
        ir (parser/parse options)
        tpn (cond
              (:error ir)
              (do
                (log/errorf "unable to parse: %s\nerror: %s" input (with-out-str (pprint (:error ir))))
                ir)
              :else
              (tpn/load-tpn ir options))]
    (if (:error tpn)
      (do
        (if-not (:error ir)
          (log/errorf "unable to create TPN: %s\nerror: %s" input (:error tpn)))
        1)
      (do
        ;; (println "TPN is valid")
        (if visualize
          ;; (tpn/visualize tpn options)
          (do
            (log/error "visualize not implemented yet")
            1)
          0)))))

(defn htn
  "Load model(s) and construct HTN and TPN"
  {:added "0.4.0"}
  [options]
  (let [{:keys [root-task file-format input output]} options
        ir (parser/parse options)]
    (if (:error ir)
      (do
        (log/errorf "unable to parse: %s\nerror: %s" input (with-out-str (pprint (:error ir))))
        1)
      (if-not (#{"edn" "json" "json-ir"} file-format) ;+++ remove json-ir when we have a single integrated json format
        (do
          (log/errorf "illegal file-format for htn: %s" file-format)
          1)
        (htn/plan-htn ir root-task file-format output)))))

(def #^{:added "0.2.0"}
  actions
  "Valid PAMELA command line actions"
  {"check" (var check-model)
   "build" (var build-model)
   "unparse" (var unparse-model)
   "tpn" (var tpn)
   "htn" (var htn)})

(def log-levels #{"trace" "debug" "info" "warn" "error" "fatal" "report"})

(def #^{:added "0.2.0"}
  output-formats
  "Valid PAMELA output file formats"
  #{"edn" "json"})

;; command line processing -----------------------------------

(def #^{:added "0.2.0"}
  cli-options
  "Command line options"
  [["-V" "--version" "Print PAMELA version"]
   ["-a" "--magic MAGIC" "Magic lvar initializtions"
    :default nil
    :parse-fn #(input-file %)
    :validate [#(or (nil? %) (fs/exists? %))
               "MAGIC file does not exist"]]
   ["-b" "--output-magic OUTPUT-MAGIC" "Output magic file"
    :default nil]
   ["-c" "--construct-tpn CFM " "Construct TPN using class C field F method M (as C:F:M)"]
   ["-f" "--file-format FORMAT" "Output file format [json]"
    :default "json"
    :validate [#(contains? output-formats %)
               (str "FORMAT not supported, must be one of "
                 (vec output-formats))]]
   [nil "--json-ir" "Generate IR in json form" :default false]
   ["-h" "--help" "Print usage"]
   ["-i" "--input INPUT" "Input file(s) (or - for STDIN)"
    :default ["-"]
    :parse-fn #(input-file %)
    :validate [#(or (= "-" %) (fs/exists? %))
               "INPUT file does not exist"]
    :assoc-fn (fn [m k v]
                (let [oldv (get m k [])
                      oldv (if (= oldv ["-"]) [] oldv)]
                  (assoc m k (conj oldv v))))]
   ["-l" "--log-level LEVEL" "Logging level"
    :default "warn"
    :parse-fn (fn [level]
                (if (log-levels level)
                  level
                  (throw (Exception. (str "\nlog-level must be one of: "
                                       log-levels)))))]
   ["-d" "--dbg-level LEVEL" "Debugging level"
    :default "warn"
    :parse-fn (fn [level]
                (if (log-levels level)
                  level
                  (throw (Exception. (str "\ndbg-level must be one of: "
                                       log-levels)))))]
   ["-o" "--output OUTPUT" "Output file (or - for STDOUT)"
    :default "-"]
   ["-s" "--source PAMELA" "source as reconstructed from IR"]
   ["-t" "--root-task ROOTTASK" "Label for HTN root-task [main]"]
   ["-v" "--verbose" "Increase verbosity"
    :default 0
    :assoc-fn (fn [m k _] (update-in m [k] inc))]
   ])

(defn usage
  "Print PAMELA command line help."
  {:added "0.2.0" :doc/format :markdown}
  [options-summary]
  (->> (for [a (sort (keys actions))]
         (str "  " a "\t" (:doc (meta (get actions a)))))
    (concat [""
             "Probabalistic Advanced Modeling and Execution Learning Architecture (PAMELA)"
             ""
             "Usage: pamela [options] action"
             ""
             "Options:"
             options-summary
             ""
             "Actions:"])
    (string/join \newline)))

(defn exit
  "Exit PAMELA with given status code (and optional messages)."
  {:added "0.2.0"}
  [status & msgs]
  (log/trace "EXIT(" status ")")
  (when msgs
    (if (zero? status)
      (println (string/join \newline msgs))
      (log/error \newline (string/join \newline msgs))))
  (flush) ;; ensure all pending output has been flushed
  (if (or (repl?) test-mode)
    (log/info "exit" status "pamela in DEV MODE. Not exiting" "repl?" (repl?) "test-mode" test-mode)
    (do (log/info "exiting with status" status)
      (shutdown-agents)
        (System/exit status)))
  status)

(defn base-64-decode [b64]
  ;; The following requires JDK 8
  ;; (String. (.decode (Base64/getDecoder) b64))
  (String. (base64/decode (.getBytes b64))))

(defn exec-action
  "Performs no checks whatsoever !!"
  [action options]
  ((get actions action) options))

(defn pamela
  "PAMELA command line processor. (see usage for help)."
  {:added "0.2.0"
   :version "0.6.3-SNAPSHOT"}
  [& args]
  (when (and (:pamela-version env)
          (not= (:pamela-version env) (:version (meta #'pamela))))
    (exit 1 (str "ERROR: please update pamela meta data version to: "
              (:pamela-version env))))
  (set-cwd! (or (:pamela-cwd env) (:user-dir env)))
  (let [{:keys [options arguments errors summary]}
        (parse-opts args cli-options)
        {:keys [help version verbose construct-tpn
                file-format input log-level dbg-level output root-task
                source magic output-magic]} options
        log-level (keyword (or log-level "warn"))
        _ (plog/initialize log-level (apply pr-str args))
        dbg-level (keyword (or dbg-level "warn"))
        _ (set-dbg-println-level dbg-level)
        cmd (first arguments)
        action (get actions cmd)
        root-task (if (and root-task
                        (string? root-task)
                        (pos? (count root-task)))
                    (if (string/starts-with? root-task "(")
                      root-task
                      (base-64-decode root-task)))
        options (assoc options :root-task root-task :log-level log-level)
        verbose? (pos? (or verbose 0))
        exit?
        (cond
          help
          (exit 0 (usage summary))
          errors
          (exit 1 (string/join \newline errors) (usage summary))
          version
          (exit 0 (:version (meta #'pamela)))
          (not= (count arguments) 1)
          (exit 1 "Specify exactly one action" (usage summary)))]
    (when (and verbose? (not exit?))
      (when (> verbose 1)
        (println "repl?:" (repl?))
        (println "version:" (:pamela-version env)))
      (println "verbosity level:" verbose)
      (println "log level:" log-level)
      (println "debug level:" dbg-level)
      (println "construct-tpn:" construct-tpn)
      (println "file-format:" file-format)
      (println "input:" input)
      (println "output:" output)
      (println "source:" source)
      (println "magic:" magic)
      (println "output-magic:" output-magic)
      (println "root-task:" root-task)
      (println "cmd:" cmd (if action "(valid)" "(invalid)")))
    (if (or exit? (not action))
      (if-not action
        (exit 1 (str "Unknown action: \"" cmd "\". Must be one of " (keys actions)))
        1 ;; just return exit code instead of repeating (usage summary)
        )
      (if (> verbose 1) ;; throw full exception with stack trace when -v -v
        (exit (action options))
        (try
          (exit (action options))
          (catch Throwable e
            (exit 1 "ERROR caught exception:" (.getMessage e))))))))

(defn reset-gensym-generator
  "Resets gensym generators in htn and tpn.
   Helps with repeatable testing"
  []
  (htn/reset-my-count) (tpn/reset-my-count))

(defn -main
  "PAMELA main entry point (see pamela)"
  {:added "0.2.0"}
  [& args]
  (apply pamela args))
