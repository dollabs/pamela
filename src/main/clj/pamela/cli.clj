;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;;; Acknowledgement and Disclaimer:
;;; This material is based upon work supported by the Army Contracting
;;; and DARPA under contract No. W911NF-15-C-0005.
;;; Any opinions, findings and conclusions or recommendations expressed
;;; in this material are those of the author(s) and do necessarily reflect the
;;; views of the Army Contracting Command and DARPA.

(ns pamela.cli
  "PAMELA command line interface."
  (:require [clojure.java.io :refer :all] ;; for as-file
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.data.json :as json]
            [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            [pamela.mode :as mode]
            [pamela.utils :refer [get-input var-of repl?]]
            [pamela.db :as db]
            [pamela.daemon :as daemon]
            ;; [pamela.pclass :as pclass :refer [get-model-var]]
            ;; [pamela.models :as models]
            [pamela.parser :as parser]
            [pamela.htn :as htn]
            [pamela.tpn :as tpn]
            [clojure.tools.logging :as log]
            [pamela.log :as plog]
            [environ.core :refer [env]])
  (:import [java.security
            PrivilegedActionException]))

;; actions ----------------------------------------------

(defn cat-input-output
  "Simply copies the input to output (for debugging)"
  {:added "0.2.0"}
  [options]
  (let [[_ data] (get-input options)
        output (:output options)]
    (if (daemon/stdout? output)
      (print data)
      (spit output data))))

(defn delete-model
  "Deletes given model from the database"
  {:added "0.2.0"}
  [options]
  (db/with-db options
    (db/delete-model options)))

(defn describe-model
  "Analyze and describe the given model"
  {:added "0.2.0"}
  [options]
  (db/with-db options
    (db/describe-model options)))

(defn export-model
  "Export model source from the database"
  {:added "0.2.0"}
  [options]
  (db/with-db options
    (db/export-model options)))

(defn import-model
  "Import model into the database"
  {:added "0.2.0"}
  [options]
  (db/with-db options
    (db/import-model options)))

(defn list-models
  "List models (--simple for names only, --load for memory only)"
  {:added "0.2.0"}
  [options]
  (let [msg "list-models not implemented yet"]
    (println msg)
    (log/error msg)
    false))
  ;; (if (:load options) ;; memory only
  ;;   (println (pclass/list-models false))
  ;;   (db/with-db options
  ;;     (let [models (db/list-models options)]
  ;;       (if (:simple options)
  ;;         (doseq [m models] (println m))
  ;;         (pprint models))))))

(defn load-models
  "Load model(s) in memory only"
  {:added "0.2.0"}
  [options]
  (let [msg "load-models not implemented yet"]
    (println msg)
    (log/error msg)
    false))
  ;; (binding [pclass/*pclasses* (atom [])]
  ;;   (let [{:keys [verbose]} options
  ;;         [url source] (get-input options)
  ;;         load-result (models/load-pamela-string source url)]
  ;;         (if-not (empty? load-result)
  ;;           (do
  ;;             (log/errorf "unable to load: %s" load-result)
  ;;             false)
  ;;           (do
  ;;             (if (> verbose 1)
  ;;               (println "loaded classes:" @pclass/*pclasses*))
  ;;             @pclass/*pclasses*)))))

(defn build-model
  "Load model(s) in memory, construct --model PCLASS, save as EDN"
  {:added "0.3.0"}
  [options]
  (let [{:keys [input output]} options
        ir (parser/parse options)]
    (cond
      (not ir)
      (do
        (println "unable to parse: %s" input)
        (log/errorf "unable to parse: %s" input)
        false)
      :else
      (if (daemon/stdout? output)
        (pprint ir)
        (spit output (with-out-str (pprint ir)))))))

(defn parse-model
  "Load model(s) in memory, construct --model PCLASS, save as EDN"
  {:added "0.3.0"}
  [options]
  (let [{:keys [cwd input output]} options
        filename (if (= 1 (count input)) (first input))
        pir (if filename (parser/parse options))]
    (if pir
      (if (daemon/stdout? output)
        (print pir)
        (spit output pir))
      (if filename
        (do
          (log/errorf "unable to parse: %s" filename)
          false)
        (do
          (log/errorf "invalid input to parse: %s" input)
          false)))))

(defn tpn
  "Load model(s) in memory only, process as a TPN"
  {:added "0.2.0"}
  [options]
  (let [{:keys [construct-tpn file-format cwd input output visualize]} options
        ;; loaded (load-models options)
        filename (if (= 1 (count input))
                   (first input)
                   (log/error "multiple TPN input files not implemented yet")
                   )
        loaded (if filename (parser/parse options))
        tpn (if loaded
              (if construct-tpn
                ;; FIXME (tpn/construct-tpn-cfm construct-tpn file-format)
                (log/error "construct-tpn not implemented yet")
                (tpn/load-tpn loaded file-format)))]
    (when (and loaded tpn)
      (if (daemon/stdout? output)
        (print tpn)
        (spit output tpn))
      (when visualize
        ;; (tpn/visualize tpn options)
        (log/error "visualize not implemented yet")
        ))))

(defn htn
  "Load model(s) in memory only, process as a HTN"
  {:added "0.4.0"}
  [options]
  (let [{:keys [root-task file-format cwd input output]} options
        ir (parser/parse options)]
    (if-not ir
      (do
        (println "unable to parse: %s" input)
        (log/errorf "unable to parse: %s" input)
        false)
      ;; root-task may be nil for the default case
      (let [htn (htn/plan-htn ir root-task)
            out (if (= file-format "json")
                  (with-out-str (json/pprint htn))
                  (with-out-str (pprint htn)))]
        (if (daemon/stdout? output)
          (println out)
          (spit output out))))))

(def #^{:added "0.2.0"}
  actions
  "Valid PAMELA command line actions"
  {"build" (var build-model)
   "parse" (var parse-model)
   "cat" (var cat-input-output)
   "delete" (var delete-model)
   "describe" (var describe-model)
   "export" (var export-model)
   "import" (var import-model)
   "list" (var list-models)
   "load" (var load-models)
   "tpn" (var tpn)
   "htn" (var htn)})

(def #^{:added "0.2.0"}
  output-formats
  "Valid PAMELA output file formats"
  #{"edn" "cytoscape" "dot" "json"})

;; command line processing -----------------------------------

(def #^{:added "0.2.0"}
  cli-options
  "Command line options"
  [["-h" "--help" "Print usage"]
   ["-V" "--version" "Print PAMELA version"]
   ["-v" "--verbose" "Increase verbosity"
    :default 0
    :assoc-fn (fn [m k _] (update-in m [k] inc))]
   ["-c" "--construct-tpn CFM " "Construct TPN using class C field F method M (as C:F:M)"]
   ["-d" "--daemonize PORT" "Run PAMELA as a daemon on the given PORT"
    :default 0
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 80 % 0x10000) "Must be a number between 80 and 65536"]]
   ["-e" "--database DATABASE" "Remote database server name (ES_SERVER)"
    :default (:es-server env)]
   ["-f" "--file-format FORMAT" "Output file format [edn]"
    :default "edn"
    :validate [#(contains? output-formats %)
               (str "FORMAT not supported, must be one of "
                 (vec output-formats))]]
   ["-i" "--input INPUT" "Input file (or - for STDIN)"
    :default ["-"]
    :validate [#(or (daemon/running?) (= "-" %) (.exists (as-file %)))
               "INPUT file does not exist"]
    :assoc-fn (fn [m k v]
                (let [oldv (get m k [])
                      oldv (if (= oldv ["-"]) [] oldv)]
                  (assoc m k (conj oldv v))))]
   ["-l" "--load" "List models in memory only"]
   ["-o" "--output OUTPUT" "Output file (or - for STDOUT)"
    :default "-"]
   ["-m" "--model MODEL" "Model name"]
   ["-r" "--recursive" "Recursively process model"]
   ["-s" "--simple" "Simple operation"]
   ["-t" "--root-task ROOTTASK" "Label for HTN root-task [main]"]
   ["-g" "--visualize" "Render TPN as SVG (set -f dot)"]
   ["-w" "--web WEB" "Web request hints (internal use only)"]
   ;; NOT Supported: all imports will currently update
   ;; ["-u" "--update" "Update a model (if it already exists)"]
   ])

(defn usage
  "Print PAMELA command line help.

  **Probabalistic Advanced Modeling and Execution Learning Architecture (PAMELA)**

  Usage: **pamela** *[options]* action

  **Options:**

  - -h, --help              **Print usage**
  - -V, --version           **Print PAMELA version**
  - -v, --verbose           **Increase verbosity**
  - -c  --construct-tpn CFM **Construct TPN using class C field F method M (as C:F:M)**
  - -d, --daemonize *PORT*  **Run PAMELA as a daemon on the given PORT**
  - -e, --database *DATABASE* **Remote database server name (ES_SERVER)**
  - -f, --file-format *FORMAT*   **Output file format (one of dot, json, cytoscape)**
  - -i, --input *INPUT*     **Input file(s) (or - for STDIN)**
  - -l, --load              **List models in memory only**
  - -o, --output *OUTPUT*   **Output file (or - for STDOUT)**
  - -m, --model *MODEL*     **Model name**
  - -r, --recursive         **Recursively process model**
  - -s, --simple            **Simple operation**
  - -g, --visualize         **Render TPN as SVG (set -f dot)**
  - -w, --web *WEB*         **Web request hints (_internal use only_)**

  **Actions:**

  - **cat**	Simply copies the input to output (for debugging)
  - **delete**	Deletes given model from the database
  - **describe**	Analyze and describe the given model
  - **export**	Export model source from the database
  - **import**	Import model into the database
  - **list**	List models (--simple for names only, --load for memory only)
  - **load**	Load model(s) in memory only
  - **tpn**	Load model(s) in memory only, process as a TPN
  "
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
  (if msgs (println (string/join \newline msgs)))
  (flush) ;; ensure all pending output has been flushed
  (when-not (daemon/running?)
    (when (repl?)
      (throw (Exception. (str "DEV MODE exit(" status ")"))))
    (shutdown-agents)
    (System/exit status))
  true)

(defn pamela
  "PAMELA command line processor. (see usage for help)."
  {:added "0.2.0"
   :version "0.4.0"}
  [& args]
  (when (and (:pamela-version env)
          (not= (:pamela-version env) (:version (meta #'pamela))))
    (exit 1 (str "ERROR: please update pamela meta data version to: "
              (:pamela-version env))))
  (plog/initialize)
  (log/info (str "args: " (pr-str args)))
  (let [{:keys [options arguments errors summary]}
        (parse-opts args cli-options)
        cmd (first arguments)
        action (get actions cmd)
        {:keys [help version verbose construct-tpn daemonize database file-format input load output model recursive root-task simple visualize web]} options
        cwd (or (:pamela-cwd env) (:user-dir env))
        output (if-not (daemon/stdout-option? output)
                 (if (.startsWith output "/")
                   output ;; absolute
                   (str cwd "/" output)))
        options (assoc options :output output)
        verbose? (pos? (or verbose 0))
        exit?
        (cond
          (pos? daemonize)
          (or (daemon/server-start options) true)
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
        (println "mode:" @mode/program-mode)
        (println "repl?:" (repl?))
        (println "cwd:" cwd)
        (println "daemon?:" (daemon/running?))
        (println "database running?:" (db/running?))
        (println "version:" (:pamela-version env))
        (println "web:" web))
      (println "verbosity level:" verbose)
      (println "construct-tpn:" construct-tpn)
      (println "daemonize:" daemonize)
      (println "database:" database)
      (println "file-format:" file-format)
      (println "input:" input)
      (println "load:" load)
      (println "output:" output)
      (println "model:" model)
      (println "recursive:" recursive)
      (println "root-task:" root-task)
      (println "simple:" simple)
      (println "visualize:" visualize)
      ;; (println "update:" (:update options))
      (println "cmd:" cmd (if action "(valid)" "(invalid)")))
    (if-not action
      (if-not exit?
        (exit 1 (str "Unknown action: \"" cmd "\". Must be one of " (keys actions)))
        (usage summary))
      (try
        (action (assoc options :cwd cwd))
        ;; DEBUG
        ;; (catch Throwable e ;; note AssertionError not derived from Exception
          ;; NOTE: this alternate exception is to help generate a stack trace
          ;; perhaps it's better to generate a stack trace in every case
          ;; HOWEVER pamelad *must* trap all exceptions at the top level
        (catch PrivilegedActionException e
          ;; FIXME: use proper logging
          (binding [*out* *err*]
            (println "ERROR caught exception:" (.getMessage e)))
          (exit 1))))
    (exit 0)))
