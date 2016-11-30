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
  (:require ;; [clojure.java.io :refer :all] ;; for as-file
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.data.json :as json]
            [clojure.data.codec.base64 :as base64]
            [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            [me.raynes.fs :as fs]
            [pamela.mode :as mode]
            [pamela.utils :refer [get-input var-of repl? set-cwd!
                                  input-file output-file]]
            [pamela.db :as db]
            [pamela.daemon :as daemon]
            [pamela.parser :as parser]
            [pamela.htn :as htn]
            [pamela.tpn :as tpn]
            [clojure.tools.logging :as log]
            [pamela.log :as plog]
            [environ.core :refer [env]]))

;; actions ----------------------------------------------

(defn cat-input-output
  "Simply copies the input to output (for debugging)"
  {:added "0.2.0"}
  [options]
  (let [{:keys [output]} options
        [_ data] (get-input options)]
    (output-file output "edn" data)
    0))

(defn delete-model
  "Deletes given model from the database"
  {:added "0.2.0"}
  [options]
  (db/with-db options
    (db/delete-model options))
  0)

(defn describe-model
  "Analyze and describe the given model"
  {:added "0.2.0"}
  [options]
  (db/with-db options
    (db/describe-model options))
  0)

(defn export-model
  "Export model source from the database"
  {:added "0.2.0"}
  [options]
  (db/with-db options
    (db/export-model options))
  0)

(defn import-model
  "Import model into the database"
  {:added "0.2.0"}
  [options]
  (db/with-db options
    (db/import-model options))
  0)

(defn list-models
  "List models (--simple for names only, --load for memory only)"
  {:added "0.2.0"}
  [options]
  (let [msg "list-models not implemented yet"]
    (log/error msg)
    1))

(defn load-models
  "Load model(s) in memory only"
  {:added "0.2.0"}
  [options]
  (let [msg "load-models not implemented yet"]
    (log/error msg)
    1))

(defn build-model
  "Load model(s) in memory, construct --model PCLASS, save as EDN"
  {:added "0.3.0"}
  [options]
  (let [{:keys [input output]} options
        ir (parser/parse options)]
    (if (:error ir)
      (do
        (log/errorf "unable to parse: %s\nerror: %s" input (:error ir))
        1)
      (do
        (output-file output "edn" ir)
        0))))

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
      (let [tree (parser/parse (assoc options :check-only? true))]
        (if (:error tree)
          (do
            (log/errorf "unable to parse: %s\nerror: %s" input (:error tree))
            1)
          (do
            (output-file output "edn" (:tree tree))
            0))))))

(defn parse-model
  "Load model(s) in memory, construct --model PCLASS, save as EDN"
  {:added "0.3.0"}
  [options]
  (log/error "The parse action is deprecated")
  1)

(defn tpn
  "Load model(s) in memory only, process as a TPN"
  {:added "0.2.0"}
  [options]
  (let [{:keys [construct-tpn file-format input output visualize]} options
        stdout? (daemon/stdout? output)
        ir (parser/parse options)
        tpn (cond
              (:error ir)
              (do
                (log/errorf "unable to parse: %s\nerror: %s" input (:error ir))
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
  "Load model(s) in memory only, process as a HTN"
  {:added "0.4.0"}
  [options]
  (let [{:keys [root-task file-format input output]} options
        ir (parser/parse options)]
    (if (:error ir)
      (do
        (log/errorf "unable to parse: %s\nerror: %s" input (:error ir))
        1)
      (if-not (#{"edn" "json"} file-format)
        (do
          (log/errorf "illegal file-format for htn: %s" file-format)
          1)
        (htn/plan-htn ir root-task file-format output)))))

(def #^{:added "0.2.0"}
  actions
  "Valid PAMELA command line actions"
  {"build" (var build-model)
   "check" (var check-model)
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

(def log-levels #{"trace" "debug" "info" "warn" "error" "fatal" "report"})

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
    :parse-fn #(input-file %)
    :validate [#(or (daemon/running?) (= "-" %) (fs/exists? %))
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
   ["-L" "--load" "List models in memory only"]
   ["-o" "--output OUTPUT" "Output file (or - for STDOUT)"
    :default "-"]
   ["-a" "--magic MAGIC" "Magic lvar initializtions"
    :default nil
    :parse-fn #(input-file %)
    :validate [#(or (daemon/running?) (nil? %) (fs/exists? %))
               "MAGIC file does not exist"]]
   ["-b" "--output-magic OUTPUT-MAGIC" "Output magic file"
    :default nil]
   ["-m" "--model MODEL" "Model name"]
   ["-r" "--recursive" "Recursively process model"]
   ["-s" "--strict" "Enforce strict plan schema checking"]
   ["-S" "--simple" "Simple operation"]
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
  - -S, --simple            **Simple operation**
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
  (log/trace "EXIT(" status ")")
  (when msgs
    (if (zero? status)
      (println (string/join \newline msgs))
      (log/error \newline (string/join \newline msgs))))
  (flush) ;; ensure all pending output has been flushed
  (when-not (daemon/running?)
    (when (repl?)
      (throw (Exception. (str "DEV MODE exit(" status ")"))))
    (shutdown-agents)
    (System/exit status))
  true)

(defn base-64-decode [b64]
  ;; The following requires JDK 8
  ;; (String. (.decode (Base64/getDecoder) b64))
  (String. (base64/decode (.getBytes b64))))

(defn pamela
  "PAMELA command line processor. (see usage for help)."
  {:added "0.2.0"
   :version "0.4.2"}
  [& args]
  (when (and (:pamela-version env)
          (not= (:pamela-version env) (:version (meta #'pamela))))
    (exit 1 (str "ERROR: please update pamela meta data version to: "
              (:pamela-version env))))
  (set-cwd! (or (:pamela-cwd env) (:user-dir env)))
  (let [{:keys [options arguments errors summary]}
        (parse-opts args cli-options)
        {:keys [help version verbose construct-tpn daemonize database
                file-format input log-level load output model recursive root-task
                simple strict visualize web magic output-magic]} options
        log-level (keyword (or log-level "warn"))
        _ (plog/initialize log-level (apply pr-str args))
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
        (println "daemon?:" (daemon/running?))
        (println "database running?:" (db/running?))
        (println "version:" (:pamela-version env))
        (println "web:" web))
      (println "verbosity level:" verbose)
      (println "log level:" log-level)
      (println "construct-tpn:" construct-tpn)
      (println "daemonize:" daemonize)
      (println "database:" database)
      (println "file-format:" file-format)
      (println "input:" input)
      (println "load:" load)
      (println "output:" output)
      (println "magic:" magic)
      (println "output-magic:" output-magic)
      (println "model:" model)
      (println "recursive:" recursive)
      (println "root-task:" root-task)
      (println "simple:" simple)
      (println "strict:" strict)
      (println "visualize:" visualize)
      (println "cmd:" cmd (if action "(valid)" "(invalid)")))
    (if-not action
      (if-not exit?
        (exit 1 (str "Unknown action: \"" cmd "\". Must be one of " (keys actions)))
        (usage summary))
      (if (> verbose 1) ;; throw full exception with stack trace when -v -v
        (exit (action options))
        (try
          (exit (action options))
          (catch Throwable e
            (exit 1 "ERROR caught exception:" (.getMessage e))))))
    (exit 0)))
