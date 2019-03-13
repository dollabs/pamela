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

(ns pamela.utils
  "Utility helper functions."

  ;; (:refer-clojure :exclude [update]) ;; clj-http
  (:require [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.java.io :refer [as-url]]
            [me.raynes.fs :as fs]
            [clojure.pprint :as pp]
            [environ.core :refer [env]]
            [clojure.data.json :as json]
            [camel-snake-kebab.core :as translate]
            [avenir.utils :refer [str-append vec-index-of]]
            [clojure.tools.logging :as log]
            [plan-schema.sorting :refer [sort-map sort-mixed-map]]))

(defn stdout?
  "Returns true if the output file name is STDOUT (or running as pamelad)"
  {:added "0.2.0"}
  [output]
  (or (nil? output) (= output "-")))

(def cwd (atom fs/*cwd*))

(defn set-cwd! [new-cwd]
  (reset! cwd (if (fs/file? new-cwd)
                  new-cwd
                  (fs/file new-cwd))))

(defn get-cwd []
  @cwd)

(defn encode-ir-tokens-as-strings
  "Encode string/symbol/keyword distinction as strings that can re reversed simply after reading"
  [ir]
  (cond (string? ir) (str "\"" ir "\"")
        (keyword? ir) (str ir)
        (symbol? ir) ir
        ;; Recursive data structures
        (list? ir) (map (fn [sir] (encode-ir-tokens-as-strings sir)) ir)
        (vector? ir) (seq (map (fn [sir] (encode-ir-tokens-as-strings sir)) ir))
        (set? ir) (set (map (fn [sir] (encode-ir-tokens-as-strings sir)) ir))
        (map? ir) (into {} (map (fn [[key val]] {(encode-ir-tokens-as-strings key) (encode-ir-tokens-as-strings val)}) ir))
        :else ir))                                ; All other cases remain unchanged.

(defn unencode-ir-strings
  "Unencode string/symbol/keyword encoded as readable strings"
  [ir]
  (cond (string? ir) (read-string ir)
        ;; Recursive data structures
        (list? ir) (map (fn [sir] (unencode-ir-strings sir)) ir)
        (vector? ir) (seq (map (fn [sir] (unencode-ir-strings sir)) ir))
        (set? ir) (set (map (fn [sir] (unencode-ir-strings sir)) ir))
        (map? ir) (into {} (map (fn [[key val]] {(unencode-ir-strings key) (unencode-ir-strings val)}) ir))
        :else ir))                                ; All other cases remain unchanged.

;; file-format's supported: edn edn-mixed json string
;; if input is a map, sort it as a homogeneous key map
;;    unless the format is edn-mixed = sort as heterogeneous keymap
(defn output-file [filename file-format data]
  (binding [*print-length* nil] ;;Just in case someone has this set in their environment
    (let [is-stdout? (stdout? filename)
          filename (if (and (not is-stdout?) (string? filename))
                     (fs/expand-home filename)
                     filename)
          output-filename (if (not is-stdout?)
                            (if (fs/absolute? filename)
                              (fs/file filename)
                              (fs/file (get-cwd) filename)))
          data (if (= file-format "json-ir") (encode-ir-tokens-as-strings data) data) ; Only for JSON (IR) files
          data (if (map? data)
                 ;; (if (= file-format "edn-mixed")
                 ;;   (sort-mixed-map data)
                 ;;   (sort-map data))
                 (sort-mixed-map data)
                 data)
          out (cond
                (#{"edn" "edn-mixed"} file-format)
                (with-out-str (pprint data))
                (or (= file-format "json")
                    (= file-format "json-ir"))
                (with-out-str (json/pprint data))
                :else
                data)]
      (if is-stdout?
        (println out)
        (spit output-filename out)))))

(defn input-file [filename]
  (let [is-stdin? (stdout? filename)
        filename (if (and (not is-stdin?) (string? filename))
                   (fs/expand-home filename)
                   filename)
        input-filename (if is-stdin?
                         filename
                         (if (fs/absolute? filename)
                           (fs/file filename)
                           (fs/file (get-cwd) filename)))]
    input-filename))

;; remove Clojure style comments
(defn remove-clojure-comments
  "Remove Clojure comments from s"
  {:added "0.2.2"}
  [s]
  (string/join "\n"
    (remove empty?
      (map #(string/replace % #";.*$" "")
        (string/split-lines s)))))

(defn repl?
  "Helper function returns true if on the REPL (for development)"
  {:added "0.2.0"}
  []
  (= (:pager env) "cat"))

(defn var-of
  "Returns the var corresponding the value v (if defined in ns)."
  {:added "0.2.0"}
  [v ns]
  (first (filter #(identical? v (deref %))
           (vals (ns-publics ns)))))

(defn sleep
  "sleeps for the given number of seconds (may be fractional)"
  {:pamela :utils :added "0.2.0"}
  [s]
  (Thread/sleep (* 1000 s)))

(defn user-tmpdir [program]
  (let [tmp (:java-io-tmpdir env)
        user (:user env)
        tmpdir (str tmp "/" user "/" program)]
    (if (fs/exists? tmpdir)
      tmpdir
      (do
        (fs/mkdirs tmpdir)
        tmpdir))))

(defn display-name-string
  "Generates a pretty-name for display to the user.  Currently, this is Title Case"
  [raw-name]
  (clojure.string/replace
   (translate/->Camel_Snake_Case_String raw-name)
   "_" " "))

;;Supported modes are:
;; :log - use the logging utility (e.g., log/warn)
;; :println - Just use println if level
(def ^{:doc
       "Specifies which mode of debugging output will be used, either :log or :println"}
  dbg-println-mode :println)

(def ^{:doc
       "The highest level, where higher = lower severity, at which println will be called
 when in :println mode"}
  dbg-println-level
  (atom 3))

;;For internal use only.  The order matters.
(def dbg-println-levels [:fatal :error :warn :info :debug :trace])

(defn set-dbg-println-level
  "Set the level for dbg-println. The highest level, where higher =
  lower severity (max is 6), at which println will be called when
  in :println mode"
  [new-level]
  (let [new-level (if (keyword? new-level)
                    (inc (or (vec-index-of dbg-println-levels new-level) 2))
                    new-level)
        new-level (if (and (integer? new-level)
                        (>= new-level 1)
                        (<= new-level 6))
                    new-level
                    3)]
    (log/info "setting dbg-println-level to" new-level)
    (reset! dbg-println-level new-level)))

(defn get-dbg-println-level
  "Get the level for dbg-println. The highest level, where higher =
  lower severity (max is 6), at which println will be called when
  in :println mode"
  []
  @dbg-println-level)

(defmacro dbg-println "Configurable alternative to using println for debugging."
    [level & more]
  (case dbg-println-mode
    :log (let [f (case level
                   (1 :fatal) 'log/fatal
                   (2 :error) 'log/error
                   (3 :warn) 'log/warn
                   (4 :info) 'log/info
                   (5 :debug) 'log/debug
                   (6 :trace) 'log/trace)]
           `(~f ~@more))
    :println (let [level (if (integer? level)
                           level
                           (inc (.indexOf dbg-println-levels level)))]
               `(if (<= ~level @dbg-println-level)
                  (println ~@more)))))

;; Placeholder, until we move to Clojure 1.9
(defn clj19-boolean?
  "Return true if x is a Boolean"
  [x] (instance? Boolean x))

(defn default-bounds?
  "Return true of bounds are [0 :infinity]"
  {:added "0.2.0"}
  [bounds]
  (and (vector? bounds)
       (or (zero? (count bounds))
           (and
             (= 2 (count bounds))
             (= 0 (first bounds))
             (= :infinity (second bounds))))))

(defn remove-default-bounds [bounds]
  (if (default-bounds? bounds)
    nil
    bounds))

;; returns the narrowest bounds
;; if a is an lvar {:type :lvar ...} then use a,
;; else if b is an lvar then use b
;; else expect both are bounds vectors
(defn merge-bounds [a b]
  (cond
    (and (map? a) (= (:type a) :lvar))
    a
    (and (map? b) (= (:type b) :lvar))
    b
    (map? a)
    (remove-default-bounds b)
    (map? b)
    (remove-default-bounds a)
    :else
    (let [[a-lb a-ub] (remove-default-bounds a)
          [b-lb b-ub] (remove-default-bounds b)
          lb (if a-lb
               (if b-lb
                 (min a-lb b-lb)
                 a-lb)
               (if b-lb
                 b-lb
                 0))
          ub (if a-ub
               (if b-ub
                 (if (or (= a-ub :infinity) (= b-ub :infinity))
                   :infinity
                   (max a-ub b-ub))
                 a-ub)
               (if b-ub
                 b-ub
                 :infinity))
          rv (if (and (= lb 0) (= ub :infinity))
               nil ;; do NOT explicitly return default bounds
               [lb ub])]
      rv)))
