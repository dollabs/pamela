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
            [avenir.utils :refer [str-append]]
            [clojure.tools.logging :as log])
  (:import [java.net
            URL]))

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

;; file-format's supported: edn json string
;; if input is a map, sort it
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
          data (if (map? data)
                 (into (sorted-map) data)
                 data)
          out (cond
                (= file-format "edn")
                (with-out-str (pprint data))
                (= file-format "json")
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
