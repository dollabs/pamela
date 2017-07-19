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
            [clojure.tools.logging :as log]
            [plan-schema.utils :refer [sort-map]])
  (:import [java.lang ;; required by sort-map2
            Number]
           [java.util
            Comparator]
           [java.io
            Serializable]
           [clojure.lang
            Numbers]))

;; implementation of sort-map2 -- to move to plan-schema.utils -------------

(defn compare-by-class
  "Compare two objects. If they are of the same class use the class
   specific Comparator else compare on the class name"
  [o1 o2]
  (if (nil? o1)
    (if (nil? o2)
      0   ;; nil == nil
      -1) ;; nil < non-nil
    (if (nil? o2)
      1   ;; non-nil > nil
      (let [c1 (class o1)
            c2 (class o2)]
        (if (= c1 c2) ;; same class?
          (if (number? o1)
            (Numbers/compare o1 o2) ;; numeric comparison
            (.compareTo o1 o2))     ;; non-numeric comparison
          (.compareTo (.getName c1) (.getName c2))))))) ;; compare on class name

(declare default-comparator-by-class)

;; This is a special "interface' of Serializable
;; http://docs.oracle.com/javase/8/docs/api/java/io/Serializable.html
(definterface ReadResolve
  (readResolve []))

;; implements Comparator, Serializable
(deftype comparator-by-class []
  Comparator
  (compare [_ o1 o2]
    (compare-by-class o1 o2))
  ReadResolve ;; Serializable
  (readResolve [_]
    default-comparator-by-class))

(def default-comparator-by-class (->comparator-by-class))

(defn sorted-map-by-class
  "Like sorted-map, but uses compare-by-class such that maps with
   heterogeneously typed keys will always sort into a consistent, stable order"
  [& keyvals]
  (apply sorted-map-by default-comparator-by-class keyvals))

(defn sort-map2
  "Returns a map (including any nested maps) are in sorted order"
  {:added "0.3.3"}
  ([v]
   (cond
     (map? v)
     (reduce-kv sort-map2 (sorted-map-by-class) v)
     :else v))
  ([m k v]
   (assoc m k
     (cond
       (map? v)
       (reduce-kv sort-map2 (sorted-map-by-class) v)
       :else v))))

;; --------------------------------------------------------------------

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
                 (sort-map2 data)
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

;;Supported modes are:
;; :log - use the logging utility (e.g., log/warn)
;; :println - Just use println if level
(def ^{:doc
       "Specifies which mode of debugging output will be used, either :log or :println"}
  dbg-println-mode :println)

(def ^{:doc
       "The highest level, where higher = lower severity, at which println will be called
 when in :println mode"
       :dynamic true}
  *dbg-println-level*
  3)

;;For internal use only.  The order matters.
(def dbg-println-levels [:fatal :error :warn :info :debug :trace])

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
               `(if (<= ~level *dbg-println-level*)
                  (println ~@more)))))

;; Placeholder, until we move to Clojure 1.9
(defn clj19-boolean?
  "Return true if x is a Boolean"
  [x] (instance? Boolean x))
