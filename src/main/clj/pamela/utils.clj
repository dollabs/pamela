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

(ns pamela.utils
  "Utility helper functions."

  (:refer-clojure :exclude [update]) ;; clj-http
  (:require [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.java.io :refer [as-url]]
            [me.raynes.fs :as fs]
            [clojure.pprint :as pp]
            ;; to be replaced with aleph
            [clj-http.client :as http]
            [environ.core :refer [env]]
            [clojure.data.json :as json]
            [pamela.mode :as mode]
            [avenir.utils :refer [str-append]]
            [clojure.tools.logging :as log])
  (:import [java.net
            URL]))

(def cwd (atom fs/*cwd*))

(defn set-cwd! [new-cwd]
  (reset! cwd (if (fs/file? new-cwd)
                  new-cwd
                  (fs/file new-cwd))))

(defn get-cwd []
  @cwd)

(defn stdout? [filename]
  (or (nil? filename) (= filename "-")))

;; file-format's supported: edn json string
(defn output-file [filename file-format edn]
  (let [is-stdout? (stdout? filename)
        filename (if (and (not is-stdout?) (string? filename))
                   (fs/expand-home filename)
                   filename)
        output-filename (if (not is-stdout?)
                          (if (fs/absolute? filename)
                            (fs/file filename)
                            (fs/file (get-cwd) filename)))
        out (cond
              (= file-format "edn")
              (with-out-str (pprint edn))
              (= file-format "json")
              (with-out-str (json/pprint edn))
              :else
              edn)]
    (if is-stdout?
      (println out)
      (spit output-filename out))))

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

;; will contain file data iff set by daemon
(defonce input-data (atom nil))

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
  (and (mode/development?) (= (:pager env) "cat")))

(defn var-of
  "Returns the var corresponding the value v (if defined in ns)."
  {:added "0.2.0"}
  [v ns]
  (first (filter #(identical? v (deref %))
           (vals (ns-publics ns)))))

(defn make-url
  "Tries to make a URL from the path (or returns nil on failure)."
  {:added "0.2.0"}
  ([path]
   (try (java.net.URL. path) (catch Exception e nil)))
  ([protocol host port file]
   (try (java.net.URL. protocol host port file) (catch Exception e nil))))

(defn http-get
  "Wrapper for GET requests"
  {:added "0.2.0"}
  ([url]
   (http-get url nil))
  ([url req]
   (let [url (if (string? url) url (.toString url))]
     (http/get url req))))

(defn get-url
  "Will retrieve the data at url (returns nil on failure)."
  {:added "0.2.0"}
  [url]
  (let [data (try
               (http-get url)
               (catch Exception e
                 (println "ERROR: unable to GET " (.toString url) "\n"
                   (.. e getMessage))))
        body (if (= 200 (:status data)) (:body data))]
    body))

(defn get-input
  "Returns a vector with the filename and the raw data input (from a file or STDIN). Note: if multiple files are given the input is concatenated and the last filename is used."
  {:added "0.2.0"}
  [options]
  (let [{:keys [verbose input]} options
        input (if (vector? input) input [input])
        verbose? (pos? (or verbose 0))]
    (loop [input input filenames [] data nil]
      (let [in (first input)
            stdin? (and in (= in "-"))]
        (if-not in ;; all done processing
          [(last filenames) data]
          (if stdin?
            (if (repl?)
              (throw (Exception. "DEV MODE cannot handle STDIN from the REPL"))
              (let [stdin (slurp *in*)]
                (log/debugf "get-input: read %d bytes from STDIN" (count stdin))
                ;; ["file:///tmp/stdin.txt" stdin]
                (recur (rest input)
                  (conj filenames "file:///tmp/stdin.txt")
                  (str-append data stdin))))
            (if-not (nil? @input-data)
              (do
                (log/debug "get-input: reading %d bytes from multipart form: %s"
                  (count @input-data) in)
                ;; ["file:///tmp/uploaded.txt" @input-data]
                (recur (rest input)
                  (conj filenames "file:///tmp/uploaded.txt")
                  @input-data))
              (let [project-dir (:user-dir env)
                    url (make-url in)
                    file (if-not url (fs/file in))
                    path (if file
                           (if (.exists file)
                             file
                             (fs/file (str project-dir in))))]
                (if url
                  (let [_ (log/debugf "get-input: GET from url: %s" url)
                        remote-filename (str url)
                        remote-data (get-url url)]
                    ;; [(str url) remote-data]
                    (recur (rest input)
                      (conj filenames remote-filename)
                      (str-append data remote-data)))
                  (if (.exists path)
                    (let [_ (log/debugf "get-input: read from file: %s" path)
                          filename (str (as-url path))
                          file-data (slurp path)]
                      ;; [filename file-data]
                      (recur (rest input)
                        (conj filenames filename)
                        (str-append data file-data)))
                    (let [_ (log/errorf "input file not found: %s" in)]
                      (recur (rest input)
                        filenames
                        data))))))))))))

(defn sleep
  "sleeps for the given number of seconds (may be fractional)"
  {:pamela :utils :added "0.2.0"}
  [s]
  (Thread/sleep (* 1000 s)))
