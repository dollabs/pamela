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
            [clojure.java.io :refer :all] ;; for as-file
            [clojure.pprint :as pp]
            ;; to be replaced with aleph
            ;; [clj-http.client :as http]
            [environ.core :refer [env]]
            [cheshire.core :as json]
            [pamela.mode :as mode]
            [clojure.tools.logging :as log])
  (:import [java.net
            URL]))

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

;; helper to grow the input data strings (NOT binary)
(defn str-append
  "Append b to a (if a is not nil), else return b"
  {:added "0.2.0"}
  [a b]
  (if a
    (str a b)
    b))

(defn ppmeta
  "Pretty print with metadata"
  {:added "0.2.0"}
  [body]
  (binding [*print-meta* true]
    (pp/pprint body)))

;; and as a function
(defn and-f
  "and as a function (to be used with reduce)"
  {:tag Boolean :added "0.2.0"}
  [& args]
  (when-let [[a & more-as] (not-empty args)]
    (if (and a more-as)
      (recur more-as)
      a)))

;; or as a function
(defn or-f
  "or as a function (to be used with reduce)"
  {:tag Boolean :added "0.2.0"}
  [& args]
  (when (not-empty args)
    (if-let [a (first args)]
      a
      (recur (rest args)))))

;; not as a function
(defn not-f
  "not as a function (to be used with reduce)"
  {:tag Boolean :added "0.2.0"}
  [arg]
  (when-not arg true))

(defn implies
  "logical implies"
  {:tag Boolean :added "0.2.0"}
  ([] true)
  ([a] true)
  ([a b] (or (not a) b))
  ([a b & more] (if-not b
                  (if a
                    false
                    (case (count more)
                      1 true
                      2 (implies (first more) (second more))
                      (apply implies (implies (first more) (second more)) (rest (rest more)))))
                  (apply and-f more))))

;; concat returns a vector
(defn concatv
  "Return the concat of args as a vector"
  {:added "0.2.0"}
  [& args]
  (vec (apply concat args)))

(declare assoc-if)

(defn prototype-map-fn
  "returns a a function like hash-map that undersands the prototype-defaults"
  {:added "0.2.0"}
  ([prototype-defaults]
   (prototype-map-fn prototype-defaults nil))
  ([prototype-defaults prototype-json-fns]
   (prototype-map-fn prototype-defaults prototype-json-fns nil))
  ([prototype-defaults prototype-json-fns prototype-name]
   (fn [& {:keys [only-meta-data] :as init-kvs}]
    (let [name (or prototype-name "prototype-map")
          meta-data (assoc-if {:prototype prototype-defaults :prototype-name name}
                      :prototype-json-fns prototype-json-fns)
          m (with-meta {} meta-data)]
      (if only-meta-data
        meta-data
        (if init-kvs
          (merge m init-kvs)
          m))))))

(defn hash-map-defaults
  "fill in all the defaults for m (verbosely)"
  {:added "0.2.0"}
  [m]
  (let [meta-data (meta m)
        prototype (:prototype meta-data)]
    (if prototype
      (merge (with-meta prototype meta-data) m)
      m)))

(defn get-default
  "get the default value for k in m"
  {:added "0.2.0"}
  [m k]
  (let [prototype (:prototype (meta m))
        default (get prototype k)]
    default))

(defn assoc-if
  "associate k v in m iff v (is not a default!)"
  {:added "0.2.0"}
  [m k v]
  (let [prototype (:prototype (meta m))
        default (get prototype k)]
    (if (= v default)
      m ;; do NOT explicity assoc on a default value!
      (assoc m k v))))

(defn hash-map-compact
  "remove all the defaults for m"
  {:added "0.2.0"}
  [m & [meta-data]]
  (let [meta-data (or (meta m) meta-data)
        prototype (:prototype meta-data)]
    (if prototype
      (reduce #(assoc-if %1 (first %2) (second %2)) (with-meta {} meta-data) m)
      (with-meta m meta-data))))

(defn hash-map-compact-json
  "Make a compact json form of m"
  {:added "0.2.0"}
  [m]
  (json/encode (hash-map-compact m)))

(defn hash-map-defaults-json
  "Make a default json form of m"
  {:added "0.2.0"}
  [m]
  (json/encode (hash-map-defaults m)))

;; NOTE the meta-data is required to properly decode from JSON
;; you can get that by using the specialized hashmap function you created
;; (def simple-map (prototype-map-fn {:a 123})
;; thus
;; (hash-map-from-compact-json simple-map-as-json (simple-map :only-meta-data true))
(defn hash-map-from-compact-json
  "Use prototype to convert from compact JSON to a hash map"
  {:added "0.2.0"}
  [j meta-data]
  (let [m (json/decode j true)
        fns (:prototype-json-fns meta-data)
        fix-json #(assoc %1 %2 (if-let [f (get fns %2)] (f %3) %3))
        m (reduce-kv fix-json {} m)]
    (hash-map-compact m meta-data)))

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
  "Wrapper for clj-http.core/get"
  {:added "0.2.0"}
  ([url]
   (http-get url nil))
  ([url req]
   (let [url (if (string? url) url (.toString url))]
     ;; (http/get url req)
     "NOT IMPLEMENTED YET"
     )))

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
                    file (if-not url (as-file in))
                    path (if file
                           (if (.exists file)
                             file
                             (as-file (str project-dir in))))]
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

(defn keywordize [k]
  "coerce k to a keyword"
  {:pamela :utils :added "0.2.0"}
  (cond
    (keyword? k) k
    (string? k)(let [k-str (if (.startsWith k ":") (subs k 1) k)]
                 (keyword k-str))
    (number? k) (keyword (str k))
    (symbol? k) (keyword (name k))
    :else
    (throw (AssertionError.
             (str "keywordize called with bad type for k: " (type k))))))
