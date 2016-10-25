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

;; Prevent aleph from pulling in netty's logging at :debug level
;; before we properly initialize logging
(require '(taoensso timbre))
(taoensso.timbre/set-level! :warn)

(ns pamela.daemon
  "PAMELA daemon support."
  (:require [clojure.java.io :refer :all] ;; for as-file
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [environ.core :refer [env]]
            [clj-time.core :as time]
            [clj-time.format :as timef]
            [clojure.java.io :as io]
            [compojure.core :refer [GET ANY POST defroutes]]
            [compojure.route :refer [resources not-found]]
            [ring.util.response :refer [content-type response redirect]]
            [ring.middleware.defaults :refer [wrap-defaults site-defaults]]
            [aleph.http :refer [start-server]]
            [pamela.mode :refer :all]
            [clojure.tools.logging :as log]
            [pamela.log :as plog]
            [pamela.utils :refer [sleep repl? input-data]]
            [avenir.utils :refer [str-append]]
            [pamela.web :as web])
  (:import [java.net.InetSocketAddress]))

;; The server is the http server which will serve the application
(defonce server (atom nil))

(defn running?
  "Returns true if running as pamelad"
  {:added "0.2.0"}
  []
  (not (nil? (:server @server))))

(defn stdout-option?
  "Returns true if the output file name is STDOUT"
  {:added "0.2.0"}
  [output]
  (or (nil? output) (= output "-")))

(defn stdout?
  "Returns true if the output file name is STDOUT (or running as pamelad)"
  {:added "0.2.0"}
  [output]
  (or (running?) (stdout-option? output)))

;; avoid recursive dependecy on cli
(defn pamela
  "Helper function to call pamela.cli/pamela"
  {:added "0.2.0"}
  [& args]
  (let [cli-pamela (ns-resolve (the-ns 'pamela.cli) 'pamela)]
    (if cli-pamela
      (apply cli-pamela args)
      (log/error "cannot resolve pamela"))))

(defn uploaded-url
  "Convert a form file name to a URL"
  {:added "0.2.0"}
  [filename]
  (if-not (running?)
    filename ;; failsafe
    (let [scheme "http"
          {:keys [host port]} @server
          hostport (if (= port 80) host (str host ":" port))]
      (str scheme "://" hostport "/uploaded/" filename))))

;; Bug: side effects here are not thread independent!
;; use a binding!
(defn pamela-handler
  "Handler for POSTing forms to pamelad"
  {:added "0.2.0"}
  [req]
  (reset! input-data nil)
  (let [user-agent (get-in req [:headers "user-agent"])
        curl? (and user-agent (.startsWith user-agent "curl"))
        ;; _ (log/info (str "curl?: " curl?))
        params (:params req)
        _ (log/info (str "pamelad params: " (pr-str params)))
        fields (sort
                 (remove nil?
                   (map #(if (string? %) (re-find #"^\d{3}$" %))
                     (keys params))))
        args (when-not (empty? fields)
               (remove empty?
                 (for [field fields]
                   (let [ftype (first (string/split (get params field) #":"))]
                     (case ftype
                       "file" (let [filepart (get params (str field ".f"))
                                    {:keys [filename tempfile]} filepart
                                    content (slurp (.getPath tempfile))]
                                (swap! input-data #(str-append % content))
                                ;; the following does not work?
                                ;; (.delete tempfile)
                                (uploaded-url filename))
                       "select" (get params (str field ".s"))
                       "text" (get params (str field ".t"))
                       ;; :default
                       "" ;; (str "UNKNOWN:" ftype)
                       )))))
        args (if curl?
               (cons "-w" (cons "curl" args))
               args)
        _ (log/info (str "pamelad query: " (pr-str args)))
        output (with-out-str (apply pamela args))
        ;; make sure output ends with a newline
        output (if (= (last output) \newline)
                 output
                 (str output \newline))
        ;; does the last line include a redirect directive?
        ;; // redirect: http://localhost:9100/downloads/tpn.dot.svg
        ultimate (last (clojure.string/split output #"\n"))
        r (and ultimate (.indexOf ultimate "redirect: "))
        page (if (and r (pos? r))
               (redirect (subs ultimate (+ r 10)))
               (-> (response output) (content-type "text/plain")))]
    (reset! input-data nil)
    page))

(defn web-html
  "Return pamelad web page"
  {:added "0.2.0"}
  [& [req]]
  (if (production?)
    (web/create-html)
    (web/create-dev-html)))

(def web-html-memo (memoize web-html))

(defroutes routes
  (GET "/" req web-html-memo)
  (POST "/pamela" req pamela-handler)
  (resources "/")
  (ANY "*" []
    (not-found (slurp (io/resource "404.html")))))

;; https://github.com/ring-clojure/ring-defaults
(def http-handler
  (wrap-defaults routes (dissoc site-defaults :security)))

(defn server-stop
  "Stop pamelad"
  {:added "0.2.0"}
  []
  (if (running?)
    (do
      (log/info "Stopping pamelad")
      (.close (:server @server))
      (reset! server nil))
    (log/info "pamelad already stopped")))

;; avoid recursive dependecy on db
(defn stop-db-exit
  "Stop the database and exit"
  {:added "0.2.0"}
  []
  (let [db-stop-db-exit (ns-resolve (the-ns 'pamela.db) 'stop-db-exit)]
    (log/info "pamelad shutting down")
    (server-stop)
    (if db-stop-db-exit
      (db-stop-db-exit)
      (log/error "cannot resolve pamela db-stop-db-exit"))))

;; files like /tmp/ring-multipart-1372716809497526970.tmp
(defn clear-uploads
  "Clear the downloads directory (on daemon startup)."
  {:added "0.2.0"}
  []
  (let [tmp (as-file "/tmp")
        files (and (.exists tmp)
                (filter #(.startsWith (.getName %) "ring-multipart-")
                  (.listFiles tmp)))]
    (when files
      (doseq [f files]
        (.delete f)))))

(defn clear-downloads
  "Clear stale uploaded files in the /tmp directory (on daemon startup)."
  {:added "0.2.0"}
  []
  (let [downloads (as-file (str (:user-dir env) web/downloads))
        files (and (.exists downloads) (.listFiles downloads))]
    (when files
      (doseq [f files]
        (.delete f)))))

;; export SERVER_HOST=0.0.0.0 will bind this port for other hosts to access
(defn server-start
  "Start pamelad"
  {:added "0.2.0"}
  [options]
  (let [{:keys [daemonize verbose]} options
        verbose? (pos? (or verbose 0))]
    (plog/initialize (:log-level options))
    (if (= daemonize 65535) ;; graceful shutdown
      ;; NOTE allow the remote request to complete first
      (future
        (sleep 3)
        (stop-db-exit))
      (if (running?)
        (println "pamelad already running")
        (let [hostname (or (:server-host env) "localhost")
              socket-address (java.net.InetSocketAddress. hostname daemonize)]
          (log/info (str "Starting pamelad on host " hostname
                      " and port " daemonize))
          (reset! server {:host hostname
                          :port daemonize
                          :server (start-server http-handler {:socket-address socket-address})})
          (clear-uploads)
          (clear-downloads)
          (when-not (repl?)
            (while (running?)
              (print ".")
              (flush)
              (sleep 10))))))))
