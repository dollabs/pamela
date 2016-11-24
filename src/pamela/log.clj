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

(ns pamela.log
  "PAMELA logging."
  (:require [clojure.string :as string]
            [clojure.pprint :as pp]
            [environ.core :refer [env]]
            [clojure.tools.logging :as log]
            [taoensso.timbre :as timbre]
            [avenir.utils :refer [assoc-if]]))

(defonce pamela-logging (atom nil))

(def config
  {:level :warn  ; e/o #{:trace :debug :info :warn :error :fatal :report}

   ;; Control log filtering by namespaces/patterns. Useful for turning off
   ;; logging in noisy libraries, etc.:
   :ns-whitelist  [] #_["my-app.foo-ns"]
   :ns-blacklist  [] #_["taoensso.*"]

   :middleware [] ; (fns [data]) -> ?data, applied left->right

   ;; {:pattern _ :locale _ :timezone _}
   :timestamp-opts timbre/default-timestamp-opts

   :output-fn timbre/default-output-fn ; (fn [data]) -> string

   :appenders
   {:println (timbre/println-appender {:stream :*err*})
    :spit (timbre/spit-appender {:fname "./logs/pamela.log"})}})

(defn initialized?
  "Return true if the log is ready"
  {:added "0.2.0"}
  []
  (not (nil? @pamela-logging)))

(defn initialize
  "Initialize the log"
  {:added "0.2.0"}
  [log-level & [debug-args]]
  (when-not (initialized?)
    (let [debug? (and (#{:trace :debug} log-level) debug-args)
          banner? (or debug? (= :info log-level))
          line-break "\n--------------------------------------------------------------------------------"]
      (timbre/set-config! (assoc-if config :level log-level))
      (if banner?
        (log/warn "PAMELA logging initialized at level" log-level
          (if debug?
            (str line-break "\nargs: " debug-args)
            line-break)))
      (reset! pamela-logging true))))
