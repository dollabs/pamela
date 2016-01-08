;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns pamela.log
  "PAMELA logging."
  (:require [clojure.string :as string]
            [clojure.pprint :as pp]
            [pamela.mode :as mode]
            [environ.core :refer [env]]
            [clojure.tools.logging :as log]
            [taoensso.timbre :as timbre]))

(defonce pamela-logging (atom nil))

(def config
  {:level :debug  ; e/o #{:trace :debug :info :warn :error :fatal :report}

   ;; Control log filtering by namespaces/patterns. Useful for turning off
   ;; logging in noisy libraries, etc.:
   :ns-whitelist  [] #_["my-app.foo-ns"]
   :ns-blacklist  [] #_["taoensso.*"]

   :middleware [] ; (fns [data]) -> ?data, applied left->right

   ;; {:pattern _ :locale _ :timezone _}
   :timestamp-opts timbre/default-timestamp-opts

   :output-fn timbre/default-output-fn ; (fn [data]) -> string

   :appenders
   {:spit (timbre/spit-appender {:fname "./logs/pamela-service.log"})}})

(defn initialized?
  "Return true if the log is ready"
  {:added "0.2.0"}
  []
  (not (nil? @pamela-logging)))

(defn initialize
  "Initialize the log"
  {:added "0.2.0"}
  []
  (when-not (initialized?)
    (timbre/set-config! config)
    (log/info "PAMELA logging initialized")
    (reset! pamela-logging true)))
