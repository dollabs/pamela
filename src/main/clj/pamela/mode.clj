;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns pamela.mode
  "Functions for determining if PAMELA is run in one of
  development, testing or production mode."
  (:require [environ.core :refer [env]]))

;; The program-mode environment variable is set for each
;; profile in project.clj
(def #^{:added "0.2.0"} program-mode
  "Current program state (one of :dev :test or :prod)."
  (atom (or (:program-mode env) :prod)))

(defn development?
  "Returns true if in :dev mode"
  {:added "0.2.0"}
  []
  (= @program-mode :dev))

(defn testing?
  "Returns true if in :test mode"
  {:added "0.2.0"}
  []
  (= @program-mode :test))

(defn production?
  "Returns true if in :prod mode"
  {:added "0.2.0"}
  []
  (= @program-mode :prod))
