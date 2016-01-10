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

(ns testing.selenium
  (:import [org.openqa.selenium.firefox FirefoxBinary FirefoxProfile FirefoxDriver])
  (:require [clojure.java.io :refer :all]
            [environ.core :refer [env]]
            [clj-webdriver.driver :as driver]
            [clj-webdriver.taxi :as taxi]))

;; NOTE this is for integration testing.. no clj only tests here

;; most users will want to initialize firefox with this function
(defn firefox-default
  "Initialize default Firefox profile"
  []
  (taxi/set-driver! {:browser :firefox}))

;; alternate function for customizing which browser and profile you need
(defn firefox-profile
  "Set Firefox driver with specific browser path and profile"
  [browser profile]
  (let [ffpath (file browser)
        ffbin (FirefoxBinary. ffpath)
        ffprofdir (file profile)
        ffprof (FirefoxProfile. ffprofdir)
        ffdrv (FirefoxDriver. ffbin ffprof)
        firefox (driver/init-driver {:webdriver ffdrv})]
    (taxi/set-driver! firefox)))

(defn start-firefox
  "Start firefox (with optional browser path and profile)"
  [& {:keys [browser profile] :as opts}]
  (let [b (or browser (:selenium-browser env))
        p (or profile (:selenium-profile env))]
    (if (and b p)
      (firefox-profile b p)
      (firefox-default))))

(defn stop-firefox []
  (taxi/quit))

(defn run-firefox [html-file]
  (let [html-url (str "file://" (.getCanonicalPath (as-file html-file)))
        runner-script "testing.runner.runtests(false);"]
    (println "Loading...:" html-url)
    (start-firefox)
    (taxi/to html-url)
    (taxi/wait-until #(taxi/exists? "#out"))
    (println "Loaded____:" html-url)
    (taxi/execute-script runner-script)
    (let [out (taxi/attribute (taxi/element "#out") :value)]
      (stop-firefox)
      out)))
