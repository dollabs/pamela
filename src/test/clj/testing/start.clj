;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns testing.start
  (:require [clojure.string :as string]
            [testing.selenium :as wd]
            [testing.web :as web]
            [environ.core :refer [env]]
            [pamela.mode :refer :all]))

;; this is the entry point used by the lein-selenium script

(defn -main [& args]
  (let [rc (promise)]
    ;; NOTE: because 'lein test' runs with both dev and test profiles
    ;; the program-mode env variable will be set to :dev...
    ;; here we know we are really in :test mode
    (reset! program-mode :test)
    (println "program-mode" @program-mode)
    (println "selenium-browser" (:selenium-browser env))
    (println "selenium-profile" (:selenium-profile env))
    (try
      (let [out (wd/run-firefox web/html-test-file)
            result (if (= "Test succeeded."
                         (last (string/split out #"\n")))
                     0 1)]
        (println out)
        (deliver rc result))
      (catch Exception e
        (println "Test failed.")
        (wd/stop-firefox)))
    (System/exit (if (realized? rc) @rc 1))))
