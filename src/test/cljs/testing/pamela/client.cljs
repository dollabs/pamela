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

(ns testing.pamela.client
  (:require-macros [cljs.test :refer (is deftest testing)])
  (:require [cljs.test]
            [pamela.utils :as utils]
            [pamela.client :refer (app-state)]))

;; here cljs can be tested with or without the server running

(deftest testing-pamela-client
  (testing "testing-pamela-client"
    (is (not (utils/development?)))
    (is (utils/testing?))
    (is (not (utils/production?)))
    (is (not (nil? (:elements @app-state))))))
