;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

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
