;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns testing.pamela.core
  (:require [clojure.test :refer :all]
            [pamela.core :refer :all]
            [pamela.mode :refer :all]))

(deftest testing-pamela-core
  (testing "testing-pamela-core"
    (is (not (development?)))
    (is (testing?))
    (is (not (production?)))))
