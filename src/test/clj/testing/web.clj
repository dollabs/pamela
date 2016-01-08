;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns testing.web
  (:require [clojure.test :refer :all]
            [clojure.java.io :refer :all]
            [environ.core :refer [env]]
            [pamela.web :refer :all] ))

(defonce html-test-file "target/test/index.html")

(deftest testing-web
  (testing "testing-web"
    (let [html-file html-test-file]
      (write-html html-file)
      (is (.exists (as-file html-file))))))
