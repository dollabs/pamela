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

(ns testing.pamela.utils
  (:require [clojure.test :refer :all]
            [pamela.utils :refer :all]
            [environ.core :refer [env]]))

(deftest testing-pamela-utils
  (testing "testing-pamela-utils"
    (is (= true  (stdout? "-")))
    (let [cwd (:user-dir env)]
      (set-cwd! cwd)
      (is (= cwd (.getPath (get-cwd)))))
    ))
