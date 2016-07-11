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

(ns testing.pamela.models
  (:refer-clojure :exclude [assert when sequence delay]) ;; try catch
  (:require [clojure.test :refer :all]
            [pamela.pclass :refer :all]
            [pamela.models :refer :all]))

(deftest testing-pamela-models
  (testing "testing-pamela-models"
    (is (= 0 (count (list-models false))))
    (let [_ (load-pamela-project "src/test/pamela/circuit.pamela")
          models (list-models false)
          model-names (vec (sort (map name models)))
          expect-model-names ["bulb" "circuit1" "lightvals" "psw" "pwrvals"]
          c1 (new-pclass 'circuit1)
          switchedpower (get-field c1 :switchedpower)
          bulb1 (get-field c1 :bulb1)]
      (is (= 5 (count models)))
      (is (= model-names expect-model-names))

      ;; test pclass meta data
      (let [bulb1-meta (meta (get-model-var (:pclass bulb1)))]
        (is (= (:version bulb1-meta) "0.2.1"))
        (is (.endsWith (:icon bulb1-meta)
              "src/test/pamela/bulb.svg")))

      (let [c1-meta (meta (get-model-var (:pclass c1)))]
        (is (= (:depends c1-meta)
              [['psw "0.2.1"] ['bulb "0.2.1"]])))

      (is (= (with-out-str (describe-pclass-methods switchedpower))
            "pclass psw has 3 methods\nturn-on  (pamela)\n :doc\t turns on the power supply\n :pre\t (mode-of this :off)\n :post\t (mode-of this :on)\n :bounds\t [1 3]\nturn-off  (pamela)\n :doc\t turns off the power supply\n :pre\t (mode-of this :on)\n :post\t (mode-of this :off)\n :bounds\t [1 3]\nreset  (pamela)\n :doc\t resets the power supply\n :post\t (mode-of this :off)\n"))
      (is (= (with-out-str (describe-pclass-transitions switchedpower))
            "pclass psw has 3 transitions\n:off -> :on     turning on\n:on -> :off     turning off\n:* -> :fail     spontaneous switch failure\n"))

      (is (= (with-out-str (describe-pclass-transitions bulb1))
            "pclass bulb has 3 transitions\n:off -> :on     turning on bulb\n:on -> :off     turning off bulb\n:* -> :fail     spontaneous bulb failure\n"))
      (is (= (get-all-lvars c1)
            {"source"
             [['circuit1 :field :source]
              ['circuit1 :field :bulb1 'bulb :field :anode]
              ['circuit1 :field :switchedpower 'psw :field :TP1]],
             "drain"
             [['circuit1 :field :drain]
              ['circuit1 :field :bulb1 'bulb :field :cathode]
              ['circuit1 :field :switchedpower 'psw :field :TP2]],
             "pfbulb"
             [['circuit1 :field :bulb1 'bulb :transition :*:fail :probability]]}))
      )))
