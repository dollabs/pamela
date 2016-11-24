<;; Copyright © 2016 Dynamic Object Language Labs Inc.
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

(ns testing.pamela.parser
  (:require [clojure.test :refer :all]
            [pamela.parser :refer :all]))

(deftest testing-pamela-parser
  (testing "testing-pamela-parser"
    (is (= [0 0] zero-bounds))
    (let [choice-pamela "test/pamela/choice.pamela"
          choice-ir '{plant
                      {:type :pclass,
                       :args [],
                       :meta {:doc "The Plant API"},
                       :methods
                       {bounded
                        {:args [],
                         :pre {:type :literal, :value true},
                         :temporal-constraints [{:type :bounds, :value [1 5]}],
                         :reward 0,
                         :controllable false,
                         :primitive true,
                         :betweens [],
                         :post {:type :literal, :value true},
                         :cost 0,
                         :body nil,
                         :doc "Bounded"},
                        unbounded
                        {:args [],
                         :pre {:type :literal, :value true},
                         :temporal-constraints
                         [{:type :bounds, :value [0 :infinity]}],
                         :reward 0,
                         :controllable false,
                         :primitive true,
                         :betweens [],
                         :post {:type :literal, :value true},
                         :cost 0,
                         :body nil,
                         :doc "Unbounded"}}},
                      choice-tpn
                      {:type :pclass,
                       :args [plant],
                       :meta {:doc "Simple Choice with 2 Activities"},
                       :methods
                       {simple-choice-tpn
                        {:args [],
                         :pre {:type :literal, :value true},
                         :temporal-constraints
                         [{:type :bounds, :value [0 :infinity]}],
                         :reward 0,
                         :controllable false,
                         :primitive false,
                         :betweens [],
                         :post {:type :literal, :value true},
                         :cost 0,
                         :body
                         [{:type :choose,
                           :body
                           [{:type :choice,
                             :body
                             [{:type :plant-fn-symbol,
                               :name plant,
                               :method bounded,
                               :args []}]}
                            {:type :choice,
                             :body
                             [{:type :plant-fn-symbol,
                               :name plant,
                               :method unbounded,
                               :args []}]}]}],
                         :doc "Simple Choice TPN"}}}}]
      (is (= choice-ir
            (parse {:input [choice-pamela] :output "-"}))))
    ))
