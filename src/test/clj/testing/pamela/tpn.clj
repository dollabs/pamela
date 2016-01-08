;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns testing.pamela.tpn
  (:require [clojure.test :refer :all]
            [pamela.tpn :refer :all]
            [pamela.pclass :refer [lvar get-model]]
            [pamela.models :refer [load-pamela-project load-pamela-string]]))

(deftest testing-pamela-tpn
  (testing "testing-pamela-tpn"
    (is (= (:p-begin cytoscape-tpn-types) "TPN-PARALLEL-BEGIN")))
  (testing "testing-tpn-demo"
    (let [tpn (load-tpn-files "src/test/pamela/tpn-demo.pamela")
          network-id (:network-id tpn)
          network (get tpn network-id)
          begin-node-id (:begin-node network)
          begin-node (get tpn begin-node-id)
          tpn-type (:tpn-type begin-node)]
      (is (not (nil? network-id)))
      (is (not (nil? network)))
      (is (not (nil? begin-node-id)))
      (is (not (nil? begin-node)))
      (is (= tpn-type :p-begin))))
  (testing "testing-choice-tpn"
    (let [tpn (load-tpn-files
              "src/test/pamela/plant.pamela"
              "src/test/pamela/choice-tpn.pamela")
          choice-tpn-pclass (get-model "choice-tpn")
          choice-tpn (choice-tpn-pclass (lvar "plant"))
          network-id (:network-id tpn)
          network (get tpn network-id)
          begin-node-id (:begin-node network)
          begin-node (get tpn begin-node-id)
          tpn-type (:tpn-type begin-node)]
      (is (= (:doc choice-tpn) "Simple Choice with 2 Activities"))
      (is (not (nil? network-id)))
      (is (not (nil? network)))
      (is (not (nil? begin-node-id)))
      (is (not (nil? begin-node)))
      (is (= tpn-type :c-begin))))
  (testing "testing-parallel-tpn"
    (let [tpn (load-tpn-files
              "src/test/pamela/plant.pamela"
              "src/test/pamela/parallel-tpn.pamela")
          parallel-tpn-pclass (get-model "parallel-tpn")
          parallel-tpn (parallel-tpn-pclass (lvar "plant"))
          network-id (:network-id tpn)
          network (get tpn network-id)
          begin-node-id (:begin-node network)
          begin-node (get tpn begin-node-id)
          tpn-type (:tpn-type begin-node)]
      (is (= (:doc parallel-tpn) "Simple Parallel with 2 Activities"))
      (is (not (nil? network-id)))
      (is (not (nil? network)))
      (is (not (nil? begin-node-id)))
      (is (not (nil? begin-node)))
      (is (= tpn-type :p-begin))))
  (testing "testing-parallel-choice-tpn"
    (let [tpn (load-tpn-files
              "src/test/pamela/plant.pamela"
              "src/test/pamela/parallel-choice-tpn.pamela")
          parallel-choice-tpn-pclass (get-model "parallel-choice-tpn")
          parallel-choice-tpn (parallel-choice-tpn-pclass (lvar "plant"))
          network-id (:network-id tpn)
          network (get tpn network-id)
          begin-node-id (:begin-node network)
          begin-node (get tpn begin-node-id)
          tpn-type (:tpn-type begin-node)]
      (is (= (:doc parallel-choice-tpn) "Parallel and Choice, each with 2 Activities"))
      (is (not (nil? network-id)))
      (is (not (nil? network)))
      (is (not (nil? begin-node-id)))
      (is (not (nil? begin-node)))
      (is (= tpn-type :p-begin)))))
