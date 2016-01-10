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

(ns testing.pamela.utils
  (:refer-clojure :exclude [update]) ;; clj-http
  (:require [clojure.test :refer :all]
            [pamela.utils :refer :all]
            ))

(defn map-defaults-test []
  (let [node-defaults {:name "Node name"
                       :number 123
                       :activities #{}
                       :bounds [0 :infinity]
                       ;; :extra nil ;; already the default default
                       :constraints []}
        ;; probably want to (def my-map (prototype-map ...))
        ;; for general use
        node-json-fns {:activities #(set %)
                       :bounds #(let [[lb ub] %]
                                  [lb (if (integer? ub) ub (keyword ub))])
                       :extra #(keyword %)}
        node-map (prototype-map-fn node-defaults node-json-fns "node-map")
        node0 (node-map :name "First node" :extra :more-info)
        printmap #(println %1 ": meta name:" (:prototype-name (meta %2)) "map:" %2)]
    (printmap "node0" node0)
    (println "node0 meta-data" (meta node0))
    (printmap "node0-defaults" (hash-map-defaults node0))
    (println "node0 default activities:" (get-default node0 :activities))
    (printmap "node0-compact" (hash-map-compact (hash-map-defaults node0)))
    (let [node0-json (hash-map-compact-json node0)
          node1 (hash-map-from-compact-json node0-json (node-map :only-meta-data true))
          node1-defaults-json (hash-map-defaults-json node1)
          node2 (hash-map-from-compact-json node1-defaults-json (node-map :only-meta-data true))]
      (println "node0 as compact json:" node0-json)
      (printmap "node1 from compact json:" node1)
      (println "node1 as json with defaults:" node1-defaults-json)
      (printmap "node2" node2)
      )))

(deftest testing-pamela-utils
  (testing "testing-pamela-utils"
    (is (nil? (and-f)))
    (is (and-f true))
    (is (and-f true true))
    (is (and-f true true true))
    (is (.startsWith (with-out-str (ppmeta (with-meta (fn []) {:attribute :value}))) "^{:attribute :value} "))))
