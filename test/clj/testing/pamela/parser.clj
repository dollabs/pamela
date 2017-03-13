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
            [clojure.string :as string]
            [environ.core :refer [env]]
            [me.raynes.fs :as fs]
            [avenir.utils :refer [and-fn]]
            [pamela.parser :refer :all]
            [pamela.utils :refer [output-file]]))

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
                         :display-name "Bounded",
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
                         :display-name "Unbounded",
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
                         :display-name "Simple Choice Tpn",
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
    (let [top (:user-dir env)
          regression (fs/file top "test" "pamela" "regression")
          examples (filter #(string/ends-with? (fs-file-name %) ".pamela")
                     (fs/list-dir regression))
          tmpdir (fs/file top "target" "regression")]
      (if-not (fs/exists? tmpdir)
        (fs/mkdirs tmpdir))
      (doseq [example examples]
        (let [example-name (fs-file-name example)
              output (str tmpdir "/"
                       (string/replace example-name
                         #"\.pamela$" ".ir.edn"))
              options {:input [example]}
              ir (parse options)
              success? (nil? (:error ir))]
          (output-file output
            (if success? "edn" "string")
            (if success? ir (:error ir)))
          (if-not success?
            (println "ERROR in" output))
          (is success?))))))
