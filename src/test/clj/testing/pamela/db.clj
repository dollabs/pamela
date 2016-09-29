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

(ns testing.pamela.db
  (:require [clojure.test :refer :all]
            [environ.core :refer [env]]
            [pamela.log :as plog]
            [pamela.db :refer :all]
            [pamela.utils :refer [sleep remove-clojure-comments]]
            [avenir.utils :refer [assoc-if]]
            [clojurewerkz.elastisch.native :as es]
            [clojurewerkz.elastisch.native.index :as esi]
            [clojurewerkz.elastisch.native.document :as doc]
            [clojurewerkz.elastisch.query :as q]
            [clojurewerkz.elastisch.native.response :as esrsp]
            ;; [pamela.pclass :as pclass]
            ;; [pamela.models :as models]
            ))

(def test-mapping-types {:example
                         {:properties
                          {:name {:type "string" :store "yes"}
                           :number {:type "integer"}
                           :notes {:type "string" :store "yes"}}}})

(def ordinals ["first" "second" "third" "forth" "fifth"
               "sixth" "seventh" "eighth" "nineth" "tenth"])

(def test-index "pamela_development")

(defn make-test-doc [i]
  {:name (str "example" i)
   :number i
   :notes (str (nth ordinals i) " example")})

;; FIXME
;; (defn reset-db [verbose?]
;;   (let [verbose (and verbose? 2)]
;;     (delete-all-models {:verbose verbose})
;;     (if verbose?
;;       (println "import..."))
;;     (doseq [i ["one" "two" "three" "four"]]
;;       (import-model {:verbose verbose :input (str "src/test/pamela/" i ".pamela")}))
;;     (sync-models-to-db)))

(deftest testing-pamela-db
  (testing "testing-pamela-db"
    (plog/initialize)
    ;; start the database (local iff ES_SERVER not set to a remote DB)
    (is (start-db-pamela (assoc-if {:verbose 0} :server (:es-server env))))
    (when-not (:es-server env)
      ;; give the db 10 seconds to start up
      (sleep 10)
      ;; (println "--- db started ---")
      )
    ;; test-index doesn't exist or can be deleted
    (is (or (not (esi/exists? (db-conn) test-index))
          (:ok (esi/delete (db-conn) test-index))))
    ;; create test-index with mapping
    (is (:ok (esi/create (db-conn) test-index :mappings test-mapping-types)))
    ;; give the index time to settle down
    (sleep 3)
    ;; no hits yet
    (is (= 0 (esrsp/total-hits (doc/search (db-conn) test-index "example"
                                 :query (q/wildcard :name "example*")))))
    ;; add 5 records
    (is (= 5 (count (for [i (range 5)]
                      (doc/create (db-conn) test-index
                        :example (make-test-doc i))))))
    ;; give the index time to settle down
    (sleep 3)
    ;; expect 5 records
    (is (= 5 (esrsp/total-hits (doc/search (db-conn) test-index "example"
                                 :query (q/wildcard :name "example*")))))
    (let [example0 (first (:hits (:hits (doc/search (db-conn) test-index "example" :query (q/term :name "example0")))))
          id (:_id example0)
          notes (:notes (:_source example0))
          new-notes "example at position zero"]
      ;; find the id for example0
      (is (not (nil? id)))
      ;; verify read the record correctly
      (is (= notes "first example"))
      ;; replace the notes
      (is (not (nil? (doc/replace (db-conn) test-index :example id
                       (assoc (make-test-doc 0) :notes new-notes)))))
      ;; give the db time to settle down
      (sleep 3)
      ;; verify the notes have been updated
      (is (= new-notes
            (:notes (:_source (first (:hits (:hits (doc/search (db-conn) test-index "example" :query (q/term :name "example0")))))))))
      ;; delete example0 by query
      (is (:ok (doc/delete-by-query (db-conn) test-index "example" (q/term :name "example0"))))
      ;; now there should be 4 records
      (is (= 4
            (esrsp/total-hits (doc/search (db-conn) test-index "example" :query (q/wildcard :name "example?"))))))
    ;; FIXME
    ;; (let [_ (reset-db false)] ;; does imports (see above)
    ;;   ;; list-models
    ;;   (is (= (list {:depends [["one" "0.2.0"] ["three" "0.2.0"]], :name "four", :doc "Forth", :version "0.2.0"} {:name "one", :doc "First", :version "0.2.0"} {:depends [["two" "0.2.0"]], :name "three", :doc "Third", :version "0.2.0"} {:name "two", :doc "Second", :version "0.2.0"})
    ;;         (sort #(compare (:name %1) (:name %2))
    ;;           (map #(dissoc % :source :url :id) (list-models)))))
    ;;   ;; describe-model
    ;;   (is (= "DESCRIBE MODEL: three\n  doc: Third\n  icon: \n  type: pclass-enumeration\n  version: \"0.2.0\"\n  depends: [[two \"0.2.0\"]]\n  rdepends: [[four \"0.2.0\"]]\nfields:\nmodes:\n  :e is an unconditional or enumeration mode\n  :f is an unconditional or enumeration mode\nmethods:\ntransitions:\nlvars:\n"
    ;;         (with-out-str (describe-model {:model "three"}))))
    ;;   (is (= (remove-clojure-comments
    ;;            (slurp (str (:user-dir env)
    ;;                     "/src/test/pamela/four.pamela")))
    ;;         (do
    ;;           (export-model {:model "four" :output "target/four.pamela"})
    ;;           (remove-clojure-comments
    ;;             (slurp (str (:user-dir env) "/target/four.pamela"))))))
    ;;   (is (= (remove-clojure-comments
    ;;            (slurp (str (:user-dir env)
    ;;                     "/src/test/pamela/four-all.pamela")))
    ;;         (do
    ;;           (export-model {:model "four"
    ;;                          :output "target/four-all.pamela"
    ;;                          :recursive true})
    ;;           (remove-clojure-comments
    ;;             (slurp (str (:user-dir env)
    ;;                      "/target/four-all.pamela"))))))
    ;;   (is (= "ERROR: cannot delete model \"two\" without --recursive due to these reverse dependencies: [[three \"0.2.0\"]]\n"
    ;;         (with-out-str (delete-model {:model "two"}))))
    ;;   (is (= "delete-model two\ndelete reverse dependencies: [[three 0.2.0]]\ndelete-model three\ndelete reverse dependencies: [[four 0.2.0]]\ndelete-model four\n"
    ;;         (with-out-str (delete-model {:verbose 1 :model "two" :recursive true}))))
    ;;   (sleep 3)
    ;;   (is (= (list "one")
    ;;         (map :name (list-models))))
    ;;   ;; stop the database
    ;;   (is (nil? (delete-all-models)))
    ;;   (is (nil? (stop-db))))))
    ))
