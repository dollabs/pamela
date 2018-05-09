;; Copyright © 2017 Dynamic Object Language Labs Inc.
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


(ns pamela.inheritance
  (:require [clojure.pprint :refer :all]
            [pamela.utils :as putils]))


(defn get-parents [clas ir]
  (get-in ir [clas :inherit] []))

(defn collect-parents
  "For the given class, return parents in depth-first order"
  [clas ir]
  (loop [dfs [clas]
         to-visit (get-parents clas ir)]
    (let [head (first to-visit)
          head-parents (get-parents head ir)]
      (if (= 0 (count to-visit))
        dfs
        (recur (conj dfs head) (into head-parents (rest to-visit)))))))

(defn create-class-precedence
  "Return inheritance hierarchy in depth first order.
  See inheritance-hierarchy.pamela and expected output in testing.pamela.inheritance.pamela"
  [ir]
  (reduce (fn [result clas]
            (conj result {clas (collect-parents clas ir)})
            ) {} (keys ir)))

; Refer pamela.parser/default-mdef
; methods are uniquely identified by name-symbol, args( number of args)
; and following keys
(def method-signature #{:pre :post :temporal-constraints :cost :reward
                        :controllable :primitive :args-count :probability})

(defn make-method-signature
  "Given a method (map describing the method), return signature of the method"
  [method-sym method]
  (merge {:name method-sym :args-count (count (:args method))} (select-keys method method-signature)))

(defn assoc-method
  "Given vector of methods for the method,
   create a map of method-signature -> method"
  [method-sym m-vec]
  (reduce (fn [result method]
            (let [msig (make-method-signature method-sym method)]
              ;(println "method signature" method-sym)
              ;(pprint msig)
              (if (contains? msig result)
                (do (putils/dbg-println :warn "pamela.inheritance assoc-method method:"
                                        method-sym "has multiple methods with same signature" msig "\ncurrent will stay")
                    (putils/dbg-println :warn "current")
                    (putils/dbg-println :warn (with-out-str (pprint (get msig result))))
                    (putils/dbg-println :warn "another")
                    (putils/dbg-println :warn (with-out-str (pprint method))))
                (merge result {msig method}))))
          {} m-vec))

(defn assoc-methods
  "Given :methods of class, create a map of method-signature -> method"
  [clas-methods ir]
  (reduce (fn [result [method-sym m-vec]]
            (merge result (assoc-method method-sym m-vec)))
          {} clas-methods))

(defn methods-from-signature-map
  "Given method-signature -> method, returns map of {method-name [methods]} for IR"
  [methods]
  ;(println "methods-from-signature-map")
  ;(pprint methods)
  (reduce (fn [result [msig method]]
            (if (contains? result (:name msig))
              (update-in result [(:name msig)] (fn [old-vec]
                                                 (conj old-vec method)))
              (merge result {(:name msig) [method]})))
          {} methods))

(defn merge-methods
  "Returns a map of methods suitable for IR"
  [clist ir]
  (let [m-with-signature (reduce (fn [result clas]
                                   ;(println "method signature")
                                   ; For each method of class, create method signature and assoc in map
                                   ; merge and return methods
                                   ; result always holds the value from the class itself or the closest parent
                                   (merge (assoc-methods (get-in ir [clas :methods]) ir) result))
                                 {} clist)]
    (methods-from-signature-map m-with-signature)))

(defn merge-transition
  "More specific transition overrides specific transition.
  However, if they have :temporal-constraints, they are merged such that the
  new constraints have min and max of the two transitions."
  [specific more-specific]
  (let [has-tc? (or (:temporal-constraints specific) (:temporal-constraints more-specific))
        trans (or more-specific specific)]
    (if has-tc?
      (conj trans {:temporal-constraints [{:type :bounds :value (putils/merge-bounds (-> specific :temporal-constraints first :value)
                                                                              (-> more-specific :temporal-constraints first :value))}]
                   })
      trans)))

(defn merge-transitions
  "Returns a map of transitions-signature -> transition
  Note: Currently, transitions signature is assumed to be name of the transitions."
  [clist ir]
  (reduce (fn [result clas]
            (let [bounds-updated (reduce (fn [inner-result [name trans]]
                                           (merge inner-result {name (merge-transition trans (name inner-result))}))
                                         result (get-in ir [clas :transitions]))]
              ; Note bounds-updated has transition from more specific class
              ; and union of the bounds
              (merge result bounds-updated)))
          {} clist))

(defn merge-modes
  "Returns a map of modes-signature -> mode"
  [clist ir]
  (reduce (fn [result clas]
            ; result always holds the value from the class itself or the closest parent
            (merge (get-in ir [clas :modes]) result)) {} clist))

; Fields are uniquely identified by their symbol.
(defn merge-fields [clist ir]
  (reduce (fn [result clas]
            ; result always holds the value from the class itself or the closest parent
            (merge (get-in ir [clas :fields]) result)) {} clist))

(defn merge-hierarchy
  "Given class list in the order of decreasing specificity,
  merge classes and return merged IR"
  [clist ir]
  ;(println "clist" clist (first clist))
  (let [clas (first clist)
        new-class-ir (clas ir)

        fields (merge-fields clist ir)
        new-class-ir (if (pos? (count fields))
                       (assoc new-class-ir :fields fields)
                       new-class-ir)

        methods (merge-methods clist ir)
        new-class-ir (if (pos? (count methods))
                       (assoc new-class-ir :methods methods)
                       new-class-ir)

        transitions (merge-transitions clist ir)
        new-class-ir (if (pos? (count transitions))
                       (assoc new-class-ir :transitions transitions)
                       new-class-ir)

        modes (merge-modes clist ir)
        new-class-ir (if (pos? (count modes))
                       (assoc new-class-ir :modes modes)
                       new-class-ir)]
    ;(println "modes")
    ;(pprint modes)
    new-class-ir))

(defn flatten-inheritance
  ; https://bitbucket.org/dollinc-pamela/pamela/wiki/PamelaExtensions
  "For all classes, recreate IR such that classes with inheritance have modes, methods, transitions and fields
  merged according to the inheritance rules as documented."
  [ir]
  (let [class-hierarchy (create-class-precedence ir)]
    ;(println "Class hierarchy")
    ;(pprint class-hierarchy)
    (reduce (fn [result [clas clist]]
              ;(println "ir so far")
              ;(pprint result)
              (conj result {clas (merge-hierarchy clist ir)})) {} class-hierarchy)))

