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

(ns pamela.tpn2
  "TPN functions."
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            [avenir.utils :refer [concatv assoc-if keywordize vec-index-of]]
            [clojure.tools.logging :as log]))

;; helpers
(defn default-bounds?
  "Return true of bounds are [0 :infinity]"
  {:added "0.2.0"}
  [bounds]
  (and (vector? bounds)
    (or (zero? (count bounds))
      (and
        (= 2 (count bounds))
        (= 0 (first bounds))
        (= :infinity (second bounds))))))

;; returns the narrowest bounds
(defn merge-bounds [a b]
  (let [[a-lb a-ub] (if (default-bounds? a) nil a)
        [b-lb b-ub] (if (default-bounds? b) nil b)
        ;; _ (println a-lb a-ub b-lb b-ub)
        lb (if a-lb
             (if b-lb
               (min a-lb b-lb)
               a-lb)
             (if b-lb
               b-lb
               0))
        ub (if a-ub
             (if b-ub
               (if (or (= a-ub :infinity) (= b-ub :infinity))
                 :infinity
                 (max a-ub b-ub))
               a-ub)
             (if b-ub
               b-ub
               :infinity))
        ;; _ (println "LB" lb "UB" ub)
        rv (if (and (= lb 0) (= ub :infinity))
             nil ;; do NOT explicitly return default bounds
             [lb ub])]
    ;; (println "MERGE-BOUNDS" a b "=>" rv)
    rv
    ))

;; {uid tpn-object} where tpn-object has keys trimmed
(def ^{:dynamic true} *tpn-plan-map* (atom {}))

(defn reinitialize-tpn-plan-map []
  (reset! *tpn-plan-map* {}))

(defn get-tpn-plan-map [uid-or-object]
  (let [uid (if (keyword? uid-or-object)
              uid-or-object
              (:uid uid-or-object))]
    (get @*tpn-plan-map* uid)))

(defn update-tpn-plan-map! [object]
  (let [uid (:uid object)]
    (swap! *tpn-plan-map* assoc uid object)
    object))

(defn remove-tpn-plan-map! [uid-or-object]
  (let [uid (if (keyword? uid-or-object)
              uid-or-object
              (:uid uid-or-object))]
    (swap! *tpn-plan-map* dissoc uid)))


;; TPN Object hierarchy ------------------------------------

(def tpn-hierarchy (-> (make-hierarchy)
                     (derive :network :tpn-object)
                     (derive :temporal-constraint :tpn-object)
                     (derive :null-activity :tpn-object)
                     (derive :delay-activity :null-activity)
                     (derive :activity :delay-activity)
                     (derive :state :tpn-object)
                     (derive :c-begin :state)
                     (derive :p-begin :state)
                     (derive :c-end :state)
                     (derive :p-end :state)))

(defn tpn-isa? [child parent]
  (isa? tpn-hierarchy child parent))

;; print-object multi-methods --------------------------------------

(defn object-dispatch [object]
  (:tpn-type object))

(defmulti print-object
  "Print based on object type"
  #'object-dispatch
  :default :tpn-object
  :hierarchy #'tpn-hierarchy)

(defmethod print-object :tpn-object
  [object]
  (pprint object))

;; tpn-object hierarchy ----------------

(defn tpn-object
  "All TPN objects inherit from tpn-object"
  [{:keys [prefix uid]
    :or {prefix "uid-"}}]
  (update-tpn-plan-map!
    {:tpn-type :tpn-object
     :uid (or uid (keyword (gensym prefix)))}))

(defn tpn-network
  "A network"
  [{:keys [prefix uid begin-node end-node]
    :or {prefix "net-"}}]
  (update-tpn-plan-map!
    (assoc-if (tpn-object {:prefix prefix :uid uid})
      :tpn-type :network
      :begin-node begin-node
      :end-node end-node)))

;; where value is [lb ub], ub may be :infinity
(defn tpn-temporal-constraint
  "A temporal constraint"
  [{:keys [prefix uid value end-node]
    :or {prefix "tc-"}}]
  (update-tpn-plan-map!
    (assoc-if (tpn-object {:prefix prefix :uid uid})
      :tpn-type :temporal-constraint
      :value value
      :end-node end-node)))

(defn tpn-null-activity
  "A null activity"
  [{:keys [prefix uid constraints order end-node]
    :or {prefix "na-"}}]
  (update-tpn-plan-map!
    (assoc-if (tpn-object {:prefix prefix :uid uid})
      :tpn-type :null-activity
      :constraints (or constraints #{})
      :order order
      :end-node end-node)))

(defn tpn-delay-activity
  "A delay activity"
  [{:keys [prefix uid constraints order end-node
           task name controllable htn-node]
    :as options}]
  (update-tpn-plan-map!
    (assoc-if (tpn-null-activity (assoc options :prefix (or prefix "da-")))
      :tpn-type :delay-activity
      :task task
      :name name
      :controllable controllable
      :htn-node htn-node)))

(defn tpn-activity
  "An activity"
  [{:keys [prefix uid constraints order end-node
           task name controllable htn-node
           plant plantid command args argsmap]
    :as options}]
  (update-tpn-plan-map!
    (assoc-if (tpn-delay-activity (assoc options :prefix (or prefix "act-")))
      :tpn-type :activity
      :plant plant
      :plantid plantid
      :command command
      :args args
      :argsmap argsmap)))

(defn tpn-state
  "A state node"
  [{:keys [prefix uid constraints activities incidence-set
           task htn-node end-node begin]
    :or {prefix "node-"}}]
  ;; :or {prefix "na-"}}]
  (update-tpn-plan-map!
    (assoc-if (tpn-object {:prefix prefix :uid uid})
      :tpn-type :state
      :constraints (or constraints #{})
      :activities (or activities #{})
      :incidence-set (or incidence-set #{})
      :task task
      :htn-node htn-node
      :end-node end-node
      :begin begin)))

(defn tpn-c-begin
  "A choice begin node"
  [{:keys [prefix uid constraints activities incidence-set
           task htn-node end-node begin]
    :as options}]
  (update-tpn-plan-map!
    ;; (assoc-if (tpn-state (assoc options :prefix (or prefix "cb-")))
    (assoc-if (tpn-state (assoc options :prefix (or prefix "node-")))
      :tpn-type :c-begin)))

(defn tpn-c-end
  "A choice end node"
  [{:keys [prefix uid constraints activities incidence-set
           task htn-node end-node begin]
    :as options}]
  (update-tpn-plan-map!
    ;; (assoc-if (tpn-state (assoc options :prefix (or prefix "ce-")))
    (assoc-if (tpn-state (assoc options :prefix (or prefix "node-")))
      :tpn-type :c-end)))

(defn tpn-p-begin
  "A parallel begin node"
  [{:keys [prefix uid constraints activities incidence-set
           task htn-node end-node begin]
    :as options}]
  (update-tpn-plan-map!
    ;; (assoc-if (tpn-state (assoc options :prefix (or prefix "pb-")))
    (assoc-if (tpn-state (assoc options :prefix (or prefix "node-")))
      :tpn-type :p-begin)))

(defn tpn-p-end
  "A parallel end node"
  [{:keys [prefix uid constraints activities incidence-set
           task htn-node end-node begin]
    :as options}]
  (update-tpn-plan-map!
    ;; (assoc-if (tpn-state (assoc options :prefix (or prefix "pe-")))
    (assoc-if (tpn-state (assoc options :prefix (or prefix "node-")))
      :tpn-type :p-end)))
