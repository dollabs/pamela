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

(ns pamela.tpn
  "TPN functions."
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.logging :as log]
            [me.raynes.fs :as fs]
            [avenir.utils :refer [concatv assoc-if keywordize vec-index-of]]
            [pamela.daemon :as daemon]
            [pamela.utils :refer [output-file]]))

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
        rv (if (and (= lb 0) (= ub :infinity))
             nil ;; do NOT explicitly return default bounds
             [lb ub])]
    rv))

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
  [{:keys [prefix uid value end-node
           ;; label probability cost reward guard
           ]
    :or {prefix "tc-"}}]
  (update-tpn-plan-map!
    (assoc-if (tpn-object {:prefix prefix :uid uid})
      :tpn-type :temporal-constraint
      :value value
      :end-node end-node
      ;; :label label
      ;; :probability probability
      ;; :cost cost
      ;; :reward reward
      ;; :guard guard
      )))

(defn tpn-null-activity
  "A null activity"
  [{:keys [prefix uid constraints order end-node
           label sequence-label probability cost reward guard
           ]
    :or {prefix "na-"}}]
  (update-tpn-plan-map!
    (assoc-if (tpn-object {:prefix prefix :uid uid})
      :tpn-type :null-activity
      :constraints (or constraints #{})
      :order order
      :end-node end-node
      :label label
      :sequence-label sequence-label
      :probability probability
      :cost cost
      :reward reward
      :guard guard
      )))

(defn tpn-delay-activity
  "A delay activity"
  [{:keys [prefix uid constraints order end-node
           label sequence-label probability cost reward guard
           task name controllable htn-node]
    :as options}]
  (let [controllable (if (or (false? controllable) (nil? controllable))
                       false true)]
    (update-tpn-plan-map!
      (assoc-if
        (assoc
          (tpn-null-activity (assoc options :prefix (or prefix "da-")))
            :controllable controllable)
        :tpn-type :delay-activity
        :task task
        :name name
        :htn-node htn-node))))

(defn tpn-activity
  "An activity"
  [{:keys [prefix uid constraints order end-node
           label sequence-label probability cost reward guard
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
           task htn-node end-node begin
           label sequence-label cost<= reward>=]
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
      :begin begin
      :label label
      :sequence-label sequence-label
      :cost<= cost<=
      :reward>= reward>=
      )))

(defn tpn-c-begin
  "A choice begin node"
  [{:keys [prefix uid constraints activities incidence-set
           task htn-node end-node begin
           label sequence-label cost<= reward>=]
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
           task htn-node end-node begin
           label sequence-label cost<= reward>=]
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

;; ---------------------------

;; remove invalid slots
(defn remove-invalid-tpn-attributes []
  (doseq [uid (keys @*tpn-plan-map*)]
    (let [object (get-tpn-plan-map uid)
          {:keys [tpn-type end-node constraints]} object]
      (cond
        (and (#{:state :c-end :p-end} tpn-type) end-node)
        ;; (and (#{:c-end :p-end} tpn-type) end-node)
        ;; (and (#{:state} tpn-type) end-node)
        (update-tpn-plan-map! (dissoc object :end-node :htn-node))
        (#{:c-end :p-end} tpn-type)
        (update-tpn-plan-map! (dissoc object :htn-node))
        (and (= :null-activity tpn-type) constraints)
        (update-tpn-plan-map! (dissoc object :constraints))
        ))))

(defn get-tpn-end-node-ids [activity-ids]
  (mapv #(:end-node (get-tpn-plan-map %)) activity-ids))

;; move all constraints that end on from-end to to-end
(defn move-constraints [tpn-notes from-end to-end]
  (let [{:keys [tc-ends network]} @tpn-notes
        {:keys [network-id begin-node end-node]} network
        from-uids (or (get tc-ends from-end) [])
        to-uids (or (get tc-ends to-end) [])]
    (when (= from-end begin-node)
      (update-tpn-plan-map!
        (assoc (get-tpn-plan-map network-id)
          :begin-node to-end))
      (swap! tpn-notes assoc-in [:network :begin-node] to-end))
    (when (= from-end end-node)
      (update-tpn-plan-map!
        (assoc (get-tpn-plan-map network-id)
          :end-node to-end))
      (swap! tpn-notes assoc-in [:network :end-node] to-end))
    (when (pos? (count from-uids))
      (doseq [from-uid from-uids]
        (update-tpn-plan-map!
          (assoc (get-tpn-plan-map from-uid)
            :end-node to-end)))
      (swap! tpn-notes update-in [:tc-ends]
        (fn [tc-ends]
          (assoc (dissoc tc-ends from-end)
            to-end (concatv to-uids from-uids)))))))

(defn get-todo [tpn-notes node-id activities]
  (let [{:keys [todo done]} (get-in @tpn-notes [:nodes node-id])]
    (if todo
      [todo done]
      (do
        (swap! tpn-notes update-in [:nodes node-id]
          assoc :todo activities :done #{})
        [activities #{}]))))

(defn update-todo [tpn-notes node-id completed added]
  (let [{:keys [todo done]} (get-in @tpn-notes [:nodes node-id])
        todo (disj todo completed)
        todo (if added (conj todo added) todo)
        done (conj done completed)]
    (swap! tpn-notes update-in [:nodes node-id]
          assoc :todo  todo :done done)))

(defn remove-superfluous [tpn-notes a-uid]
  (let [a (get-tpn-plan-map a-uid)
        {:keys [tpn-type constraints activities htn-node]} a
        a-type tpn-type
        a-constraints constraints
        a-activities activities
        a-htn-node htn-node
        an (count a-activities)
        ;; a-todo (set/difference a-activities a-done)
        [a-todo a-done] (get-todo tpn-notes a-uid a-activities)
        a0-uid (first a-todo)
        a0 (if a0-uid (get-tpn-plan-map a0-uid))
        a0-type (:tpn-type a0)
        a0-cost (:cost a0)
        a0-reward (:reward a0)
        a0-probability (:probability a0)
        a0-htn-node (:htn-node a0)
        s-uid (:end-node a0)
        s (if s-uid (get-tpn-plan-map s-uid))
        {:keys [tpn-type constraints activities label sequence-label htn-node]} s
        move-constraints? (pos? (count constraints))
        sn (count activities)
        s0-uid (first activities)
        s0 (if s0-uid (get-tpn-plan-map s0-uid))
        s0-type (:tpn-type s0)
        s0-cost (:cost s0)
        s0-reward (:reward s0)
        s0-probability (:probability s0)
        s0-htn-node (:htn-node s0)
        b-uid (:end-node s0)
        b (if b-uid (get-tpn-plan-map b-uid))
        b-constraints (:constraints b)
        b-type (:tpn-type b)
        move-tc-to-b? (or (#{:p-begin :c-begin} a-type)
                        (#{:p-begin :c-begin} b-type))]
    ;; (println "CHECK a-uid" a-uid "N" an "A0-UID" a0-uid
    ;;   "\n  todo" (count a-todo) "=" a-todo
    ;;   "\n  done" (count a-done) "=" a-done
    ;;   "\n  a0-type" a0-type "s-uid" s-uid  "tpn-type" tpn-type
    ;;   "\n  s0-type" s0-type "b-type" b-type "b-uid" b-uid)
    ;; (when label
    ;;   (println "  LABEL" label))
    ;; (when sequence-label
    ;;   (println "  SEQUENCE-LABEL" sequence-label))
    (cond
      (zero? (count a-todo))
      (let [node-ids (get-tpn-end-node-ids a-activities)]
        ;; (println "  ... no more to remove for" a-uid)
        (doseq [node-id node-ids]
          (remove-superfluous tpn-notes node-id)))
      ;; s/ A a0 S s0 B / A s0 B /
      (and (= tpn-type :state) (= 1 sn) (= s0-type :null-activity)
        (= a0-type :null-activity))
      (let [a-activities (disj a-activities a0-uid)
            a-activities (if s0-uid (conj a-activities s0-uid) a-activities)
            ;; a-constraints (set/union a-constraints constraints)
            a (assoc-if a
                :activities a-activities
                :htn-node (or a-htn-node htn-node))
            ;; a (assoc a
            ;;     :activities a-activities
            ;;     :constraints a-constraints)
            move-to-b? (and b move-tc-to-b?)
            [a b] (if move-constraints?
                          (if move-to-b?
                            [a
                             (assoc-if b
                               :constraints
                               (set/union b-constraints constraints)
                               :label label
                               :sequence-label sequence-label)]
                            [(assoc-if a
                               :constraints
                               (set/union a-constraints constraints)
                               :label label
                               :sequence-label sequence-label)
                             b])
                          [a b])
            a (if move-to-b?
                a
                (assoc-if a
                  :label label
                  :sequence-label sequence-label))
            b (if move-to-b?
                (assoc-if b
                  :label label
                  :sequence-label sequence-label)
                b)]
        (update-tpn-plan-map! a)
        (update-tpn-plan-map! b)
        (when (or a0-cost a0-reward a0-probability a0-htn-node)
          (update-tpn-plan-map!
            (assoc-if s0
              :cost a0-cost
              :reward a0-reward
              :probability a0-probability
              :htn-node a0-htn-node
              )))
        (move-constraints tpn-notes s-uid (if move-to-b? b-uid a-uid))
        (remove-tpn-plan-map! a0-uid)
        (remove-tpn-plan-map! s-uid)
        ;; (println "  REMOVE s/ A na S a B / A a B /" a0-uid s-uid
        ;;   "A" a-activities "S0-UID" s0-uid "A-TC" a-constraints)
        (update-todo tpn-notes a-uid a0-uid s0-uid)
        (remove-superfluous tpn-notes a-uid))
      ;; s/ A a S na B / A a B /
      (and (= tpn-type :state) (= s0-type :null-activity) (= 1 sn)
        (not move-constraints?)
        b (not (#{:c-begin :p-begin :c-end :p-end} b-type)))
      (do
        (doseq [act a-activities] ;; move end-node to b-uid
          (update-tpn-plan-map!
            (assoc (get-tpn-plan-map act) :end-node b-uid)))
        (when (or label sequence-label htn-node)
          (update-tpn-plan-map!
            (assoc-if b
              :label label
              :sequence-label sequence-label
              :htn-node htn-node)))
        (when (or s0-cost s0-reward s0-probability s0-htn-node)
          (update-tpn-plan-map!
            (assoc-if a0
              :cost s0-cost
              :reward s0-reward
              :probability s0-probability
              :htn-node s0-htn-node
              )))
        (move-constraints tpn-notes s-uid b-uid) ;; for other constraints
        (remove-tpn-plan-map! s0-uid)
        (remove-tpn-plan-map! s-uid)
        ;; (println "  REMOVE s/ A a S na B / A a B /" s0-uid s-uid)
        (remove-superfluous tpn-notes a-uid))
      ;; s/ A na S / A /
      (and (= a0-type :null-activity) (= 1 an) (zero? (count a-constraints))
        (= tpn-type :state) (not b))
      (do
        (update-tpn-plan-map!
          (assoc-if a
            :activities #{}
            :label label
            :sequence-label sequence-label
            :htn-node htn-node))
        (when (or a0-cost a0-reward a0-probability a0-htn-node)
          (update-tpn-plan-map!
            (assoc-if s0
              :cost a0-cost
              :reward a0-reward
              :probability a0-probability
              :htn-node a0-htn-node
              )))
        (move-constraints tpn-notes s-uid a-uid)
        ;; (println "  REMOVE s/ A na S / A /" a0-uid s-uid)
        (remove-tpn-plan-map! a0-uid)
        (remove-tpn-plan-map! s-uid))
      ;; s/ S na A / A /
      (and (= a-type :state) (= a0-type :null-activity) (= 1 an)
        (not (#{:c-end :p-end} tpn-type))
        (= a-uid (get-in @tpn-notes [:network :begin-node])))
      (do
        (when (or (not (empty? a-constraints)) a-htn-node)
          (update-tpn-plan-map!
            (assoc-if s
              :constraints (if (empty? a-constraints)
                             constraints
                             (set/union a-constraints constraints))
              :htn-node a-htn-node)))
        (when (or a0-cost a0-reward a0-probability a0-htn-node)
          (update-tpn-plan-map!
            (assoc-if s0
              :cost a0-cost
              :reward a0-reward
              :probability a0-probability
              :htn-node a0-htn-node
              )))
        (move-constraints tpn-notes a-uid s-uid)
        (remove-tpn-plan-map! a0-uid)
        (remove-tpn-plan-map! a-uid)
        ;; (println "  REMOVE s/ S na A / A /" a0-uid a-uid)
        (remove-superfluous tpn-notes s-uid))
      ;; move constraints on state node before begin to the begin
      (and (pos? (count a-constraints)) (= a-type :state) (= 1 an)
        (#{:p-begin :c-begin} tpn-type))
      (let [a (assoc a :constraints #{})
            s (assoc s :constraints (set/union constraints a-constraints))]
        (update-tpn-plan-map! a)
        (update-tpn-plan-map! s)
        ;; (println "  MOVE constraints on state node before begin to the begin"
        ;;   s-uid)
        (remove-superfluous tpn-notes s-uid))
      :else
      (do
        ;; (println "  DONE for" a0-uid)
        (update-todo tpn-notes a-uid a0-uid nil)
        (remove-superfluous tpn-notes a-uid)))))

;;superfluous nodes/null-activities
;; s/ A na S a  B / A a B /
;; s/ A a  S na B / A a B /
(defn remove-superfluous-null-activities []
  (let [network-id (get-tpn-plan-map :network-id)
        network (get-tpn-plan-map network-id)
        {:keys [begin-node end-node]} network
        ;; network-end-node end-node
        tpn-notes (atom {:network {:network-id network-id
                                   :begin-node begin-node
                                   :end-node end-node}})]
    ;; (println "REMOVE SUPERFLUOUS NA's network-id" network-id
    ;;   "begin-node" begin-node)
    (doseq [uid (keys @*tpn-plan-map*)]
      (let [object (get-tpn-plan-map uid)
            {:keys [tpn-type end-node]} object]
        (if (= tpn-type :temporal-constraint)
          (swap! tpn-notes update-in [:tc-ends end-node]
            (fn [tc-uids]
              (if tc-uids (conj tc-uids uid) [uid]))))))
    (remove-superfluous tpn-notes begin-node)))

(defn optimize-tpn-map []
  (remove-superfluous-null-activities)
  (remove-invalid-tpn-attributes))

(defn get-tc-from-body [body end-node]
  (let [{:keys [temporal-constraints
                ;; label probability cost reward guard
                ]} body
        bounds (if (and temporal-constraints
                     (= 1 (count temporal-constraints)))
                 (:value (first temporal-constraints)))
        bounds (if (default-bounds? bounds) nil bounds)
        tc (if bounds
             (tpn-temporal-constraint
               {:value bounds
                :end-node end-node
                ;; :label label
                ;; :probability probability
                ;; :cost cost
                ;; :reward reward
                ;; :guard guard
                }))]
    tc))

(defn build-tpn [ir labels plant-args pfn parent-begin-uid & [parent-order]]
  (let [{:keys [type name method args temporal-constraints body
                label cost reward controllable]} pfn
        sequence? (= :sequence type)
        [label sequence-label] (if sequence? [nil label] [label nil])
        choice? (= :choose type)
        plant-fn? (= type :plant-fn-symbol)
        delay-fn? (= type :delay)
        basic-fn? (or plant-fn? delay-fn?)
        plant-def (if (and plant-fn? name) (get plant-args name))
        {:keys [access observable initial]} plant-def
        {:keys [pclass id interface]} initial
        plant (or pclass name)
        plant-method (if plant-fn? (get-in ir [plant :methods method]))
        plant (if plant (str plant))
        plantid (if id (str id (if interface "@") interface))
        method-args (if plant-fn? (map str (:args plant-method)))
        argsmap (if plant-fn? (zipmap method-args args))
        command (if delay-fn? "delay" (str method))
        cost (or cost (:cost plant-method))
        reward (or reward (:reward plant-method))
        controllable (if (false? controllable)
                       controllable
                       (or controllable (:controllable plant-method) false))
        ;; _ (println "  sequence?" sequence? "choice?" choice?
        ;;     "plant-fn?" plant-fn? "basic-fn?" basic-fn?
        ;;     "label" label "sequence-label" sequence-label)
        parent-begin (get-tpn-plan-map parent-begin-uid)
        {:keys [end-node]} parent-begin
        parent-end (get-tpn-plan-map end-node)
        ;; _ (println "BUILD-TPN for" type "between"
        ;;     (:uid parent-begin) "and" (:uid parent-end))
        end-na (tpn-null-activity {:end-node (:uid parent-end)})
        ;; _ (println "END-NA" end-na)
        end ((if (= type :parallel)
               tpn-p-end
               (if (= type :choose)
                 tpn-c-end
                 tpn-state))
             {:activities (if end-na #{(:uid end-na)})})
        bounds (if (and temporal-constraints (= 1 (count temporal-constraints)))
                 (:value (first temporal-constraints)))
        bounds (if (default-bounds? bounds) nil bounds)
        method-bounds (if plant-method
                        (get-in plant-method [:temporal-constraints 0 :value]))
        method-bounds (if (or (not method-bounds)
                            (default-bounds? method-bounds))
                        nil method-bounds)
        bounds (or bounds method-bounds) ;; default to method bounds
        tc (if bounds (tpn-temporal-constraint
                        {:value bounds :end-node (:uid end)}))
        activity (if basic-fn?
                   (tpn-activity
                     {:plant plant
                      :plantid plantid
                      :command command
                      :args args
                      :argsmap argsmap
                      :label label
                      ;; :sequence-label sequence-label
                      :cost cost
                      :reward reward
                      :controllable controllable
                      ;; :order parent-order
                      :end-node (:uid end)}))
        ;; _ (if activity (println "ACT" activity))
        begin ((if (= type :parallel)
                 tpn-p-begin
                 (if (= type :choose)
                   tpn-c-begin
                   tpn-state))
               {:activities (if activity #{(:uid activity)})
                :constraints (if tc #{(:uid tc)})
                :end-node (:uid end)
                :label (if-not basic-fn? label)
                :sequence-label (if-not basic-fn? sequence-label)})
        prev-end (atom begin)
        order (atom 0)]
    ;; record label for activity
    (if (:label activity)
      (swap! labels assoc (:label activity)
        [(:uid begin) (:end-node activity)]))
    ;; record label(s) for begin
    (if (:label begin)
      (swap! labels assoc (:label begin)
        [(:uid begin) (:end-node begin)]))
    (if (:sequence-label begin)
      (swap! labels assoc (:sequence-label begin)
        [(:uid begin) (:end-node begin)]))
    ;; connect to parent-begin
    (let [na-begin (tpn-null-activity {:order parent-order
                                       :end-node (:uid begin)})]
      (update-tpn-plan-map!
        (update-in parent-begin [:activities] conj
          (:uid na-begin))))
    ;; (println "BUILD-TPN begin" (:uid begin) "end" (:uid end)
    ;; "BODY" (count body))
    (when (#{:parallel :choose :sequence} type)
        (doseq [b body]
          ;; NOTE: not cost reward, etc.
          (let [{:keys [type temporal-constraints
                        label sequence-label cost<= reward>=
                        probability cost reward guard body]} b
                plant-fn? (= type :plant-fn-symbol)
                ;; _ (println "--B" (dissoc b :body))
                se (tpn-state {})
                bounds (if (and temporal-constraints
                             (= 1 (count temporal-constraints)))
                         (:value (first temporal-constraints)))
                bounds (if choice? ;; may be on choice and plant-fn
                         (if (default-bounds? bounds) nil bounds))
                tc (if bounds
                       (tpn-temporal-constraint
                         {:value bounds
                          :end-node (:uid se)}))
                [choice-tc choice-opts tc b]
                (if choice?
                  [tc
                   (assoc-if {}
                     :probability probability
                     :cost cost
                     :reward reward
                     :guard guard
                     )
                   nil ;; (get-tc-from-body (first body) (:uid se))
                   (first body)]
                  [nil
                   {}
                   tc
                   b])
                ;; avoid duplicating TC in a plant-fn
                choice-tc (if (and choice-tc
                                (not= (:value choice-tc) (:value tc)))
                            choice-tc)
                ;; _ (println "TC[" @order"]" tc "CHOICE-TC" choice-tc)
                first-b? (zero? @order)
                [label sequence-label] (if choice?
                                         (if (#{:p-begin :c-begin} (:type b))
                                           [label nil]
                                           [nil label]
                                           ))
                sb (tpn-state (assoc-if
                                {:constraints (if tc
                                                #{(:uid tc)}
                                                (if choice-tc
                                                  #{(:uid choice-tc)}))
                                 :end-node (:uid se)}
                                :label label
                                :sequence-label sequence-label))]
            ;; record label(s) for sb
            (if (:label sb)
              (swap! labels assoc (:label sb)
                [(:uid sb) (:end-node sb)]))
            (if (:sequence-label sb)
              (swap! labels assoc (:sequence-label sb)
                [(:uid sb) (:end-node sb)]))
            (when sequence?
              ;; more than 1 in sequence? disj end-na from end
              (let [na (tpn-null-activity {:end-node (:uid sb)})]
                ;; (println "SEQ JOIN-NA" na)
                (update-tpn-plan-map!
                  (update-in @prev-end [:activities] conj
                    (:uid na))))
              (reset! prev-end se))
            (when (not sequence?)
              (let [se-na (tpn-null-activity {:end-node (:uid end)})]
                (update-tpn-plan-map!
                  (update-in se [:activities] conj
                    (:uid se-na))))
              (let [na (tpn-null-activity
                         (merge choice-opts {:end-node (:uid sb)}))
                    begin (get-tpn-plan-map begin)
                    begin (assoc-if begin
                            :activities (conj (:activities begin) (:uid na))
                            ;; :constraints (if choice-tc #{(:uid choice-tc)})
                            )]
                (update-tpn-plan-map! begin)))
            (build-tpn ir labels plant-args b (:uid sb) @order)
            (swap! order inc)))
        (when sequence?
          (let [na-end (tpn-null-activity {:end-node (:uid end)})]
            (update-tpn-plan-map!
              (update-in @prev-end [:activities] conj
                (:uid na-end))))))))

(defn add-between-constraints [labels betweens]
  (doseq [between betweens]
    (let [{:keys [type from to temporal-constraints]} between
          bounds (get-in temporal-constraints [0 :value])
          bounds (if (default-bounds? bounds) nil bounds)
          [from-begin from-end] (get @labels from)
          [to-begin to-end] (get @labels to)
          tc (if bounds
               (tpn-temporal-constraint
                 {:value bounds
                  :end-node (if (= type :between-ends) to-end to-begin)}))
          begin-uid (if (= type :between-starts) from-begin from-end)]
      (when tc
        (update-tpn-plan-map!
          (update-in (get-tpn-plan-map begin-uid) [:constraints] conj
            (:uid tc)))))))

;; assumes args are to the tpn-pclass constructor!
(defn create-tpn [ir tpn-ks tpn-args]
  (reinitialize-tpn-plan-map)
  (let [tpn-class (get ir (first tpn-ks))
        {:keys [args]} tpn-class
        tpn-method (get-in ir tpn-ks)
        ;; not pre post cost reward controllable betweens
        {:keys [temporal-constraints body betweens]} tpn-method
        plant-args (zipmap args tpn-args) ;; maps symbol -> argval
        end (tpn-state {})
        bounds (if (and temporal-constraints (= 1 (count temporal-constraints)))
                 (:value (first temporal-constraints)))
        bounds (if (default-bounds? bounds) nil bounds)
        tc (if bounds
             (tpn-temporal-constraint
               {:value bounds :end-node (:uid end)}))
        begin (tpn-state {:constraints (if tc #{(:uid tc)})
                          :end-node (:uid end)})
        labels (atom {})]
    (swap! *tpn-plan-map* assoc
      :network-id (:uid (tpn-network {:begin-node (:uid begin)
                                      :end-node (:uid end)})))
    (build-tpn ir labels plant-args (first body) (:uid begin))
    (add-between-constraints labels betweens)
    (optimize-tpn-map)
    @*tpn-plan-map*))

;; returns tpn-ks in ir
(defn find-tpn-method-construct [ir construct-tpn]
  (let [[demo-pclass tpn-field tpn-method] (if construct-tpn
                                             (string/split construct-tpn  #":"))
        demo-pclass (symbol demo-pclass)
        tpn-field (keyword tpn-field)
        tpn-method (symbol tpn-method)
        tpn-pclass (if (= :pclass (get-in ir [demo-pclass :type]))
                     (get-in ir [demo-pclass :fields tpn-field :initial :pclass]))
        tpn-args (get-in ir [demo-pclass :fields tpn-field :initial :args])
        tpn-ks [tpn-pclass :methods tpn-method]
        tpn-method-def (get-in ir tpn-ks)
        field-def (fn [field-sym]
                    (get-in ir [demo-pclass :fields (keyword field-sym)]))
        args (mapv field-def tpn-args)]
    (if-not tpn-method-def
      (do
        (log/errorf "parse: --construct-tpn argument invalid:"
          construct-tpn)
        [nil nil])
      [tpn-ks args])))

;; NOTE there might not be a plant (there may be just one pclass)
(defn find-tpn-method-default [ir]
  (let [ir-syms (keys ir)]
    (loop [tpn-ks nil plant nil tpn-args nil k (first ir-syms) more (rest ir-syms)]
      (if-not k
        (if (and tpn-ks tpn-args)
          [tpn-ks tpn-args]
          (do
            (log/error "unable to find the default TPN method: please specify --construct-tpn pclass:field:method")
            [nil nil]))
        (let [k-def (get ir k)
              {:keys [type methods args]} k-def
              plant (if (and (= :pclass type) (zero? (count args)))
                      (if plant
                        (do
                          (log/error "unable to determine tpn plant, more than one pclass takes zero args")
                          nil)
                        k))
              tpn-args (if plant
                     [{:type :pclass-ctor, :pclass plant, :args []}]
                     tpn-args)
              tpn-ks (if (and (= :pclass type) (= (count methods) 1)
                           (or
                             (and (= plant k) (zero? (count args)))
                             (and (not= plant k) (= 1 (count args)))))
                       (if tpn-ks
                         (do
                           (log/error "unable to determine tpn demo class, more than one pclass takes one arg and has one method")
                           nil)
                         [k :methods (first (keys (get-in ir [k :methods])))]))]
          (recur tpn-ks plant tpn-args (first more) (rest more))
          )))))

;; if construct-tpn is specified
;;    C:F:M argument where
;;      C is the demo-class,
;;      F is the TPN field and
;;      M is the tpn-method
;; else
;;   ONE of the pclasses must be the plant (zero args)
;;   and the OTHER pclass must be the TPN and have exactly one zero arg pmethod
(defn load-tpn [ir options]
  (let [{:keys [construct-tpn file-format cwd output]} options
        [tpn-ks args] (if construct-tpn
                        (find-tpn-method-construct ir construct-tpn)
                        (find-tpn-method-default ir))
        tpn (if tpn-ks
              (create-tpn ir tpn-ks args)
              false)
        stdout? (daemon/stdout? output)
        output-filename (if (not stdout?)
                          (if (fs/absolute? output)
                            output
                            (str cwd "/" output)))]
    (output-file stdout? cwd output-filename file-format tpn)))
