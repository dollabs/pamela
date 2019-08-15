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
            [pamela.unparser :as unparser]
            [pamela.utils :refer [output-file dbg-println default-bounds? display-name-string]]))

;; local implementation of gensym so that we get predictable uids in
;; generated plans.
(defonce my-count (atom 0))

(defn my-gensym [prefix]
  (str prefix (swap! my-count inc)))

(defn reset-my-count []
  (reset! my-count 0))

;; helpers
(defn union-items
  "Unions all items into a set (ignores nil)"
  {:added "0.2.0"}
  ([]
   #{})
  ([item]
   (if item
     (if (set? item) item #{item})
     #{}))
  ([item & more]
   (set/union
     (union-items item)
     (apply union-items more))))

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
                     (derive :constraint :tpn-object)
                     (derive :temporal-constraint :constraint)
                     (derive :cost<=-constraint :constraint)
                     (derive :reward>=-constraint :constraint)
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

(defn constraint? [object]
  (isa? tpn-hierarchy (:tpn-type object) :constraint))

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
     :uid (or uid (keyword (my-gensym prefix)))}))

(defn tpn-network
  "A network"
  [{:keys [prefix uid begin-node end-node]
    :or {prefix "net-"}}]
  (update-tpn-plan-map!
    (assoc-if (tpn-object {:prefix prefix :uid uid})
      :tpn-type :network
      :begin-node begin-node
      :end-node end-node)))

;; NOTE: a between is a vector of two keywords
;;   :between [:from-end :to-begin]
;;   :between-starts [:from-begin :to-begin]
;;   :between-ends [:from-end :to-end]
;;
(defn tpn-constraint
  "A constraint superclass"
  [{:keys [prefix uid value end-node
           between between-ends between-starts
           label ;; probability cost reward guard
           ]
    :or {prefix "cnstr-"}}]
  (update-tpn-plan-map!
    (assoc-if (tpn-object {:prefix prefix :uid uid})
      :tpn-type :constraint
      :value value
      :end-node end-node
      :between between
      :between-ends between-ends
      :between-starts between-starts
      :label label
      ;; :probability probability
      ;; :cost cost
      ;; :reward reward
      ;; :guard guard
      )))

;; where value is [lb ub], ub may be :infinity
(defn tpn-temporal-constraint
  "A temporal constraint"
  [{:keys [prefix uid value end-node
           between between-ends between-starts
           label]
    :as options}]
  (update-tpn-plan-map!
    (assoc (tpn-constraint (assoc options :prefix (or prefix "tc-")))
      :tpn-type :temporal-constraint)))

(defn tpn-cost<=-constraint
  "A cost<= constraint"
  [{:keys [prefix uid value end-node
           between between-ends between-starts
           label]
    :as options}]
  (update-tpn-plan-map!
    (assoc (tpn-constraint (assoc options :prefix (or prefix "costle-")))
      :tpn-type :cost<=-constraint)))

(defn tpn-reward>=-constraint
  "A reward>= constraint"
  [{:keys [prefix uid value end-node
           between between-ends between-starts
           label]
    :as options}]
  (update-tpn-plan-map!
    (assoc (tpn-constraint (assoc options :prefix (or prefix "rewardge-")))
      :tpn-type :reward>=-constraint)))

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
           plant plant-id plant-part plant-interface
           command display-name display-args args argsmap]
    :as options}]
  (update-tpn-plan-map!
    (assoc-if (tpn-delay-activity (assoc options :prefix (or prefix "act-")))
      :tpn-type :activity
      :plant plant
      :plant-id plant-id
      :plant-part plant-part
      :plant-interface plant-interface
      :command command
      :display-name display-name
      :display-args display-args
      :args args
      :argsmap argsmap)))

(defn tpn-state
  "A state node"
  [{:keys [prefix uid constraints activities incidence-set
           task htn-node end-node begin
           label sequence-end sequence-label
           ;; cost<= reward>=
           ]
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
      :sequence-end sequence-end
      :sequence-label sequence-label
      ;; :cost<= cost<=
      ;; :reward>= reward>=
      )))

(defn tpn-c-begin
  "A choice begin node"
  [{:keys [prefix uid constraints activities incidence-set
           task htn-node end-node begin
           label sequence-label]
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
           label sequence-label]
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

(defn get-tpn-end-node-ids [activity-ids]
  (mapv #(:end-node (get-tpn-plan-map %)) activity-ids))

(defn get-uncompleted-ids [tpn-notes a-uid end-node-ids]
  (let [completed (:completed
                   (swap! tpn-notes update-in [:completed] conj a-uid))]
    (loop [uncompleted-ids []
           end-node (first end-node-ids)
           more (rest end-node-ids)]
      (if-not end-node
        uncompleted-ids
        (recur
          (if (completed end-node)
            uncompleted-ids
            (conj uncompleted-ids end-node))
          (first more)
          (rest more))))))

(defn dbg-tpn-notes [tpn-notes]
  (println "==TPN-NOTES====================")
  (let [notes (dissoc @tpn-notes :network :tc-ends :nodes)
        {:keys [sequence-begins sequence-ends]} notes]
    (pprint notes)
    (doseq [sequence-begin (keys sequence-begins)]
      (let [begin (get-tpn-plan-map sequence-begin)
            sequence-end (get sequence-begins sequence-begin)
            reverse-begin (get sequence-ends sequence-end)
            end (get-tpn-plan-map sequence-end)]
        (println "VERIFY TPN-SEQUENCE sb" (or sequence-begin "BAD SB!")
          "se" (or sequence-end "BAD SE!")
          "begin:sequence-end" (and begin (:sequence-end begin)
                                 (= (:sequence-end begin) sequence-end))
          "end" (if (:uid end) "exists" "BAD END!")
          (if (= reverse-begin sequence-begin)
            "reverse MATCH"
            (str "reverse BAD " reverse-begin))
          )
        ))
    (println "============================")
    ))

;; move all constraints that end on from-end to to-end
;; ALSO handle state nodes that have :sequence-end
(defn move-constraints [tpn-notes from-end to-end]
  (let [{:keys [network tc-ends sequence-begins sequence-ends]} @tpn-notes
        {:keys [network-id begin-node end-node]} network
        tc-from-uids (get tc-ends from-end [])
        tc-to-uids (get tc-ends to-end [])
        sequence-begin (get sequence-ends from-end)
        sequence-end (get sequence-begins from-end)]
    (dbg-println :trace "  MOVE CONSTRAINTS FROM" from-end "TO" to-end
      "SEQUENCE-BEGIN" sequence-begin "SEQUENCE-END" sequence-end)
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
    (when (pos? (count tc-from-uids))
      (doseq [from-uid tc-from-uids]
        (update-tpn-plan-map!
          (assoc (get-tpn-plan-map from-uid)
            :end-node to-end)))
      (swap! tpn-notes update-in [:tc-ends]
        (fn [tc-ends]
          (assoc (dissoc tc-ends from-end)
            to-end (concatv tc-to-uids tc-from-uids)))))
    ;; is from-end a sequence-begin, move the the begin to to-end
    (when sequence-begin
      (let [begin (get-tpn-plan-map sequence-begin)]
        (if-not begin
          (dbg-println :trace "  BAD sequence-begin node!")
          (do
            (update-tpn-plan-map! (assoc begin :sequence-end to-end))
            (swap! tpn-notes update-in [:sequence-ends] #(dissoc % from-end))
            (swap! tpn-notes assoc-in [:sequence-ends to-end] sequence-begin)
            (swap! tpn-notes assoc-in [:sequence-begins sequence-begin] to-end)
            )
          )))
    ;; if from-end is a sequence-end, update the sequence-begin
    (when sequence-end
      (swap! tpn-notes update-in [:sequence-begins] #(dissoc % from-end))
      (swap! tpn-notes assoc-in [:sequence-begins to-end] sequence-end)
      (swap! tpn-notes assoc-in [:sequence-ends sequence-end] to-end)
      )
    ;; for extreme debugging
    ;; (when (or sequence-begin sequence-end)
    ;;   (dbg-tpn-notes tpn-notes))
    ))

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
        a-begin? (#{:p-begin :c-begin} a-type)
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
        {:keys [tpn-type constraints activities label display-name
                sequence-end sequence-label htn-node]} s
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
        move-tc-to-b? (or
                        (and a-begin?
                          (> an 1))
                        (and (#{:p-begin :c-begin} b-type)
                          (= :state a-type)
                          (= an 1))
                        (and (#{:p-end :c-end} a-type)
                          ;; (= :state b-type)
                          (= an 1)))
        _ (dbg-println :trace "CHECK a-uid" a-uid "a-type" a-type "a-htn-node" a-htn-node
            "\n  AN" an "A0-UID" a0-uid
            "\n  todo" (count a-todo) "=" a-todo
            "\n  done" (count a-done) "=" a-done
            "\n  a0-type" a0-type "s-uid" s-uid  "SN" sn "tpn-type" tpn-type
            "\n  htn-node" htn-node "sequence-end" sequence-end
            "\n  a-constraints" a-constraints
            "\n  constraints" constraints "move-tc-to-b?" move-tc-to-b?
            "\n  b-constraints" b-constraints
            "\n  s0-type" s0-type "b-type" b-type "b-uid" b-uid)
        next-uid
        (cond
          (zero? (count a-todo))
          (let [end-node-ids (get-tpn-end-node-ids a-activities)
                uncompleted-ids (get-uncompleted-ids
                                  tpn-notes a-uid end-node-ids)]
            ;; (println "  ... no more to remove for" a-uid)
            (doseq [node-id (rest uncompleted-ids)]
              (remove-superfluous tpn-notes node-id))
            (first uncompleted-ids))
          ;; s/ A a0 S s0 B / A s0 B /
          (and (= tpn-type :state) (= 1 sn) (= s0-type :null-activity)
            (= a0-type :null-activity))
          (let [a-activities (disj a-activities a0-uid)
                a-activities (if s0-uid (conj a-activities s0-uid) a-activities)
                a (assoc-if a
                    :activities a-activities
                    :end-node (if (= (:end-node a) s-uid)
                                b-uid))
                b (assoc-if b
                    :htn-node htn-node)
                [a b] (if move-constraints?
                        (if move-tc-to-b?
                          [a
                           (assoc-if b
                             :constraints
                             (set/union b-constraints constraints)
                             :display-name display-name
                             :label label
                             :sequence-end sequence-end
                             :sequence-label sequence-label
                             )]
                          [(assoc-if a
                             :constraints
                             (set/union a-constraints constraints)
                             :display-name display-name
                             :label label
                             :sequence-end sequence-end
                             :sequence-label sequence-label
                             )
                           b])
                        [a b])
                a (if move-tc-to-b?
                    a
                    (assoc-if a
                      :display-name display-name
                      :label label
                      :sequence-end sequence-end
                      :sequence-label sequence-label
                      ))
                b (if move-tc-to-b?
                    (assoc-if b
                      :display-name display-name
                      :label label
                      :sequence-end sequence-end
                      :sequence-label sequence-label
                      )
                    b)]
            (update-tpn-plan-map! a)
            (update-tpn-plan-map! b)
            (when (or a0-cost a0-reward a0-probability a0-htn-node)
              (update-tpn-plan-map!
                (assoc-if s0
                  :cost a0-cost
                  :reward a0-reward
                  :probability a0-probability
                  :htn-node a0-htn-node)))
            (move-constraints tpn-notes s-uid
              (if (and move-tc-to-b? (not (#{:p-end :c-end} a-type)))
                b-uid a-uid))
            (remove-tpn-plan-map! a0-uid)
            (remove-tpn-plan-map! s-uid)
            (dbg-println :trace "  REMOVE s/ A na S a B / A a B /" a0-uid s-uid
              "A" a-activities "S0-UID" s0-uid "A-TC" a-constraints)
            (update-todo tpn-notes a-uid a0-uid s0-uid)
            a-uid)
          ;; s/ A a S na B / A a B /
          (and (= tpn-type :state) (= s0-type :null-activity) (= 1 sn)
            ;; OK, we move them below... (not move-constraints?)
            b (not (#{:c-begin :p-begin :c-end :p-end} b-type)))
          (do
            (when (= (:end-node a) s-uid)
              (update-tpn-plan-map! (assoc a :end-node b-uid)))
            (doseq [act a-activities] ;; move end-node to b-uid
              (update-tpn-plan-map!
                (assoc (get-tpn-plan-map act) :end-node b-uid)))
            (when (or label display-name sequence-end sequence-label htn-node)
              (update-tpn-plan-map!
                (assoc-if b
                  :display-name display-name
                  :label label
                  :sequence-end sequence-end
                  :sequence-label sequence-label
                  :htn-node htn-node)))
            (when (or s0-cost s0-reward s0-probability s0-htn-node)
              (update-tpn-plan-map!
                (assoc-if a0
                  :cost s0-cost
                  :reward s0-reward
                  :probability s0-probability
                  :htn-node s0-htn-node)))
            (when move-constraints? ;; move constraints to b
              (update-tpn-plan-map!
                (assoc-if b
                  :constraints (set/union b-constraints constraints)
                  :display-name display-name
                  :label label
                  :sequence-end sequence-end
                  :sequence-label sequence-label
                  :htn-node htn-node)))
            (move-constraints tpn-notes s-uid b-uid) ;; for other constraints
            (remove-tpn-plan-map! s0-uid)
            (remove-tpn-plan-map! s-uid)
            (dbg-println :trace "  REMOVE s/ A a S na B / A a B /" s0-uid s-uid)
            a-uid)
          ;; s/ A na S / A / ;; END OF GRAPH
          (and (= a0-type :null-activity) (= 1 an) (zero? (count a-constraints))
            (= tpn-type :state) (not b))
          (do
            (update-tpn-plan-map!
              (assoc-if a
                :activities #{}
                :display-name display-name
                :label label
                :sequence-end sequence-end
                :sequence-label sequence-label
                :htn-node (or a-htn-node htn-node)))
            (move-constraints tpn-notes s-uid a-uid)
            (dbg-println :trace "  REMOVE s/ A na S / A /" a0-uid s-uid)
            (remove-tpn-plan-map! a0-uid)
            (remove-tpn-plan-map! s-uid)
            nil)
          ;; s/ A na S / S / ;; BEGINNING OF GRAPH
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
                  :htn-node a-htn-node
                  :display-name (:display-name a)
                  :label (:label a)
                  :sequence-end (:sequence-end a)
                  :sequence-label (:sequence-label a)
                  )))
            (when (or a0-cost a0-reward a0-probability
                    (and (not s0-htn-node) a0-htn-node))
              (update-tpn-plan-map!
                (assoc-if s0
                  :cost a0-cost
                  :reward a0-reward
                  :probability a0-probability
                  ;; give priority to the lower level hem on the right
                  ;; iff present in s0
                  :htn-node (or s0-htn-node a0-htn-node))))
            (move-constraints tpn-notes a-uid s-uid)
            (remove-tpn-plan-map! a0-uid)
            (remove-tpn-plan-map! a-uid)
            (dbg-println :trace "  REMOVE s/ S na A / A /" a0-uid a-uid
              "(:sequence-end S)" (:sequence-end a))
            s-uid)
          ;; move constraints, htn-node on state node before begin to the begin
          (and (= a-type :state) (= 1 an) (#{:p-begin :c-begin} tpn-type)
            (or (pos? (count a-constraints)) a-htn-node))
          (let [a (assoc (dissoc a :htn-node) :constraints #{})
                s (assoc-if s
                    :constraints (set/union constraints a-constraints)
                    :htn-node a-htn-node)]
            (update-tpn-plan-map! a)
            (update-tpn-plan-map! s)
            (dbg-println :trace "  MOVE a-constraints" a-constraints
              "a-htn-node" a-htn-node
              "on state node before begin to the begin" s-uid)
            s-uid)
          :else
          (do
            ;; consider moving a-constraints that end on B to the one activity
            ;; ONLY handle exactly one constraint now
            ;; (when (and (= 1 an) (pos? (count a-constraints)))
            (if (and (= 1 an) (= 1 (count a-constraints)))
              (let [ac (get-tpn-plan-map (first a-constraints))
                    a0 (get-tpn-plan-map (first a-activities))]
                (when (= (:end-node ac) s-uid)
                  (dbg-println :trace "  MOVING constraint from node" a-uid
                    "to activity" (:uid a0))
                  (update-tpn-plan-map!
                    (assoc a0
                      :constraints (set/union (:constraints a0) a-constraints)))
                  (update-tpn-plan-map!
                    (assoc a
                      :constraints #{}))))
              (dbg-println :trace "  NOTMOVING constraint from node" a-uid
                "an" an "acn" (count a-constraints))
              )
            (dbg-println :trace "  DONE for" a0-uid)
            ;; extreme debugging
            ;; (dbg-tpn-notes tpn-notes)
            (update-todo tpn-notes a-uid a0-uid nil)
            a-uid))]
    (if next-uid
      (recur tpn-notes next-uid))))

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
                                   :end-node end-node}
                         :completed #{}})]
    ;; (println "REMOVE SUPERFLUOUS NA's network-id" network-id
    ;;   "begin-node" begin-node)
    (doseq [uid (keys @*tpn-plan-map*)]
      (let [object (get-tpn-plan-map uid)
            {:keys [tpn-type end-node sequence-end]} object]
        (cond
          (constraint? object)
          (swap! tpn-notes update-in [:tc-ends end-node]
            (fn [tc-uids]
              (if tc-uids (conj tc-uids uid) [uid])))
          sequence-end
          (do
            (dbg-println :trace "TPN SEQUENCE sequence-begin" uid
              "sequence-end" sequence-end)
            ;; NOTE since a given node can have ONLY one :sequence-end
            ;; we make the assumption that the sequence is a 1-1 mapping
            ;; add pointer from sequence-end to the sequence-begin
            (swap! tpn-notes assoc-in [:sequence-ends sequence-end] uid)
            ;; add pointer to the sequence begin
            (swap! tpn-notes assoc-in [:sequence-begins uid] sequence-end)
            ;; extreme debugging
            ;; (dbg-tpn-notes tpn-notes)
            ))))
    (remove-superfluous tpn-notes begin-node)))

;; remove invalid slots
;; 1. Remove state :end-node if :sequence-end is not set
;; 2. add :end-node if :sequence-end is set
;; 3. remove :sequence-end everywhere
(defn remove-invalid-tpn-attributes []
  (doseq [uid (keys @*tpn-plan-map*)]
    (let [object (get-tpn-plan-map uid)
          {:keys [tpn-type sequence-end end-node constraints]} object]
      (cond
        (and (= :state tpn-type) end-node (not sequence-end))
        (update-tpn-plan-map! (dissoc object :end-node))
        (and (= :state tpn-type) sequence-end (not= end-node sequence-end))
        (update-tpn-plan-map!
          (assoc
            (dissoc object :sequence-end)
              :end-node sequence-end))
        sequence-end
        (update-tpn-plan-map! (dissoc object :sequence-end))
        ;; (and (#{:state :c-end :p-end} tpn-type) end-node)
        ;; (update-tpn-plan-map! (dissoc object :end-node))
        ;; (#{:c-end :p-end} tpn-type)
        ;; (update-tpn-plan-map! (dissoc object :htn-node))
        ;; (and (= :null-activity tpn-type) constraints)
        ;; (update-tpn-plan-map! (dissoc object :constraints))
        ))))

(defn update-incidence-set []
  (doseq [uid (keys @*tpn-plan-map*)]
    (let [object (get-tpn-plan-map uid)
          {:keys [tpn-type end-node]} object
          node (if (and (pamela.tpn/tpn-isa? tpn-type :null-activity) end-node)
                 (get-tpn-plan-map end-node))]
      (when node
        (update-tpn-plan-map!
          (update-in node [:incidence-set] conj uid))))))

(defn optimize-tpn-map []
  (remove-superfluous-null-activities)
  (remove-invalid-tpn-attributes) ;; MUST run after remove-superfluous-null-activities
  (update-incidence-set))

;; (defn get-tc-from-body [body end-node]
;;   (let [{:keys [temporal-constraints
;;                 ;; label probability cost reward guard
;;                 ]} body
;;         bounds (if (and temporal-constraints
;;                      (= 1 (count temporal-constraints)))
;;                  (:value (first temporal-constraints)))
;;         bounds (if (default-bounds? bounds) nil bounds)
;;         tc (if bounds
;;              (tpn-temporal-constraint
;;                {:value bounds
;;                 :end-node end-node
;;                 ;; :label label
;;                 ;; :probability probability
;;                 ;; :cost cost
;;                 ;; :reward reward
;;                 ;; :guard guard
;;                 }))]
;;     tc))

;; NOTE: plant-id is nil and no longer used
(defn build-tpn [ir labels tpn-pclass tpn-plant-id tpn-pclass-args pfn parent-begin-uid
                 & [parent-order]]
  (dbg-println :trace "BUILD-TPN tpn-pclass:" tpn-pclass "tpn-pclass-args" tpn-pclass-args
    "\n  pfn type:" (:type pfn))
  (let [{:keys [type primitive body probability name method-ref args
                condition field temporal-constraints
                cost reward cost<= reward>=
                exactly min max controllable
                catch label enter leave]} pfn
        ;; _ (println "BUILD-TPN1" type "cost<=" cost<= "reward>=" reward>=)
        ;; FIXME: these classic TPN's typically do NOT use full
        ;; symbol-ref dereferencing therefore we'll just assume the
        ;; second name is the method
        [method-arg method] (:names method-ref)
        method-pclass (or (:pclass (get tpn-pclass-args method-arg))
                        method-arg)
        _ (dbg-println :trace "BUILD-TPN type:" type "method-pclass:" method-pclass
            "method:" method)
        sequence? (= :sequence type)
        [label sequence-label] (if sequence? [nil label] [label nil])
        choice? (= :choose type)
        plant-fn? (= type :method-fn)
        delay-fn? (= type :delay)
        basic-fn? (or plant-fn? delay-fn?)
        ;; plant-sym (or (first (keys tpn-pclass-args)) tpn-pclass)
        plant-sym (or method-pclass tpn-pclass)
        plant-ctor (if plant-fn? (get tpn-pclass-args method-arg))
        {:keys [pclass plant-id plant-interface]} plant-ctor
        ;; assume the first mdef is the correct one
        plant-mdef (if plant-fn? (get-in ir [plant-sym :methods method 0]))
        _ (dbg-println :trace "BUILD-TPN plant-sym:" plant-sym
            "plant-mdef:" plant-mdef)
        id (if plant-fn? (or plant-id tpn-plant-id))
        plant-id (if id (str id (if plant-interface "@") plant-interface))
        _ (dbg-println :trace "BUILD-TPN plant-ctor:" plant-ctor
            "\n id" id "plant-id" plant-id)
        method-args (if plant-fn? (map str (:args plant-mdef)))
        args (mapv unparser/unparse-cond-expr args)
        argsmap (if plant-fn? (zipmap method-args args))
        _ (dbg-println :trace "BUILD-TPN method-args:" method-args
            "\n  args:" args
            "\n  argsmap:" argsmap)
        command (if delay-fn? "delay" (str method))
        cost (or cost (:cost plant-mdef))
        reward (or reward (:reward plant-mdef))
        controllable (if (false? controllable)
                       controllable
                       (or controllable (:controllable plant-mdef) false))
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
        end-uid (:uid end)
        bounds (if (and temporal-constraints (= 1 (count temporal-constraints)))
                 (:value (first temporal-constraints)))
        _ (dbg-println :trace "BUILD-TPN bounds:" bounds)
        bounds (if (default-bounds? bounds) nil bounds)
        method-bounds (if plant-mdef
                        (get-in plant-mdef [:temporal-constraints 0 :value]))
        _ (dbg-println :trace "BUILD-TPN method-bounds:" method-bounds)
        method-bounds (if (or (not method-bounds)
                            (default-bounds? method-bounds))
                        nil method-bounds)
        bounds (or bounds method-bounds) ;; default to method bounds
        _ (dbg-println :trace "BUILD-TPN bounds:" bounds)
        tc (if bounds
             (tpn-temporal-constraint {:value bounds :end-node end-uid}))
        cost-le (if cost<=
                  (tpn-cost<=-constraint {:value cost<= :end-node end-uid}))
        reward-ge (if reward>=
                    (tpn-reward>=-constraint {:value reward>= :end-node end-uid}))
        constraints (apply union-items (map :uid [tc cost-le reward-ge]))
        activity (if basic-fn?
                   (tpn-activity
                     {:plant (if (and (not delay-fn?) plant-sym) (str plant-sym))
                      :plant-id plant-id
                      :command command
                      :display-name (display-name-string command)
                      :args (if-not delay-fn? args)
                      :argsmap (if-not delay-fn? argsmap)
                      :label label
                      :sequence-label sequence-label
                      :cost cost
                      :reward reward
                      :controllable controllable
                      ;; :order parent-order
                      :end-node end-uid}))
        ;; _ (if activity (println "ACT" activity))
        begin ((if (= type :parallel)
                 tpn-p-begin
                 (if (= type :choose)
                   tpn-c-begin
                   tpn-state))
               {:activities (if activity #{(:uid activity)})
                :constraints constraints
                :end-node end-uid
                :label (if-not basic-fn? label)
                :sequence-label (if-not basic-fn? sequence-label)
                })
        begin-uid (:uid begin)
        ;; _ (println "BUILD-TPN2 constraints from" begin-uid "\n"
        ;;     (with-out-str (pprint [tc cost-le reward-ge])))
        prev-end (atom begin)
        order (atom 0)]
    ;; record label for activity
    (if (:label activity)
      (swap! labels assoc (:label activity)
        [begin-uid (:end-node activity)]))
    ;; record label(s) for begin
    (if (:label begin)
      (swap! labels assoc (:label begin)
        [begin-uid (:end-node begin)]))
    (if (:sequence-label begin)
      (swap! labels assoc (:sequence-label begin)
        [begin-uid (:end-node begin)]))
    ;; connect to parent-begin
    (let [na-begin (tpn-null-activity {:order parent-order
                                       :end-node begin-uid})]
      (update-tpn-plan-map!
        (update-in parent-begin [:activities] conj
          (:uid na-begin))))
    ;; (println "BUILD-TPN begin" begin-uid "end" end-uid
    ;; "BODY" (count body))
    (when (and (not primitive) (#{:parallel :choose :sequence} type))
      (doseq [b body]
        ;; NOTE: not cost reward, etc.
        (let [{:keys [type temporal-constraints
                      label sequence-label cost<= reward>=
                      probability cost reward guard primitive]} b
              child-body (:body b)
              child-choice? (= :choose type)
              ;; _ (println "BUILD-TPN3" type
              ;;     "choice?" choice?
              ;;     "child-choice?" child-choice?
              ;;     "body size" (count child-body)
              ;;     "primitive" primitive
              ;;     "cost<=" cost<= "reward>=" reward>=)
              plant-fn? (= type :plant-fn-symbol)
              ;; _ (println "--B" (dissoc b :body))
              se (tpn-state {})
              bounds (if (and temporal-constraints
                           (not child-choice?)
                           (= 1 (count temporal-constraints)))
                       (:value (first temporal-constraints)))
              bounds (if choice? ;; may be on choice and plant-fn
                       (if (default-bounds? bounds) nil bounds))
              tc (if bounds
                   (tpn-temporal-constraint
                     {:value bounds :end-node (:uid se)}))
              cost-le (if (and (not child-choice?) cost<=)
                        (tpn-cost<=-constraint
                          {:value cost<= :end-node (:uid se)}))
              reward-ge (if (and (not child-choice?) reward>=)
                          (tpn-reward>=-constraint
                            {:value reward>= :end-node (:uid se)}))
              constraints (apply union-items
                            (map :uid [tc cost-le reward-ge]))
              choice-opts (if choice?
                            (assoc-if {}
                              :probability probability
                              :cost cost
                              :reward reward
                              :guard guard)
                            {})
              b (if choice?
                  (if (not primitive) (first child-body))
                  b)
              ;; _ (println "BUILD-TPN3.5 b" b)
              ;; [choice-constraints choice-opts constraints b]
              ;; (if choice?
              ;;   [constraints
              ;;    (assoc-if {}
              ;;      :probability probability
              ;;      :cost cost
              ;;      :reward reward
              ;;      :guard guard
              ;;      )
              ;;    nil ;; (get-tc-from-body (first body) (:uid se))
              ;;    (and (not primitive) (first body))]
              ;;   [nil
              ;;    {}
              ;;    constraints
              ;;    b])
              ;; avoid duplicating TC in a plant-fn
              ;; choice-tc (if (and choice-tc
              ;;                 (not= (:value choice-tc) (:value tc)))
              ;;             choice-tc)
              ;; _ (println "TC[" @order"]" tc "CHOICE-TC" choice-tc)
              first-b? (zero? @order)
              [label sequence-label] (if choice?
                                       (if (#{:p-begin :c-begin} (:type b))
                                         [label nil]
                                         [nil label]
                                         ))
              sb (tpn-state (assoc-if
                              {:constraints constraints
                               ;; (or constraints choice-constraints)
                               :end-node (:uid se)}
                              :label label
                              :sequence-label sequence-label))]
          ;; (println "BUILD-TPN4 constraints from" (:uid sb) "\n"
          ;;   (with-out-str (pprint (:constraints sb))))
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
            (let [se-na (tpn-null-activity {:end-node end-uid})]
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
          (build-tpn ir labels tpn-pclass plant-id tpn-pclass-args b (:uid sb) @order)
          (swap! order inc)))
      (when sequence?
        (let [na-end (tpn-null-activity {:end-node end-uid})]
          (update-tpn-plan-map!
            (update-in @prev-end [:activities] conj
              (:uid na-end))))))))

(defn add-between-constraints [labels betweens]
  (doseq [between betweens]
    (let [{:keys [type from to temporal-constraints cost<= reward>=]} between
          bounds (get-in temporal-constraints [0 :value])
          bounds (if (default-bounds? bounds) nil bounds)
          [from-begin from-end] (get @labels from)
          [to-begin to-end] (get @labels to)
          begin-uid (if (= type :between-starts) from-begin from-end)
          end-uid (if (= type :between-ends) to-end to-begin)
          tc (if bounds
               (tpn-temporal-constraint
                 {:value bounds :end-node end-uid type [from to]}))
          cost-le (if cost<=
                    (tpn-cost<=-constraint
                      {:value cost<= :end-node end-uid type [from to]}))
          reward-ge (if reward>=
                      (tpn-reward>=-constraint
                        {:value reward>= :end-node end-uid type [from to]}))
          constraints (apply union-items (map :uid [tc cost-le reward-ge]))]
      (update-tpn-plan-map!
        (update-in (get-tpn-plan-map begin-uid) [:constraints]
          union-items constraints)))))

;; assumes args are to the tpn-pclass constructor!
(defn create-tpn [ir plant-id tpn-ks tpn-args]
  (reinitialize-tpn-plan-map)
  (let [tpn-pclass (first tpn-ks)
        tpn-pclass-def (get ir tpn-pclass)
        {:keys [args]} tpn-pclass-def
        tpn-method (get-in ir tpn-ks)
        ;; not pre post cost reward controllable betweens
        {:keys [temporal-constraints body betweens]} tpn-method
        tpn-pclass-args (zipmap args tpn-args) ;; maps symbol -> argval
        end (tpn-state {})
        end-uid (:uid end)
        bounds (if (and temporal-constraints (= 1 (count temporal-constraints)))
                 (:value (first temporal-constraints)))
        bounds (if (default-bounds? bounds) nil bounds)
        tc (if bounds
             (tpn-temporal-constraint
               {:value bounds :end-node end-uid}))
        begin (tpn-state {:constraints (if tc #{(:uid tc)})
                          :end-node end-uid})
        begin-uid (:uid begin)
        labels (atom {})]
    (swap! *tpn-plan-map* assoc
      :network-id (:uid (tpn-network {:begin-node begin-uid
                                      :end-node end-uid})))
    (dbg-println :trace "CREATE-TPN tpn-pclass:" tpn-pclass
      "tpn-pclass-args" tpn-pclass-args)
    (build-tpn ir labels tpn-pclass plant-id tpn-pclass-args (first body) begin-uid)
    (add-between-constraints labels betweens)
    (optimize-tpn-map)
    @*tpn-plan-map*))

;; returns tpn-ks in ir
(defn find-tpn-method-construct [ir construct-tpn]
  (let [[demo-pclass tpn-field tpn-method] (map symbol
                                             (string/split construct-tpn  #":"))
        _ (dbg-println :trace "FTMC demo-pclass:" demo-pclass
            "tpn-field:" tpn-field "tpn-method:" tpn-method)
        tpn-pclass (if (= :pclass (get-in ir [demo-pclass :type]))
                     (get-in ir [demo-pclass :fields tpn-field :initial :pclass]))
        ;; {:keys [args id]} (get-in ir [demo-pclass :fields tpn-field :initial])
        demo-pclass-ctor (get-in ir [demo-pclass :fields tpn-field :initial])
        _ (dbg-println :trace "FTMC tpn-pclass:" tpn-pclass
            "demo-pclass-ctor:" demo-pclass-ctor)
        ;; assume the first method is the one with zero arity
        args (:args demo-pclass-ctor)
        tpn-ks [tpn-pclass :methods tpn-method 0]
        tpn-method-def (get-in ir tpn-ks)
        get-field-def (fn [arg]
                        (let [{:keys [type value names]} arg
                              n0 (first names)]
                          (cond
                            (= :field-ref type)
                            (get-in ir [demo-pclass :fields n0 :initial])
                            :else
                            value)))
        tpn-args (mapv get-field-def args)]
    (dbg-println :trace "FTMC tpn-args" tpn-args)
    (if-not tpn-method-def
      (do
        (log/errorf "parse: --construct-tpn argument invalid:"
          construct-tpn)
        [nil nil nil])
      (if-not (= 0 (count (:args tpn-method-def)))
        (do
          (log/errorf "parse: --construct-tpn method with zero arity not found:"
            construct-tpn)
          [nil nil nil])
        [nil tpn-ks tpn-args]))))

;; NOTE there might not be a plant (there may be just one pclass)
(defn find-tpn-method-default [ir]
  (let [ir-syms (keys ir)]
    (loop [tpn-ks nil plant nil tpn-args nil k (first ir-syms) more (rest ir-syms)]
      (if-not k
        (if (and tpn-ks tpn-args)
          [nil tpn-ks tpn-args]
          (do
            (log/error "unable to find the default TPN method: please specify --construct-tpn pclass:field:method")
            [nil nil nil]))
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
                         [k :methods
                          (first (keys (get-in ir [k :methods])))
                          0 ;; assume the first mdef is correct
                          ]))]
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
;; NOTE: return {:error "message"} on failure
(defn load-tpn [ir options]
  (let [{:keys [construct-tpn file-format output]} options
        [plant-id tpn-ks args] (if construct-tpn
                                 (find-tpn-method-construct ir construct-tpn)
                                 (find-tpn-method-default ir))
        _ (dbg-println :trace "LOAD-TPN plant-id:" plant-id "tpn-ks" tpn-ks
            "args" args)
        tpn (if tpn-ks
              (create-tpn ir plant-id tpn-ks args)
              {:error
               "unable to find TPN method based on --construct argument"})]
    (if (:error tpn)
      tpn
      (do
        (output-file output file-format tpn)
        tpn))))
