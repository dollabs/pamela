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

(ns pamela.htn
  "HTN functions."
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            [avenir.utils :refer [concatv assoc-if keywordize vec-index-of]]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [instaparse.core :as insta]
            [pamela.utils :refer [stdout? output-file]]
            [pamela.parser :as parser]
            [pamela.tpn :as tpn]))


;; definitions -----------------------------------------------

;; The following is intended (only) for the support of the EdgeCT program.
;; So, we'll likely move this somewhere else.
(def network-flow-types #{"VOIP" "File Xfer" "File Transfer" "VTC" "VideoStream"})

;; vars -------------------------------------------------------

(def ^{:dynamic true} *debug-ir* (atom {}))

;; {pclass-name {method-name method}}
(def ^{:dynamic true} *htn-methods* (atom {}))

(defn reinitialize-htn-method-table []
  (reset! *htn-methods* {}))

(defn add-htn-method [method]
  (let [{:keys [pclass name]} method]
    (if (and pclass name)
      (swap! *htn-methods* assoc-in [pclass name] method))))

(defn find-htn-method [pclass name & [error-if-not-found?]]
  (let [method (get-in @*htn-methods* [pclass name])]
    (when (and (not method) error-if-not-found?)
      (throw (AssertionError. (str "method not found: " name " in pclass " pclass))))
    method))


;; {uid htn-object}
(def ^{:dynamic true} *htn-objects* (atom {}))

(defn reinitialize-htn-object-table []
  (reset! *htn-objects* {}))

(defn get-htn-object [uid-or-object]
  (let [uid (if (keyword? uid-or-object)
              uid-or-object
              (:uid uid-or-object))]
    (get @*htn-objects* uid)))

(defn update-htn-object! [object]
  (let [uid (:uid object)]
    (swap! *htn-objects* assoc uid object)
    object))

;; {uid htn-object} where htn-object has keys trimmed
(def ^{:dynamic true} *htn-plan-map* (atom {}))

(defn reinitialize-htn-plan-map []
  (reset! *htn-plan-map* {}))

(defn get-htn-plan-map [uid-or-object]
  (let [uid (if (keyword? uid-or-object)
              uid-or-object
              (:uid uid-or-object))]
    (get @*htn-plan-map* uid)))

(defn update-htn-plan-map! [object]
  (let [uid (:uid object)]
    (swap! *htn-plan-map* assoc uid object)
    object))

(def ^{:dynamic true} *htn-plan-ancestry*
  "Used to keep track of the parents of the current node.  A list, with oldest at the end"
  '())

;; helper functions -------------------------------------

;; For now, we only add a SINGLE newline (if needed).  Any more would
;; result in too many lines in the Cytograph HTN display.
(defn add-newlines-if-needed
  "Used to break up long Task/Method names to (kind-of) fit into a Cytoscape node's bounding box."
  [s max-line-length]
  (if (or (zero? max-line-length)
        (< (count s) max-line-length))
    s
    (let [blank-pos (string/last-index-of s " ")
          pos (or blank-pos (string/last-index-of s "("))]
      (if pos
        (str (subs s 0 pos) "\n" (subs s (if blank-pos (inc pos) pos)))
        s))))

(defn make-network-flows
  "Make network-flows for PAC2MAN (TBD)"
  [name flow-characteristics arguments]
  {})

(defn variable?
  "non-NIL if the argument is a variable, i.e., if it's a symbol."
  [argument]
  (symbol? argument))

;; HTN Object hierarchy ------------------------------------

(def htn-hierarchy (-> (make-hierarchy)
                     (derive :temporal-constraint :htn-object)
                     (derive :inter-task-constraint :htn-object)
                     (derive :task-sequential-constraint :inter-task-constraint)
                     (derive :htn-task :htn-object)
                     (derive :htn-primitive-task :htn-task)
                     (derive :htn-nonprimitive-task :htn-task)
                     (derive :htn-expanded-nonprimitive-task :htn-nonprimitive-task)
                     (derive :htn-method :htn-object)
                     (derive :htn-expanded-method :htn-object)
                     (derive :htn-network :htn-object)
                     (derive :edge :htn-object)))

(defn htn-isa? [child parent]
  (isa? htn-hierarchy child parent))

;; name-with-args multi-methods --------------------------------------

;; FUTURE: add pclass to name-with-args
(def name-with-args-max-line-length
  "The (preferred) maximimum line length for Task/Method Names including arguments"
  25)

(def name-with-args-include-args?
  "Whether to include argument lists when displaying names of Tasks and Methods"
  true)

(defn name-with-args-dispatch [object & [max-line-length]]
  (:type object))

(defmulti name-with-args
  "Dispatch based on object type"
  #'name-with-args-dispatch
  :default :htn-object
  :hierarchy #'htn-hierarchy)

(defmethod name-with-args :htn-object
  [object & [max-line-length]]
  (str (:uid object)))

(defmethod name-with-args :htn-task
  [object & [max-line-length]]
  (let [{:keys [name arguments]} object]
    (add-newlines-if-needed
      (str name (if name-with-args-include-args? (or (seq arguments) '())))
      (or max-line-length name-with-args-max-line-length))))

(defmethod name-with-args :htn-method
  [object & [max-line-length]]
  (let [{:keys [name nonprimitive-task]} object
        method-name (str name)
        {:keys [name arguments]} nonprimitive-task]
    (str
      (add-newlines-if-needed
        method-name
        (or max-line-length name-with-args-max-line-length))
      (if max-line-length "\n" " - ")
      (add-newlines-if-needed
        (str name (if name-with-args-include-args? (or (seq arguments) '())))
        (or max-line-length name-with-args-max-line-length)))))

(defmethod name-with-args :htn-expanded-method
  [object & [max-line-length]]
  (let [{:keys [expansion-method argument-mappings]} object
        {:keys [name nonprimitive-task]} expansion-method
        method-name (str name)
        {:keys [name arguments]} nonprimitive-task
        argvals (apply str (interpose " " (map #(get argument-mappings %) arguments)))]
    (add-newlines-if-needed
      (str method-name
        (if name-with-args-include-args? "(")
        (if name-with-args-include-args? argvals)
        (if name-with-args-include-args? ")"))
      (or max-line-length name-with-args-max-line-length))))

;; name-with-argvals multi-methods --------------------------------------

(defn name-with-argvals-dispatch [object]
  (:type object))

(defmulti name-with-argvals
  "Dispatch based on object type"
  #'name-with-argvals-dispatch
  :default :htn-object
  :hierarchy #'htn-hierarchy)

(defmethod name-with-argvals :htn-object
  [object]
  (str (:uid object)))

(defn resolve-arguments [arguments ancestry-path]
  (let [hem (get-htn-object (first ancestry-path))
        {:keys [argument-mappings]} hem
        resolve-arg (fn [arg]
                      (if (variable? arg)
                        (get argument-mappings arg)
                        arg))
        arguments (map resolve-arg arguments)
        unresolved (count (filter variable? arguments))
        argvals (if (pos? unresolved)
                  (resolve-arguments arguments (nthrest ancestry-path 2))
                  arguments)]
    argvals))

(defmethod name-with-argvals :htn-task
  [object]
  (let [{:keys [name arguments ancestry-path]} object
        arguments (or arguments [])
        unresolved (count (filter variable? arguments))
        argvals (if (pos? unresolved)
                  (resolve-arguments arguments ancestry-path)
                  arguments)]
    (apply str name (conj (vec (conj (interpose ", " argvals) "(")) ")"))))

(defmethod name-with-argvals :htn-method
  [object]
  (let [{:keys [name nonprimitive-task]} object
        method-name (str name)
        {:keys [name arguments]} nonprimitive-task]
    (str
      method-name
      (str name (or (seq arguments) '())))))

(defmethod name-with-argvals :htn-expanded-method
  [object]
  (let [{:keys [expansion-method argument-mappings]} object
        {:keys [name nonprimitive-task]} expansion-method
        method-name (str name)
        {:keys [name arguments]} nonprimitive-task
        argvals (apply str (interpose " " (map #(get argument-mappings %) arguments)))]
    (str method-name "(" argvals ")")))

;; print-object multi-methods --------------------------------------

(defn object-dispatch [object]
  (:type object))

(defmulti print-object
  "Print based on object type"
  #'object-dispatch
  :default :htn-object
  :hierarchy #'htn-hierarchy)

(defmethod print-object :htn-object
  [object]
  (name-with-args object))

;; children-have-resources? multi-methods --------------------------------------

(defmulti children-have-resources?
  "Dispatch based on object type"
  #'object-dispatch
  :default :htn-object
  :hierarchy #'htn-hierarchy)

(defmethod children-have-resources? :htn-object
  [object]
  false)

(defmethod children-have-resources? :htn-expanded-nonprimitive-task
  [object]
  (some #(children-have-resources? %) (:task-expansions object)))

(defmethod children-have-resources? :htn-expanded-method
  [object]
  (some #(or (:resources %) (children-have-resources? %))
    (:subtasks object)))

;; copy-constraint-into-expansion multi-methods --------------------------------------

(defn copy-constraint-into-expansion-dispatch [object orig-method orig-subtasks new-subtasks]
  (:type object))

(defmulti copy-constraint-into-expansion
  "Dispatch based on object type"
  #'copy-constraint-into-expansion-dispatch
  :default :htn-object
  :hierarchy #'htn-hierarchy)

(defmethod copy-constraint-into-expansion :htn-object
  [object orig-method orig-subtasks new-subtasks]
  nil)

(declare task-sequential-constraint)

;; NOTE: must ensure :tasks and :subtasks slots are vectors
(defmethod copy-constraint-into-expansion :task-sequential-constraint
  [object orig-method orig-subtasks new-subtasks]
  (let [tasks (map #(get new-subtasks (vec-index-of orig-subtasks %))
                (:tasks object))
        constraint (task-sequential-constraint {:tasks tasks})]
    constraint))

;; helpers for plan-htn-task --------------------------------------

;; return methods vector
(defn find-methods-that-expand-task [task]
  (let [{:keys [type pclass name arguments]} task
        all-methods (seq (get @*htn-methods* pclass))]
    ;; DAN.. CLOSism: was (when (= type :htn-nonprimitive-task)
    ;; (println "FMTET" task)
    (when (#{:htn-nonprimitive-task :htn-expanded-nonprimitive-task} type)
      (loop [methods [] [mname method] (first all-methods) more (rest all-methods)]
        (if-not mname
          methods
          (let [{:keys [nonprimitive-task]} method
                compatible? (fn [[task-arg method-arg]]
                              (or (variable? task-arg)
                                (variable? method-arg)
                                (= task-arg method-arg)))
                match? (and (= name (:name nonprimitive-task))
                         (= (count arguments) (count (:arguments nonprimitive-task)))
                         (every? compatible?
                           (map vector arguments (:arguments nonprimitive-task))))
                methods (if match? (conj methods method) methods)]
            (recur methods (first more) (rest more))))))))

;; return args-mappings map
(defn arg-mappings-for-task-and-method [task method]
  (let [static-task-args (get-in method [:nonprimitive-task :arguments])
        dynamic-task-args (:arguments task)]
    (if (or (not (vector? static-task-args))
          (not (vector? dynamic-task-args))
          (not= (count static-task-args) (count dynamic-task-args)))
      (throw
        (AssertionError.
          (str "cannot create arg-mappings with static-task-args: "
            static-task-args " and dynamic-task-args: " dynamic-task-args))))
    (zipmap static-task-args dynamic-task-args)))

(declare htn-primitive-task)
(declare htn-expanded-nonprimitive-task)
(declare copy-temporal-constraints)
(declare htn-expanded-method)

;; NOTE assumes old-task is :htn-primitive-task or :htn-nonprimitive-task
(defn make-expanded-task
  "Make an expanded task"
  [old-task & [argument-mapping]]
  ;; (println "MET" old-task argument-mapping)
  (let [task-type (:type old-task)
        old-task (dissoc old-task :uid :label)
        task (if (= task-type :htn-primitive-task)
               (htn-primitive-task old-task)
               (htn-expanded-nonprimitive-task old-task))
        {:keys [temporal-constraints arguments]} task
        new-arguments (if argument-mapping
                        (mapv #(get argument-mapping %) arguments)
                        arguments)]
    ;; (println "  MET TASK" task)
    (update-htn-object!
      (assoc-if task
        :arguments arguments
        :temporal-constraints (and temporal-constraints
                                (copy-temporal-constraints temporal-constraints))))))

;; plan-htn-task multi-methods --------------------------------------

(defmulti plan-htn-task
  "Dispatch based on object type"
  #'object-dispatch
  :default :htn-object
  :hierarchy #'htn-hierarchy)

(defmethod plan-htn-task :htn-object
  [object]
  (throw
    (AssertionError.
      (str "cannot plan-htn-task for object type: " (:type object)))))

;; "Nothing to do for primitive tasks"
(defmethod plan-htn-task :htn-primitive-task
  [object])

;;   "Expand"
(defmethod plan-htn-task :htn-expanded-nonprimitive-task
  [object]
  (let [task object
        methods (find-methods-that-expand-task task)]
    (binding [*htn-plan-ancestry* (cons task *htn-plan-ancestry*)]
      (doseq [method methods]
        (let [{:keys [pclass name subtasks irks]} method
              arg-mappings (arg-mappings-for-task-and-method task method)
              expanded-subtasks (mapv #(make-expanded-task % arg-mappings)
                                  subtasks)
              expanded-method (htn-expanded-method
                                {:expansion-method method
                                 :argument-mappings arg-mappings
                                 :ancestry-path *htn-plan-ancestry*
                                 :subtasks expanded-subtasks
                                 :irks irks})
              ;; conj onto :task-expansions of task
              task-expansions (conj (:task-expansions (get-htn-object task))
                                expanded-method)
              task (assoc task :task-expansions task-expansions)]
          ;; (println "PHT method" method "SUBTASKS" subtasks)
          (update-htn-object! task)))
      (doseq [expanded-method (:task-expansions (get-htn-object task))]
        (let [{:keys [subtasks]} expanded-method]
          (doseq [subtask subtasks]
            (let [{:keys [cost task-type probability mission-effectiveness priority]} subtask
                  cost (if (= 1 (count subtasks))
                         (or cost (:cost task))
                         cost)
                  task-type (or task-type (:task-type task))
                  probability (or probability (:probability task))
                  mission-effectiveness (or mission-effectiveness (:mission-effectiveness task))
                  priority (or priority (:priority task))
                  ;; THIS IS NOT YET PORTED
                  ;; (if (and (not *prune-tasks-without-resources-p*)
                  ;;       (null (task-resources child-task)))
                  ;;   (setf (task-resources child-task) (task-resources task)))))
                  subtask (assoc-if subtask
                            :cost cost
                            :task-type task-type
                            :probability probability
                            :mission-effectiveness mission-effectiveness
                            :priority priority)]
              (update-htn-object! subtask)))))
      (doseq [expansion-method (:task-expansions (get-htn-object task))]
        (binding [*htn-plan-ancestry* (cons expansion-method *htn-plan-ancestry*)]
          (let [{:keys [subtasks]} expansion-method]
            (doseq [subtask subtasks]
              (let [{:keys [ancestry-path]} subtask
                    subtask (assoc subtask
                              :ancestry-path *htn-plan-ancestry*)]
                (plan-htn-task (update-htn-object! subtask))))))))))

;; pprint-htn-object multi-methods --------------------------------------

(defn just-uid [objects]
  (if (map? objects)
    (:uid objects)
    (mapv :uid objects)))

(defmulti pprint-htn-object
  "pprint based on object type"
  (fn [object] (:type object))
  :default nil)

(defmethod pprint-htn-object nil
  [object]
  (pprint object))

(defmethod pprint-htn-object :task-sequential-constraint
  [object]
  (pprint (-> object
            (update-in [:ancestry-path] just-uid)
            (update-in [:tasks] just-uid))))

(defmethod pprint-htn-object :htn-primitive-task
  [object]
  (pprint (-> object
            (update-in [:ancestry-path] just-uid)
            (update-in [:temporal-constraints] just-uid))))

(defmethod pprint-htn-object :htn-nonprimitive-task
  [object]
  (pprint (-> object
            (update-in [:ancestry-path] just-uid)
            (update-in [:temporal-constraints] just-uid))))

(defmethod pprint-htn-object :htn-expanded-nonprimitive-task
  [object]
  (pprint (-> object
            (update-in [:ancestry-path] just-uid)
            (update-in [:temporal-constraints] just-uid)
            (update-in [:task-expansions] just-uid))))

(defmethod pprint-htn-object :htn-method
  [object]
  (pprint (-> object
            (update-in [:ancestry-path] just-uid)
            (update-in [:nonprimitive-task] just-uid)
            (update-in [:subtasks] just-uid)
            (update-in [:subtask-constraints] just-uid)
            (update-in [:task-expansions] just-uid))))

(defmethod pprint-htn-object :htn-expanded-method
  [object]
  (pprint (-> object
            (update-in [:ancestry-path] just-uid)
            (update-in [:expansion-method] just-uid)
            (update-in [:subtasks] just-uid)
            (update-in [:subtask-constraints] just-uid))))

(defn pprint-htn-objects []
  (doseq [object (vals @*htn-objects*)]
    (pprint-htn-object object)))

;; htn-object hierarchy ----------------

(defn htn-object
  "All HTN objects inherit from htn-object"
  [{:keys [prefix uid ancestry-path]
    :or {prefix "hid-"}}]
  (update-htn-object!
    (assoc-if
      {:type :htn-object
       :uid (or uid (keyword (gensym prefix)))}
      :ancestry-path ancestry-path)))

(defn temporal-constraint
  "A task-local temporal constraint for that task."
  [{:keys [prefix uid ancestry-path lb ub
           start-delay ;; Wait for this long (sec) before starting this task (included in lb/ub time)
           start-time
           end-time]
    :or {prefix "tc-"}}]
  (update-htn-object!
    (assoc-if (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
      :type :temporal-constraint
      :lb lb
      :ub ub
      :start-delay start-delay
      :start-time start-time
      :end-time end-time)))

(defn inter-task-constraint
  "inter-task-constraint"
  [{:keys [prefix uid ancestry-path]
    :or {prefix "itc-"}}]
  (update-htn-object!
    (assoc (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
      :type :inter-task-constraint)))

(defn task-sequential-constraint
  "List of tasks that must be executed in sequential order.
   This is shorthand for a set of FS constraints."
  [{:keys [prefix uid ancestry-path tasks]
    :or {prefix "tsc-"
         tasks []}}]
  (update-htn-object!
    (assoc (inter-task-constraint {:prefix prefix :uid uid :ancestry-path ancestry-path})
      :type :task-sequential-constraint
      :tasks (vec tasks))))

(defn htn-task
  "All HTN tasks inherit from HTN-task"
  [{:keys [prefix uid ancestry-path pclass name arguments cost task-type probability temporal-constraints
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight
           label incidence-set edges]
    :or {prefix "task-"
         task-type :communications
         temporal-constraints []}}]
  (let [network-flows (if (network-flow-types name)
                        (make-network-flows name flow-characteristics arguments)
                        network-flows)]
    (update-htn-object!
      (assoc-if (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
        :type :htn-task
        :pclass pclass
        :name name
        :arguments arguments
        :cost cost
        :task-type task-type
        :probability probability
        :temporal-constraints temporal-constraints ;; CONSIDER converting list-> temp cons objects?
        :mission-effectiveness mission-effectiveness
        :priority priority
        :resources resources
        :network-flows network-flows
        :flow-characteristics flow-characteristics
        :mission-task-weight mission-task-weight
        :label label
        :incidence-set incidence-set ;; NOTE should be a clojure set
        :edges edges))))

(defn htn-primitive-task
  "Primitive tasks can't be decomposed any further"
  [{:keys [prefix uid ancestry-path pclass name arguments cost task-type probability temporal-constraints
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight
           label incidence-set edges irks]
    :as options}]
  (update-htn-object!
    (assoc-if (htn-task (assoc options :prefix (or prefix "hpt-")))
      :type :htn-primitive-task
      :irks irks)))

(defn htn-nonprimitive-task
  "NonPrimitive tasks can be decomposed into other tasks, by user HTN methods"
  [{:keys [prefix uid ancestry-path pclass name arguments cost task-type probability temporal-constraints
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight
           label incidence-set edges irks]
    :as options}]
  (update-htn-object!
    (assoc-if (htn-task (assoc options :prefix (or prefix "hnpt-")))
      :type :htn-nonprimitive-task
      :irks irks)))

(defn htn-expanded-nonprimitive-task
  "Expanded NonPrimitive tasks have already been decomposed into other tasks, by using HTN methods"
  [{:keys [prefix uid ancestry-path pclass name arguments cost task-type probability temporal-constraints
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight task-expansions
           label incidence-set edges irks]
    :or {task-expansions []}
    :as options}]
  (let [task (assoc-if (htn-nonprimitive-task (assoc options :prefix (or prefix "henpt-")))
               :type :htn-expanded-nonprimitive-task
               :task-expansions task-expansions)
        label (if (empty? label) (name-with-args task) label)]
    (update-htn-object! (assoc task :label label))))

(defn htn-method
  "An HTN-method is used to decompose a nonprimitive-task into one or more subtasks"
  [{:keys [prefix
           uid
           ancestry-path
           pclass
           name
           nonprimitive-task ;;This is the HTN-nonprimitive-task that this method matches on
           preconditions
           subtasks ;;This is a simple list of the tasks
           subtask-constraints ;;A list of constraints, expressing all of the partial orders
           mission-task-combination
           choice-weight
           task-expansions
           label incidence-set network edges
           irks]
    :or {prefix "hm-"
         subtasks []
         subtask-constraints []
         task-expansions []}}]
  (let [method (assoc-if (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
                 :type :htn-method
                 :pclass pclass
                 :name name
                 :nonprimitive-task nonprimitive-task
                 :preconditions preconditions
                 :subtasks (vec subtasks)
                 :subtask-constraints (vec subtask-constraints)
                 :mission-task-combination mission-task-combination
                 :choice-weight choice-weight
                 :task-expansions task-expansions
                 :incidence-set incidence-set ;; NOTE should be a clojure set
                 :edges edges
                 :network network
                 :irks irks)
        label (if (empty? label) (name-with-args method) label)
        method (update-htn-object! (assoc method :label label))]
    (add-htn-method method)
    method))

(defn htn-network
  "HTN network"
  [{:keys [prefix uid ancestry-path label rootnodes parentid]
    :or {prefix "net-"
         rootnodes []}}]
  (update-htn-object!
    (assoc-if (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
      :type :htn-network
      :label label
      :rootnodes rootnodes
      :parentid parentid)))

(defn htn-edge
  "HTN edge"
  [{:keys [prefix uid ancestry-path label end-node edge-type] ;; edge-type is usually only :choice (if not nil)
    :or {prefix "hedge-"}}]
  (update-htn-object!
    (assoc-if (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
      :type :edge
      :label label
      :end-node end-node
      :edge-type edge-type)))

;; There will be a 1:1 mapping between task-expansions and methods
(defn htn-expanded-method
  "This captures the results of applying the HTN-method to a nonprimitive task."
  [{:keys [prefix
           uid
           ancestry-path
           expansion-method
           argument-mappings
           subtasks
           subtask-constraints
           label incidence-set network
           irks]
    :or {prefix "hem-"
         subtasks []
         subtask-constraints []}}]
  (let [orig-method expansion-method
        orig-subtasks (:subtasks orig-method)
        new-subtasks subtasks ;; expanded henpt's
        subtask-constraints (map
                              #(copy-constraint-into-expansion %
                                 orig-method orig-subtasks new-subtasks)
                              (:subtask-constraints orig-method))
        hem   (assoc-if (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
                :type :htn-expanded-method
                :expansion-method expansion-method
                :argument-mappings argument-mappings
                :subtasks (vec subtasks)
                :subtask-constraints (vec subtask-constraints)
                :incidence-set incidence-set ;; NOTE should be a clojure set
                :network network
                :irks irks)
        label (if (empty? label) (name-with-args hem) label)
        hem   (update-htn-object! (assoc hem :label label))]
    hem))

;; Expansion -------------------------------------------------

(defn copy-temporal-constraints [from-temporal-constraints]
  (mapv
    #(temporal-constraint (dissoc % :uid))
    from-temporal-constraints))

;; Dan says this was never used
(defn computed-duration [temporal-constraint]
  (let [{:keys [start-time end-time]} temporal-constraint]
    (and start-time end-time (- end-time start-time))))

(defn lb [temporal-constraint]
  (let [{:keys [lb]} temporal-constraint]
    (or lb (computed-duration temporal-constraint) 0)))

(defn ub [temporal-constraint]
  (let [{:keys [ub]} temporal-constraint]
    (or ub (computed-duration temporal-constraint) :infinity)))

(defn contains-non-default-values? [temporal-constraint]
  (let [{:keys [lb ub start-delay start-time end-time]} temporal-constraint]
    (or (and lb (not (zero? lb)))
      (and ub (not (= ub :infinity)))
      start-delay
      start-time
      end-time)))

(defn make-temporal-constraint-for-sequential-subtasks [from-temporal-constraint]
  (temporal-constraint
    (assoc (dissoc from-temporal-constraint :uid) :lb 0)))

;; -------------------------------------------------------------------------

(defn ir-root-task [& args]
  args)

(def root-task-ir {
                   :argval identity
                   :boolean parser/ir-boolean
                   :float parser/ir-float
                   :integer parser/ir-integer
                   :keyword keyword
                   :natural parser/ir-integer
                   :number identity
                   :root-task ir-root-task
                   :safe-keyword identity
                   :string identity
                   :symbol symbol
                   })

;; return [pclass method args]
(defn identify-root-task [ir root-task]
  (log/trace "root-task" root-task)
  (let [parser (parser/build-parser "root-task.ebnf")
        tree (if root-task
               (insta/parses parser root-task))
        [pclass method & args]
        (cond
          (nil? tree)
          nil ;; FIND main in ir w/ no args
          (insta/failure? tree)
          (let [msg (str "parse: invalid root-task: " root-task)]
            (log/error msg)
            (log/error (with-out-str (pprint (insta/get-failure tree))))
            [:error msg nil])
          (not= 1 (count tree))
          (let [msg (str "parse: grammar is ambiguous for root-task: " root-task)]
            (log/error msg)
            (log/error (with-out-str (pprint tree)))
            [:error msg nil])
          :else
          (insta/transform root-task-ir (first tree)))
        args (vec args)
        ir-syms (keys ir)]
    (if (= pclass :error)
      (throw (AssertionError. method))
      (if (and pclass method) ;; verify
        (let [pclass-def (get ir pclass)
              method-def (if pclass-def (get-in pclass-def [:methods method]))
              method-args (if method-def (get method-def :args))]
          (if-not pclass-def
            (let [msg (str "root-task pclass not found: " pclass)]
              (log/error msg)
              (throw (AssertionError. msg)))
            (if-not method-def
              (let [msg (str "root-task pclass " pclass " does not have a method " method)]
                (log/error msg)
                (throw (AssertionError. msg)))
              (if (or (not method-args) (not= (count args) (count method-args)))
                (let [msg (str "root-task args \"" args "\" does not match arity of " pclass "." method " " method-args)]
                  (log/error msg)
                  (throw (AssertionError. msg)))
                [pclass method args]))))
        (loop [pclass nil method nil k (first ir-syms) more (rest ir-syms)] ;; find main
          (if (or (and pclass method) (not k))
            (if-not (and pclass method)
              (let [msg "root-task pclass with a zero arg main not found"]
                (log/error msg)
                (throw (AssertionError. msg)))
              [pclass method []])
            (let [k-def (get ir k)
                  {:keys [type methods]} k-def
                  main (if (and (= type :pclass) methods)
                         (get methods 'main))
                  main-args (if main (get main :args))
                  pclass (if (and main (empty? main-args)) k pclass)
                  method (if (and main (empty? main-args)) 'main method)]
              (recur pclass method (first more) (rest more)))))))))

;; if the root-task is nil
;;   look for htn-pclass with a "main" pmethod
;;   if main takes arguments throw exception
;; else
;;   expect that the htn-pclass and method are given as well as
;;   any literal args for that method (number of args should match)
;;   NOTE: argval = ( symbol | boolean | string | number | safe-keyword )
;;   -- here symbol would not be supported
;;   NOTE: may need equivalent of the "?variable", perhaps
;; "-t" "(isr-htn.main \"A\" \"B\" \"C\" {:type :lvar, :name \"last-location\"})"

(declare make-htn-methods)

;; walk ir, look for pclasses that have non-empty methods
(defn transform-htn [ir]
  (let [ir-syms (seq ir)]
    (loop [[k v] (first ir-syms) more (rest ir-syms)]
      (if-not k
        nil
        (let [{:keys [type methods]} v]
          ;; (println "TRANSFORM-HTN" k)
          (when (and (= type :pclass) methods)
            (let [method-syms (seq methods)]
              (loop [[mname method] (first method-syms) moar (rest method-syms)]
                (if mname ;; else (println "no more methods")
                  (let [{:keys [temporal-constraints args primitive body]} method]
                    ;; (println "  METHOD" mname "PRIMITIVE" primitive)
                    (if (and (not primitive) body)
                      (make-htn-methods ir k mname args body))
                    (recur (first moar) (rest moar)))))))
          (recur (first more) (rest more)))))))

;; consider memoizing or caching result
(defn find-plant-fn-bounds [ir plant-fn-pclass ctor-arg-i method]
  (let [ks (keys ir)]
    (loop [bounds nil k (first ks) more (rest ks)]
      (if (or bounds (not k))
        bounds
        (let [object (get ir k)
              {:keys [type fields]} object]
          (if (and (= type :pclass) fields)
            (let [fk-fields (seq fields)
                  bounds
                  (loop [b nil fk-field (first fk-fields) moar (rest fk-fields)]
                    (if (or b (not fk-field))
                      b
                      (let [[fk field] fk-field
                            {:keys [type pclass args]} (:initial field)
                            arg (if args (get args ctor-arg-i))
                            arg-kw (if (and (= type :pclass-ctor )
                                         (= pclass plant-fn-pclass) arg)
                                     (keyword arg))
                            plant (if arg-kw
                                    (get-in ir [k :fields arg-kw :initial :pclass]))
                            temporal-constraints (if plant
                                                   (get-in ir
                                                     [plant :methods method
                                                      :temporal-constraints]))
                            b (if temporal-constraints
                                (get-in temporal-constraints [0 :value]))]
                        (recur b (first moar) (rest moar)))))]
              (recur bounds (first more) (rest more)))
            (recur bounds (first more) (rest more))))))))

(defn irks->bounds [ir irks]
  (if irks
    (let [opts (get-in ir irks)
          {:keys [type field method temporal-constraints]} opts
          bounds (if temporal-constraints
                   (get-in temporal-constraints [0 :value]))]
      (or bounds
        ;; look up from plant method options
        (if (and (= type :plant-fn-field) field method)
          (let [pclass (first irks)
                field-irks [pclass :fields field :initial]
                field-init (get-in ir field-irks)
                {:keys [type name]} field-init
                args (get-in ir [pclass :args])
                ctor-arg-i (if (and (= type :arg-reference) name)
                             (vec-index-of args name))
                bounds (if ctor-arg-i
                         (find-plant-fn-bounds ir pclass ctor-arg-i method))]
            bounds))))))

;; root? is true to create the "synthetic" htn-network at the very
;; top of the HTN
;; begin is the state node of the parent task
(defn construct-hem-plan-map [ir hem henpt root? parent-begin-uid]
  (let [{:keys [uid label subtasks subtask-constraints edges
                ancestry-path argument-mappings irks]} hem
        hem-irks irks
        [pclass kw-methods method kw-body int-zero more-irks] hem-irks
        top-irks (if (and pclass method (zero? int-zero) (nil? more-irks))
                   [pclass kw-methods method])
        top-bounds (irks->bounds ir top-irks)
        bounds (irks->bounds ir hem-irks)
        hem-bounds (tpn/merge-bounds top-bounds bounds)
        hem-uid uid
        hem-map (assoc
                  (dissoc hem
                    :ancestry-path :expansion-method :argument-mappings :subtasks
                    :subtask-constraints :irks)
                  :incidence-set #{}
                  :edges [])
        edge (if root? (htn-edge {:end-node hem-uid}))
        edge-uid (:uid edge)
        hem-map (if root?
                  (update-in hem-map [:incidence-set] conj edge-uid)
                  hem-map)
        tsc (if (and subtask-constraints
                  (= (count subtask-constraints) 1)
                  (= (:type (first subtask-constraints))
                    :task-sequential-constraint))
              (first subtask-constraints))
        subtask-order (mapv :uid (if tsc (:tasks tsc) subtasks))
        n-subtasks (count subtask-order)
        sequential? (or (not (nil? tsc)) (= 1 n-subtasks))
        i-last-subtask (dec (count subtask-order))
        net-map (if (pos? (count subtasks))
                  (htn-network {:label label :rootnodes #{}}))
        parent-begin (tpn/get-tpn-plan-map parent-begin-uid) ;; TPN------
        {:keys [end-node]} parent-begin
        parent-end (tpn/get-tpn-plan-map end-node)
        hem-tc (if hem-bounds (tpn/tpn-temporal-constraint
                                {:value hem-bounds :end-node end-node}))
        parent-begin (assoc parent-begin
                       :htn-node (:uid hem-map)
                       :constraints (if hem-tc
                                      (conj (:constraints parent-begin)
                                        (:uid hem-tc))
                                      (:constraints parent-begin)))
        prev-end (atom nil)]
    (tpn/update-tpn-plan-map! parent-begin)
    (when root?
      (update-htn-plan-map! (update-in (get-htn-plan-map henpt)
                              [:edges] conj edge-uid))
      (update-htn-plan-map! edge))
    (if net-map
      (do
        (update-htn-plan-map! net-map)
        (update-htn-plan-map! (assoc hem-map :network (:uid net-map))))
      (update-htn-plan-map! hem-map))
    ;; HERE we need to recurse and, create a network of tasks, edges
    ;; to child hems
    (doseq [i (range n-subtasks)]
      (let [subtask-uid (get subtask-order i)
            edge (if (and sequential? (pos? i))
                   (htn-edge {:end-node subtask-uid}))
            subtask (get-htn-object subtask-uid)
            {:keys [type task-expansions arguments irks]} subtask
            bounds (if (and irks (not= irks hem-irks))
                     (irks->bounds ir irks))
            m-task-expansions (count task-expansions)
            primitive? (= type :htn-primitive-task)
            parallel? (not sequential?)
            choice? (and (not parallel?) (> m-task-expansions 1))
            label (name-with-argvals subtask)
            subtask-map (assoc
                          (dissoc subtask :pclass :arguments :task-expansions
                            :name :task-type :ancestry-path :temporal-constraints
                            :irks)
                          :label label
                          :incidence-set (if edge #{(:uid edge)} #{})
                          :edges #{})
            ;; TPN ------------------
            se (tpn/tpn-state {})
            activity (if primitive?
                       (tpn/tpn-activity ;; :task subtask
                         {:name label
                          :htn-node (:uid hem-map)
                          :end-node (:uid se)}))
            tc (if bounds (tpn/tpn-temporal-constraint
                            {:value bounds :end-node (:uid se)}))
            sb (tpn/tpn-state {:activities (if activity #{(:uid activity)})
                               :constraints (if tc #{(:uid tc)})
                               :end-node (:uid se)
                               :htn-node (:uid hem-map)
                               })
            [begin end] (if parallel?
                          (if (zero? i)
                            (let [end (tpn/tpn-p-end {})
                                  begin (tpn/tpn-p-begin {:end-node (:uid end)
                                                          :htn-node
                                                          (:uid hem-map)
                                                          })]
                              [begin end])
                            (let [parent-na-begin (tpn/get-tpn-plan-map
                                                    (->
                                                      (tpn/get-tpn-plan-map
                                                        parent-begin-uid)
                                                      :activities
                                                      first))
                                  begin (tpn/get-tpn-plan-map
                                          (:end-node parent-na-begin))
                                  end (tpn/get-tpn-plan-map
                                        (:end-node begin))]
                              [begin end]))
                          (if choice?
                            (let [end (tpn/tpn-c-end {}) ;; choice
                                  choose-irks (-> task-expansions
                                                first
                                                :irks
                                                butlast
                                                butlast
                                                vec)
                                  bounds (irks->bounds ir choose-irks)
                                  choice-tc (if bounds
                                              (tpn/tpn-temporal-constraint
                                                {:value bounds
                                                 :end-node (:uid end)}))
                                  begin (tpn/tpn-c-begin
                                          {:end-node (:uid end)
                                           :constraints (if choice-tc
                                                          #{(:uid choice-tc)})
                                           :htn-node (:uid hem-map)
                                           })]
                              [begin end])
                            [sb se]))]
        (update-htn-plan-map! subtask-map)
        (if (or (not sequential?) (zero? i))
          (update-htn-plan-map! (update-in (get-htn-plan-map net-map)
                                  [:rootnodes] conj (:uid subtask-map))))
        (when edge ;; we know it's sequential? here and (pos? i)
          (update-htn-plan-map! edge)
          (update-htn-plan-map!
            (update-in (get-htn-plan-map (get subtask-order (dec i)))
              [:edges] conj (:uid edge))))
        ;; TPN
        (when (zero? i) ;; connect the parent to the beginning
          (tpn/update-tpn-plan-map!
            (update-in
              (tpn/get-tpn-plan-map parent-begin)
              [:activities] conj
              (:uid (tpn/tpn-null-activity {:end-node (:uid begin)})))))
        (when (= i i-last-subtask) ;; connect end to the parent
          (tpn/update-tpn-plan-map!
            (update-in end [:activities] conj
              (:uid (tpn/tpn-null-activity {:end-node (:uid parent-end)})))))
        (when parallel? ;; begin - sb .. se -- end
          (tpn/update-tpn-plan-map!
            (update-in begin [:activities] conj
              (:uid (tpn/tpn-null-activity {:end-node (:uid sb)}))))
          (tpn/update-tpn-plan-map!
            (update-in se [:activities] conj
              (:uid (tpn/tpn-null-activity {:end-node (:uid end)})))))
        (when (and (not parallel?) (pos? i))
          ;; choice or sequence, interstitial na
          (tpn/update-tpn-plan-map!
            (update-in @prev-end [:activities] conj
              (:uid (tpn/tpn-null-activity {:end-node (:uid begin)})))))
        (reset! prev-end end)
        ;; recurse down hem graph
        (doseq [j (range m-task-expansions)]
          (let [task-expansion (get task-expansions j)
                edge-type (if choice? :choice nil)
                se (if (or parallel? (not choice?) (zero? j))
                     se
                     (tpn/tpn-state {}))
                sb (if (or parallel? (not choice?) (zero? j))
                     sb
                     (tpn/tpn-state {:end-node (:uid se)}))
                label (if parallel? nil (str (inc i)))
                hedge (htn-edge {:end-node (:uid task-expansion)
                                 :label label
                                 :edge-type edge-type})]
            (update-htn-plan-map! hedge)
            (update-htn-plan-map!
              (update-in (get-htn-plan-map hem-map)
                [:edges] conj (:uid hedge)))
            (when choice? ;; begin - sb .. se -- end
              (tpn/update-tpn-plan-map!
                (update-in (tpn/get-tpn-plan-map begin) [:activities] conj
                  (:uid (tpn/tpn-null-activity {:end-node (:uid sb)}))))
              (tpn/update-tpn-plan-map!
                (update-in se [:activities] conj
                  (:uid (tpn/tpn-null-activity {:end-node (:uid end)})))))
            (construct-hem-plan-map ir task-expansion subtask false
              (:uid sb))))
        ))
    ))

(defn construct-htn-plan-map [ir expanded-root-task]
  (let [{:keys [uid label]} expanded-root-task
        net (htn-network {:label label :rootnodes #{uid}})
        {:keys [task-expansions irks]} expanded-root-task
        ert (assoc
              (dissoc expanded-root-task :pclass :arguments :task-expansions :name
                :task-type :ancestry-path :temporal-constraints
                :irks)
              :incidence-set #{}
              :edges #{})
        end (tpn/tpn-state {})
        begin (tpn/tpn-state {:end-node (:uid end)})]
    (update-htn-plan-map! ert)
    (update-htn-plan-map! net)
    (swap! *htn-plan-map* assoc :network (:uid net)) ;; ENTRY POINT
    (swap! tpn/*tpn-plan-map* assoc
      :network-id (:uid (tpn/tpn-network
                          {:begin-node (:uid begin)
                           :end-node (:uid end)})))
    ;; now walk the graph
    (if (pos? (count task-expansions))
      (doseq [task-expansion task-expansions]
        (construct-hem-plan-map ir task-expansion ert true (:uid begin))))
    (tpn/optimize-tpn-map)
    @*htn-plan-map*))

;; rt {:type :root-task
;;         :pclass pclass
;;         :method method
;;         :args args}]
;; (assoc ir 'root-task rt)))

;; will return subtasks, if any (and create htn-methods as a side effect)
;; irks are the ks to get-in the ir for the method in question (initially nil)
(defn make-htn-methods [ir pclass mname margs body & [irks]]
  (let [irks (conj (or irks [pclass :methods mname]) :body)
        n (count body)]
    (loop [subtasks [] i 0]
      (if (= i n)
        subtasks
        (let [f (get body i)
              irks-i (conj irks i)
              {:keys [type name field method args
                      primitive body temporal-constraints]} f
              ;; _ (println "MAKE-HTN-METHODS" i "TYPE" type
              ;;     "NAME" name "METHOD" method
              ;;     "MNAME" mname "symbol?" (symbol? mname)
              ;;     "PRIMITIVE" primitive "BODY" body
              ;;     "TC" temporal-constraints)
              subtask
              (cond
                ;; FIXME :plant-fn is WRONG.. is a user-fn
                (= :plant-fn-symbol type)
                (let [make-htn-method? (symbol? mname)
                      plant-class (if (= name 'this) pclass name)
                      plant-fn-primitive?
                      (get-in ir [plant-class :methods method :primitive])
                      non-primitive? (and (= name 'this)
                                       (not plant-fn-primitive?))
                      ;; _ (println "  make-htn-method?" make-htn-method?
                      ;;     "plant-class" plant-class
                      ;;     "plant-fn-primitive?" plant-fn-primitive?
                      ;;     "non-primitive?" non-primitive?)
                      task ((if non-primitive?
                              htn-nonprimitive-task
                              htn-primitive-task)
                            {:pclass plant-class
                             :name method
                             :arguments args
                             :temporal-constraints temporal-constraints
                             :irks irks-i})
                      nt (if make-htn-method?
                           (htn-nonprimitive-task
                             {:pclass pclass :name mname
                              :arguments margs
                              :irks irks-i}))
                      st (if make-htn-method? [task])]
                  ;; (println "  PLANT-FN-SYMBOL TASK" task)
                  ;; (println "  PLANT-FN-SYMBOL SUBTASKS" st)
                  (when make-htn-method? ;; make htn-method
                    (htn-method {:pclass pclass
                                 :name mname
                                 :nonprimitive-task nt
                                 :subtasks st
                                 :irks irks-i}))
                  task)

                ;; if (symbol? mname) we need to make an htn-method
                (= :plant-fn-field type)
                (let [make-htn-method? (symbol? mname)
                      task (htn-primitive-task
                             {:pclass field
                              :name method
                              :arguments args
                              :temporal-constraints temporal-constraints
                              :irks irks-i})
                      nt (if make-htn-method?
                           (htn-nonprimitive-task
                             {:pclass pclass :name mname
                              :arguments margs
                              :irks irks-i}))
                      st (if make-htn-method? [task])]
                  (when make-htn-method?
                    (htn-method {:pclass pclass
                                 :name mname
                                 :nonprimitive-task nt
                                 :subtasks st
                                 :irks irks-i}))
                  task)

                (#{:sequence :parallel} type)
                (let [nonprimitive-task (htn-nonprimitive-task
                                          {:pclass pclass
                                           :name mname
                                           :arguments margs
                                           :temporal-constraints
                                           temporal-constraints
                                           :irks irks-i})
                      subtasks (make-htn-methods ir pclass type nil body irks-i)
                      subtask-constraints (if (= type :sequence)
                                            [(task-sequential-constraint
                                               {:tasks subtasks})]
                                            [])]
                  ;; (println " SEQ SUBTASKS" subtasks)
                  ;; work here
                  (htn-method {:pclass pclass :name mname
                               :nonprimitive-task nonprimitive-task
                               :subtasks subtasks
                               :subtask-constraints subtask-constraints
                               :irks irks-i})
                  nil)

                (= type :choose)
                 ;; shared for all choices
                (let [nonprimitive-task (htn-nonprimitive-task
                                          {:pclass pclass
                                           :name mname
                                           :arguments margs
                                           :temporal-constraints
                                           temporal-constraints
                                           :irks irks-i})
                      m (count body)]
                  (loop [j 0]
                    (if (= j m)
                      nil
                      (let [b (get body j)
                            irks-j (conj (conj irks-i :body) j)
                            {:keys [type body]} b
                            cname (symbol (str mname "-choice-" j))
                            subtasks (make-htn-methods ir pclass
                                       :choice nil body irks-j)]
                        (if (not= type :choice)
                          (log/error "HEY, I EXPECTED a :choice")) ;; FIXME
                        (htn-method {:pclass pclass :name cname
                                     :nonprimitive-task nonprimitive-task
                                     :subtasks subtasks
                                     :irks irks-j})
                        (recur (inc j))))))

                (= type :delay)
                (let [task (htn-primitive-task
                             {:name 'delay
                              :temporal-constraints temporal-constraints
                              :irks irks-i})]
                  task)

                :else
                (let [msg (str "unhandled function type in HTN: " type)
                      task (htn-primitive-task
                             {:name 'error
                              :arguments [msg]
                              :irks irks-i})]
                  (log/error msg)
                  task)
                )
              subtasks (if subtask (conj subtasks subtask) subtasks)]
          (recur subtasks (inc i)))))))

;; RETURNS htn data structure in Clojure (optionally converted to JSON in cli.clj)
;; NOTE: root-task is a string (or nil)!!!
;; Examples
;;   "(isr-htn.main \"A\" \"B\" \"C\" \"D\")"
;;   "(isr-htn.get-data-and-interpret \"A\" \"B\")"
(defn plan-htn
  "Weaves a 'plan' from the root task, using the HTN methods.  Minimally hard-coded."
  [ir root-task file-format output]
  (reinitialize-htn-method-table)
  (reinitialize-htn-object-table)
  (reinitialize-htn-plan-map)
  (tpn/reinitialize-tpn-plan-map)
  (transform-htn ir)
  (let [[pclass method args] (identify-root-task ir root-task)
        nonprimitive-root-task (htn-nonprimitive-task
                                 {:pclass pclass
                                  :name method
                                  :arguments args
                                  :irks [pclass :methods method]})
        expanded-root-task (make-expanded-task nonprimitive-root-task)
        is-stdout? (stdout? output)
        output-prefix (if is-stdout? "STDOUT" output)
        htn-filename (if is-stdout? "-" (str output-prefix ".htn." file-format))
        tpn-filename (if is-stdout? "-" (str output-prefix ".tpn." file-format))
        _ (plan-htn-task expanded-root-task)
        htn (construct-htn-plan-map ir (get-htn-object expanded-root-task))
        tpn @tpn/*tpn-plan-map*]
    (log/info "Saving HTN to" htn-filename "and TPN to" tpn-filename)
    (output-file htn-filename file-format htn)
    (output-file tpn-filename file-format tpn)
    0)) ;; exit code
