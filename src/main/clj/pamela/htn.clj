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
  (:require [clojure.java.io :refer :all] ;; for as-file
            ;; [clojure.data :refer [diff]]
            ;; [clojure.java.shell :refer [sh]]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            ;; [clojure.walk :refer [prewalk postwalk]]
            [avenir.utils :refer [concatv assoc-if keywordize]]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [clojure.data.json :as json]
            [instaparse.core :as insta]
            [pamela.parser :as parser]
            ))

;; definitions

;; The following is intended (only) for the support of the EdgeCT program.
;; So, we'll likely move this somewhere else.
(def network-flow-types #{"VOIP" "File Xfer" "File Transfer" "VTC" "VideoStream"})

;; vars

(def *htn-methods* (atom {}))

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

;; FIXME
(defn copy-temporal-constraint
  [temporal-constraint]
  temporal-constraint)

;; HTN Object hierarchy ------------------------------------

(defn htn-object
  "All HTN objects inherit from htn-object"
  [{:keys [uid]}]
  (let [uid (or uid (name (gensym "hid-")))]
    {:type :htn-object
     :uid uid
     :ancestry-path []}))

(defn temporal-constraint
  "A task-local temporal constraint for that task."
  [{:keys [uid lb ub
           start-delay ;; Wait for this long (sec) before starting this task (included in lb/ub time)
           start-time
           end-time]}]
  (assoc (htn-object {:uid uid})
    :lb lb
    :ub ub
    :start-delay start-delay
    :start-time start-time
    :end-time end-time))

(defn htn-task
  "All HTN tasks inherit from HTN-task"
  [{:keys [uid name arguments cost task-type probability temporal-constraint
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight]
    :or {task-type :communications}}]
  (let [network-flows (if (network-flow-types name)
                        (make-network-flows name flow-characteristics arguments)
                        network-flows)]
    (assoc (htn-object {:uid uid})
      :type :htn-task
      :name name
      :arguments arguments
      :cost cost
      :task-type task-type
      :probability probability
      :temporal-constraint temporal-constraint ;; CONSIDER converting list-> temp cons objects?
      :mission-effectiveness mission-effectiveness
      :priority priority
      :resources resources
      :network-flows network-flows
      :flow-characteristics flow-characteristics
      :mission-task-weight mission-task-weight)))

(defn htn-primitive-task
  "Primitive tasks can't be decomposed any further"
  [{:keys [uid name arguments cost task-type probability temporal-constraint
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight]
    :as options}]
  (assoc (htn-task options)
    :type :htn-primitive-task))

(defn htn-nonprimitive-task
  "NonPrimitive tasks can be decomposed into other tasks, by user HTN methods"
  [{:keys [uid name arguments cost task-type probability temporal-constraint
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight]
    :as options}]
  (assoc (htn-task options)
    :type :htn-nonprimitive-task))

(defn htn-expanded-nonprimitive-task
  "Expanded NonPrimitive tasks have already been decomposed into other tasks, by using HTN methods"
  [{:keys [uid name arguments cost task-type probability temporal-constraint
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight task-expansions]
    :as options}]
  (assoc (htn-nonprimitive-task options)
    :type :htn-expanded-nonprimitive-task
    :task-expansions task-expansions))

(defn htn-method
  "An HTN-method is used to decompose a nonprimitive-task into one or more subtasks"
  [{:keys [name
           nonprimitive-task ;;This is the HTN-nonprimitive-task that this method matches on
           preconditions
           subtasks ;;This is a simple list of the tasks
           subtask-constraints ;;A list of constraints, expressing all of the partial orders
           mission-task-combination
           choice-weight]}]
  (let [method {:type :htn-method
                :name name
                :nonprimitive-task nonprimitive-task
                :preconditions preconditions
                :subtasks subtasks
                :subtask-constraints subtask-constraints
                :mission-task-combination mission-task-combination
                :choice-weight choice-weight}]
    (when name ;; add to known methods
      (swap! *htn-methods* assoc name method))
    method))

(defn htn-expanded-method
  "This captures the results of applying the HTN-method to a nonprimitive task."
  [{:keys [expansion-method
           argument-mappings
           subtasks
           subtask-constraints]}]
  {:type :htn-expanded-method
   :expansion-method expansion-method
   :argument-mappings argument-mappings
   :subtasks subtasks
   :subtask-constraints subtask-constraints})

;; name-with-args multi-methods --------------------------------------

(def name-with-args-max-line-length
  "The (preferred) maximimum line length for Task/Method Names including arguments"
  25)

(def name-with-args-include-args?
  "Whether to include argument lists when displaying names of Tasks and Methods"
  true)

(defmulti name-with-args
  "Dispatch based on task type"
  (fn [task & [max-line-length]] (:type task))
  :default nil)

(defmethod name-with-args nil
  [task & [max-line-length]]
  "")

(defmethod name-with-args :htn-task
  [task & [max-line-length]]
  (let [{:keys [name arguments]} task]
    (add-newlines-if-needed
      (str name (if name-with-args-include-args? (or arguments '())))
      (or max-line-length name-with-args-max-line-length))))

(defmethod name-with-args :htn-method
  [task & [max-line-length]]
  (let [{:keys [name nonprimitive-task]} task
        method-name name
        {:keys [name arguments]} nonprimitive-task]
    (str
      (add-newlines-if-needed
        method-name
        (or max-line-length name-with-args-max-line-length))
      (if max-line-length "\n" " - ")
      (add-newlines-if-needed
        (str name (if name-with-args-include-args? (or arguments '())))
        (or max-line-length name-with-args-max-line-length)))))

(defmethod name-with-args :htn-expanded-method
  [task & [max-line-length]]
  (let [{:keys [expansion-method argument-mappings]} task
        {:keys [name nonprimitive-task]} expansion-method
        method-name name
        {:keys [name arguments]} nonprimitive-task
        argvals (map #(get argument-mappings %) arguments)]
    (str
      (add-newlines-if-needed
        method-name
        (or max-line-length name-with-args-max-line-length))
      (add-newlines-if-needed
        (str name (if name-with-args-include-args? (or argvals '())))
        (or max-line-length name-with-args-max-line-length)))))


;; print-task multi-methods --------------------------------------

(defmulti print-task
  "Dispatch based on task type"
  (fn [task] (:type task))
  :default nil)

(defmethod print-task nil
  [task]
  "")

(defmethod print-task :htn-task
  [task]
  (name-with-args task))

(defmethod print-task :htn-method
  [task]
  (name-with-args task))

(defmethod print-task :htn-expanded-method
  [task]
  (name-with-args task))

;; children-have-resources? multi-methods --------------------------------------

(defmulti children-have-resources?
  "Dispatch based on task type"
  (fn [task] (:type task))
  :default nil)

(defmethod children-have-resources? nil
  [task]
  false)

(defmethod children-have-resources? :htn-expanded-nonprimitive-task
  [task]
  (some #(children-have-resources? %) (:task-expansions task)))

(defmethod children-have-resources? :htn-expanded-method
  [task]
  (some #(or (:resources %) (children-have-resources? %))
    (:subtasks task)))

;; Expansion -------------------------------------------------

;; NOTE assumes old-task is :htn-primitive-task or :htn-nonprimitive-task
(defn make-expanded-task
  "Make an expanded task"
  [old-task & [argument-mapping]]
  (let [task-type (:type old-task)
        task (if (= task-type :htn-primitive-task)
               (htn-primitive-task old-task)
               (htn-nonprimitive-task old-task))
        {:keys [temporal-constraint arguments]} task]
    (assoc task
      :type (if (= task-type :htn-primitive-task)
              task-type
              :htn-expanded-nonprimitive-task)
      :arguments (if argument-mapping
                   (mapv #(get argument-mapping (keyword %)) arguments)
                   arguments)
      :temporal-constraint (and temporal-constraint (copy-temporal-constraint temporal-constraint))
      )))

(defn find-htn-method [name & [error-if-not-found?]]
  (let [method (get @*htn-methods* name)]
    (when (and (not method) error-if-not-found?)
      (throw (AssertionError. (str "method not found: " name))))
    method))

(defn find-methods-that-expand-task [task]
  (let [{:keys [type name arguments]} task
        all-methods (seq @*htn-methods*)]
    (when (= type :htn-nonprimitive-task)
      (loop [methods [] mname-method (first all-methods) more (rest all-methods)]
        (if-not mname-method
          methods
          (let [[mname method] mname-method
                {:keys [nonprimitive-task]} method
                compatible? (fn [task-arg-method-arg]
                              (let [[task-arg method-arg] task-arg-method-arg]
                                (or (variable? task-arg)
                                  (variable? method-arg)
                                  (= task-arg method-arg))))
                match? (and (= name (:name nonprimitive-task))
                         (= (count arguments) (count (:arguments nonprimitive-task)))
                         (every? compatible?
                           (map vector arguments (:arguments nonprimitive-task))))
                methods (if match? (conj methods method) methods)]
            (recur methods (first more) (rest more))))))))

(defn reinitialize-htn-method-table []
  (reset! *htn-methods* {}))


;; (defmethod computed-duration ((constraint temporal-constraint))
;;   (with-slots (start-time end-time) constraint
;;     (and start-time
;;          end-time
;;          (- end-time start-time))))

;; (defmethod lb ((constraint temporal-constraint))
;;   (with-slots (lb) constraint
;;     (or lb (computed-duration constraint) 0)))

;; (defmethod ub ((constraint temporal-constraint))
;;   (with-slots (ub) constraint
;;     (or ub (computed-duration constraint) :infinity)))

;; (defmethod contains-non-default-values-p ((constraint temporal-constraint))
;;   (with-slots (lb ub start-delay start-time end-time) constraint
;;     (or (and lb
;;              (not (zerop lb)))
;;         (and ub
;;              (not (eq ub :infinity)))
;;         start-delay
;;         start-time
;;         end-time)))

;; (defmethod copy-temporal-constraint ((constraint temporal-constraint))
;;   (make-instance 'temporal-constraint
;;                  :lb (lb constraint) :ub (ub constraint)
;;                  :start-delay (start-delay constraint)
;;                  :start-time (start-time constraint)
;;                  :end-time (end-time constraint)))

;; (defmethod make-temporal-constraint-for-sequential-subtasks ((constraint temporal-constraint))
;;   (make-instance 'temporal-constraint
;;                  :lb 0 :ub (ub constraint)
;;                  :start-delay (start-delay constraint)
;;                  :start-time (start-time constraint)
;;                  :end-time (end-time constraint)))

;; (defclass inter-task-constraint (HTN-object)
;;   ()
;;   )

;; (defclass task-sequential-constraint (inter-task-constraint)
;;   ((tasks :accessor tasks :initarg :tasks :initform nil))
;;   (:documentation "List of tasks that must be executed in sequential order.  This is shorthand for a set of FS constraints."))

;; ;; NOT USED - Note: If there are no constraints, the unconstrained tasks are assumed to be parallel
;; (defclass task-parallel-constraint (inter-task-constraint)
;;   ((tasks :accessor tasks :initarg :tasks :initform nil))
;;   (:documentation "List of tasks that can be executed in parallel order"))

;; ;; NOT USED - Note: if there are multiple method expansions of the same task, the alternatives are considered 'choices'
;; (defclass task-choice-constraint (inter-task-constraint)
;;   ((tasks :accessor tasks :initarg :tasks :initform nil))
;;   (:documentation "List of tasks, for which only one must be executed (e.g., lowest-cost task that meets the constraints"))

;; ;; A set of common inter-task dependencies:
;; ;; FS - Finish to Start
;; ;; FF - Finish to Finish
;; ;; SS - Start to Start
;; ;; SF - Start to Finish

;; ;; NOT USED - Note: task-sequential-constraint provides the same capability (and more)
;; (defclass task-precursor-successor-constraint (inter-task-constraint)
;;   ((precursor-task :accessor precursor-task :initarg :precursor-task :initform nil)
;;    (successor-task :accessor successor-task :initarg :successor-task :initform nil))
;;   (:documentation "Precursor task must finish before the Successor task starts. (FS)"))


;; ;; There will be a 1:1 mapping between task-expansions and methods
;; (defclass HTN-expanded-method (HTN-object)
;;   ((expansion-method :accessor expansion-method :initarg :expansion-method :initform nil)
;;    (argument-mappings :accessor argument-mappings :initarg :argument-mappings :initform nil)
;;    (subtasks :accessor expansion-subtasks :initarg :subtasks :initform nil)
;;    (subtask-constraints :accessor expansion-subtask-constraints :initarg :subtask-constraints :initform nil))
;;   (:documentation "This captures the results of applying the HTN-method to a nonprimitive task."))

;; (defmethod initialize-instance :after ((method htn-expanded-method) &rest args)
;;   (declare (ignore args))
;;   (let* ((orig-method (expansion-method method))
;; 	 (orig-subtasks (method-subtasks orig-method))
;; 	 (new-subtasks (expansion-subtasks method)))
;;     (setf (expansion-subtask-constraints method)
;; 	  (mapcar #'(lambda (constraint) (copy-constraint-into-expansion constraint orig-method orig-subtasks new-subtasks))
;; 		   (method-subtask-constraints orig-method)))))

;; (defmethod children-have-resources-p ((method htn-expanded-method))
;;   (some #'(lambda (task)
;;             (or (task-resources task)
;;                 (children-have-resources-p task)))
;;         (expansion-subtasks method)))

;; (defmethod copy-constraint-into-expansion ((orig-constraint null) orig-method orig-subtasks new-subtasks)
;;   (declare (ignore orig-method orig-subtasks new-subtasks))
;;   nil)

;; (defmethod copy-constraint-into-expansion ((orig-constraint task-sequential-constraint) orig-method orig-subtasks new-subtasks)
;;   (declare (ignore orig-method))
;;   (let ((constraint (make-instance 'task-sequential-constraint
;; 				    :tasks (mapcar #'(lambda (x) (elt new-subtasks (position  x orig-subtasks)))
;; 						   (tasks orig-constraint)))))
;;     constraint))

;; (defmethod copy-constraint-into-expansion ((orig-constraint task-parallel-constraint) orig-method orig-subtasks new-subtasks)
;;   (declare (ignore orig-method))
;;   (let ((constraint (make-instance 'task-parallel-constraint
;; 				    :tasks (mapcar #'(lambda (x) (elt new-subtasks (position  x orig-subtasks)))
;; 						   (tasks orig-constraint)))))
;;     constraint))

;; (defmethod copy-constraint-into-expansion ((orig-constraint task-choice-constraint) orig-method orig-subtasks new-subtasks)
;;   (declare (ignore orig-method))
;;   (let ((constraint (make-instance 'task-choice-constraint
;; 				    :tasks (mapcar #'(lambda (x) (elt new-subtasks (position  x orig-subtasks)))
;; 						   (tasks orig-constraint)))))
;;     constraint))

;; (defmethod copy-constraint-into-expansion ((orig-constraint task-precursor-successor-constraint) orig-method orig-subtasks new-subtasks)
;;   (let ((constraint (make-instance 'task-precursor-successor-constraint
;; 				    :precursor-task (elt new-subtasks (position (precursor-task orig-method) orig-subtasks))
;; 				    :successor-task (elt new-subtasks (position (successor-task orig-method) orig-subtasks)))))
;;     constraint))


;; (defmethod print-object ((exp-method HTN-expanded-method) stream)
;;   (print-unreadable-object (exp-method stream :type t :identity t)
;;     (format stream "~A" (name-with-args exp-method))))

;; (defmethod name-with-args ((exp-method htn-expanded-method) &optional max-line-length)
;;   (let ((static-method (expansion-method exp-method)))
;;     (format nil (if max-line-length "~A~%~A" "~A - ~A")
;;             (add-newlines-if-needed (method-name static-method) max-line-length)
;;             (add-newlines-if-needed
;;              (format nil "~A~A"
;;                      (task-name (nonprimitive-task static-method))
;;                      (if *name-with-args-include-args-p*
;;                          (or (mapcar #'(lambda (arg)
;;                                          (or (cdr (assoc arg (argument-mappings exp-method) :test #'equal)) arg))
;;                                      (task-arguments (nonprimitive-task static-method)))
;;                              "()")
;;                          ""))
;;              max-line-length))))

;; -----------------------------------------------------------------------

;; (in-package :htn)

;; (defvar *htn-plan-ancestry* nil
;;   "Used to keep track of the parents of the current node.  A list, with oldest at the end")

;; (defun plan-htn (&optional unexpanded-root-task)
;;   "Weaves a 'plan' from the root task, using the HTN methods.  Minimally hard-coded."
;;     (let* ((root-task (make-expanded-task (or unexpanded-root-task
;;                                               *parent-unexpanded-task*)))
;;            (*htn-plan-ancestry* nil))
;;       (plan-htn-task root-task)
;;       root-task))

;; (defmethod plan-htn-task ((task htn-primitive-task))
;;   "Nothing to do for primitive tasks"
;;   nil)

;; (defmethod plan-htn-task ((task htn-nonprimitive-task))
;;   (error "Attempt to expand a non-expandable task"))

;; (defun arg-mappings-for-task-and-method (task method)
;;   (let ((static-task-args (task-arguments (nonprimitive-task method)))
;; 	(dynamic-task-args (task-arguments task))
;; 	(arg-mappings '()))
;;     (assert (= (length static-task-args)
;; 	       (length dynamic-task-args)))
;;     ;; Here, we're essentially doing a multiple-list-DOLIST
;;     (do ((static-cons static-task-args (cdr static-cons))
;; 	 (dynamic-cons dynamic-task-args (cdr dynamic-cons)))
;; 	((null static-cons))
;;       (when (variable-p (car static-cons))
;;         (push (cons (car static-cons)
;;                     (car dynamic-cons))
;;               arg-mappings)))
;;     arg-mappings))

;; (defmethod plan-htn-task ((task htn-expanded-nonprimitive-task))
;;   (declare (special *prune-tasks-without-resources-p*))
;;   (let ((methods (find-methods-that-expand-task task))
;;         (*htn-plan-ancestry* (cons task *htn-plan-ancestry*)))
;;     (dolist (method methods)
;;       (let ((arg-mappings (arg-mappings-for-task-and-method task method)))
;; 	(push (make-instance 'htn-expanded-method
;; 			     :expansion-method method
;; 			     :argument-mappings arg-mappings
;;                              :ancestry-path *htn-plan-ancestry*
;; 			     :subtasks (mapcar #'(lambda (subtask)
;; 						   (make-expanded-task subtask arg-mappings))
;; 					       (method-subtasks method)))
;; 	      (task-expansions task))))
;;     ;; TODO: How do we allocate resource attributes (like Cost) from the parent to the child tasks?
;;     ;; Initially: if there is only one child task, give it the same values.
;;     (dolist (expansion (task-expansions task))
;;       (cond ((= 1 (length (expansion-subtasks expansion)))
;; 	     (let ((child-task (first (expansion-subtasks expansion))))
;; 	       (if (null (task-cost child-task))
;; 		   (setf (task-cost child-task) (task-cost task)))
;;                (if (null (task-type child-task))
;; 		   (setf (task-type child-task) (task-type task)))
;; 	       (if (null (task-probability child-task))
;; 		   (setf (task-probability child-task) (task-probability task)))
;;                #+ignore ;;More of a hindrance than a help
;; 	       (if (null (temporal-constraint child-task))
;; 		   (setf (temporal-constraint child-task) (temporal-constraint task)))
;; 	       (if (null (mission-effectiveness child-task))
;; 		   (setf (mission-effectiveness child-task) (mission-effectiveness task)))
;; 	       (if (null (task-priority child-task))
;; 		   (setf (task-priority child-task) (task-priority task)))
;;                (if (and (not *prune-tasks-without-resources-p*)
;;                         (null (task-resources child-task)))
;; 		   (setf (task-resources child-task) (task-resources task)))))
;; 	    (t
;;              ;;TODO: What about COST?
;;              (dolist (child-task (expansion-subtasks expansion))
;;                (if (null (task-type child-task))
;; 		   (setf (task-type child-task) (task-type task)))
;; 	       (if (null (task-probability child-task))
;; 		   (setf (task-probability child-task) (task-probability task)))
;;                #+ignore ;;More of a hindrance than a help
;; 	       (if (null (temporal-constraint child-task))
;; 		   (setf (temporal-constraint child-task) (temporal-constraint task)))
;; 	       (if (null (mission-effectiveness child-task))
;; 		   (setf (mission-effectiveness child-task) (mission-effectiveness task)))
;; 	       (if (null (task-priority child-task))
;; 		   (setf (task-priority child-task) (task-priority task)))
;;                (if (and (not *prune-tasks-without-resources-p*)
;;                         (null (task-resources child-task)))
;; 		   (setf (task-resources child-task) (task-resources task))))
;;              )))
;;     ;; Be sure to propogate the resource attributes (see above) before planning those subtasks (see below)
;;     (dolist (expansion-method (task-expansions task))
;;       (let ((*htn-plan-ancestry* (cons expansion-method *htn-plan-ancestry*)))
;;         (dolist (subtask (expansion-subtasks expansion-method))
;;           (setf (ancestry-path subtask) *htn-plan-ancestry*)
;;           (plan-htn-task subtask))))
;;     (and methods (task-expansions task))))

;; -------------------------------------------------------------------------

(defn ir-root-task [& args]
  args)

(def root-task-ir {
                   :argval identity
                   :boolean parser/ir-boolean
                   ;; :float handled in :ir-number
                   :integer parser/ir-integer
                   :keyword keyword
                   :natural parser/ir-integer
                   :number parser/ir-number
                   :root-task ir-root-task
                   :safe-keyword identity
                   :string identity
                   :symbol symbol
                   })

;; return [pclass method args]
(defn identify-root-task [ir root-task]
  (log/warn "ID RT" root-task)
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
;;
;;
;; RETURNS htn data structure in Clojure (optionally converted to JSON in cli.clj)
(defn plan-htn [ir root-task]
  (let [[pclass method args] (identify-root-task ir root-task)
        ]
    ))
;;     rt {:type :root-task
;;         :pclass pclass
;;         :method method
;;         :args args}]
;; (assoc ir 'root-task rt)))
