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
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            [avenir.utils :refer [concatv assoc-if keywordize vec-index-of]]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [clojure.data.json :as json]
            [instaparse.core :as insta]
            [pamela.parser :as parser]))

;; definitions -----------------------------------------------

;; The following is intended (only) for the support of the EdgeCT program.
;; So, we'll likely move this somewhere else.
(def network-flow-types #{"VOIP" "File Xfer" "File Transfer" "VTC" "VideoStream"})

;; vars

;; {pclass-name {method-name method}}
(def ^{:dynamic true} *htn-methods* (atom {}))

(def ^{:dynamic true} *htn-objects* (atom {}))

(def ^{:dynamic true} *htn-plan-ancestry*
  "Used to keep track of the parents of the current node.  A list, with oldest at the end"
  '())

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

;; TOP LEVEL :network :net-1014790698,
;; default HTN prefix "hid-"
;; :network :htn-network "net-"
;; :edge  "hedge-"
;; :temporal-constraint "cnstr-"
;; TPN's
;; :null-activity :activity :delay-activity "arc-"
;; :state :c/p-begin/end "node-"

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
    :or {prefix "cnstr-"}}]
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
    :or {prefix "cnstr-"}}]
  (update-htn-object!
    (assoc (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
      :type :inter-task-constraint)))

(defn task-sequential-constraint
  "List of tasks that must be executed in sequential order.
   This is shorthand for a set of FS constraints."
  [{:keys [prefix uid ancestry-path tasks]
    :or {prefix "cnstr-"
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
    :or {prefix "hid-"
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
           label incidence-set edges]
    :or {prefix "hid-"}
    :as options}]
  (update-htn-object!
    (assoc (htn-task options)
      :type :htn-primitive-task)))

(defn htn-nonprimitive-task
  "NonPrimitive tasks can be decomposed into other tasks, by user HTN methods"
  [{:keys [prefix uid ancestry-path pclass name arguments cost task-type probability temporal-constraints
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight
           label incidence-set edges]
    :or {prefix "hid-"}
    :as options}]
  (update-htn-object!
    (assoc (htn-task options)
      :type :htn-nonprimitive-task)))

(defn htn-expanded-nonprimitive-task
  "Expanded NonPrimitive tasks have already been decomposed into other tasks, by using HTN methods"
  [{:keys [prefix uid ancestry-path pclass name arguments cost task-type probability temporal-constraints
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight task-expansions
           label incidence-set edges]
    :or {prefix "hid-"
         task-expansions []}
    :as options}]
  (update-htn-object!
    (assoc-if (htn-nonprimitive-task options)
      :type :htn-expanded-nonprimitive-task
      :task-expansions task-expansions)))

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
           label incidence-set network edges]
    :or {prefix "hid-"
         subtasks []
         subtask-constraints []
         task-expansions []}}]
  (let [method (update-htn-object!
                 (assoc-if (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
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
                   :label label
                   :incidence-set incidence-set ;; NOTE should be a clojure set
                   :edges edges
                   :network network))]
    (add-htn-method method)
    method))

(defn htn-network
  "HTN network"
  [{:keys [prefix uid ancestry-path label rootnodes parentid]
    :or {prefix "net-"
         rootnodes []}}]
  (update-htn-object!
    (assoc-if (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
      :label label
      :rootnodes rootnodes
      :parentid parentid)))

(defn htn-edge
  "HTN edge"
  [{:keys [prefix uid ancestry-path label edge-type] ;; edge-type is usually only :choice (if not nil)
    :or {prefix "hedge-"
         rootnodes []}}]
  (update-htn-object!
    (assoc-if (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
      :label label
      :edge-type edge-type)))

(declare copy-constraint-into-expansion)

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
           label incidence-set network]
    :or {prefix "hid-"
         subtasks []
         subtask-constraints []}}]
  (let [orig-method expansion-method
        orig-subtasks (:subtasks orig-method)
        new-subtasks (:subtasks expansion-method)
        subtask-constraints (map
                              #(copy-constraint-into-expansion %
                                 orig-method orig-subtasks new-subtasks)
                              (:subtask-constraints orig-method))
        hem   (update-htn-object!
                (assoc-if (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
                  :type :htn-expanded-method
                  :expansion-method expansion-method
                  :argument-mappings argument-mappings
                  :subtasks (vec subtasks)
                  :subtask-constraints (vec subtask-constraints)
                  :label label
                  :incidence-set incidence-set ;; NOTE should be a clojure set
                  :network network))]
    hem))

;; name-with-args multi-methods --------------------------------------

;; FUTURE: add pclass to name-with-args

(def name-with-args-max-line-length
  "The (preferred) maximimum line length for Task/Method Names including arguments"
  25)

(def name-with-args-include-args?
  "Whether to include argument lists when displaying names of Tasks and Methods"
  true)

(defmulti name-with-args
  "Dispatch based on object type"
  (fn [object & [max-line-length]] (:type object))
  :default nil)

(defmethod name-with-args nil
  [object & [max-line-length]]
  "")

(defmethod name-with-args :htn-task
  [object & [max-line-length]]
  (let [{:keys [name arguments]} object]
    (add-newlines-if-needed
      (str name (if name-with-args-include-args? (or arguments '())))
      (or max-line-length name-with-args-max-line-length))))

(defmethod name-with-args :htn-method
  [object & [max-line-length]]
  (let [{:keys [name nonprimitive-task]} object
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
  [object & [max-line-length]]
  (let [{:keys [expansion-method argument-mappings]} object
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


;; print-object multi-methods --------------------------------------

(defmulti print-object
  "Dispatch based on object type"
  (fn [object] (:type object))
  :default nil)

(defmethod print-object nil
  [object]
  "")

(defmethod print-object :htn-task
  [object]
  (name-with-args object))

(defmethod print-object :htn-method
  [object]
  (name-with-args object))

(defmethod print-object :htn-expanded-method
  [object]
  (name-with-args object))

;; children-have-resources? multi-methods --------------------------------------

(defmulti children-have-resources?
  "Dispatch based on object type"
  (fn [object] (:type object))
  :default nil)

(defmethod children-have-resources? nil
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

(defmulti copy-constraint-into-expansion
  "Dispatch based on object type"
  (fn [object orig-method orig-subtasks new-subtasks] (:type object))
  :default nil)

(defmethod copy-constraint-into-expansion nil
  [object orig-method orig-subtasks new-subtasks]
  nil)

;; NOTE: must ensure :tasks and :subtasks slots are vectors
(defmethod copy-constraint-into-expansion :task-sequential-constraint
  [object orig-method orig-subtasks new-subtasks]
  (let [tasks (map #(get new-subtasks (vec-index-of orig-subtasks %))
                (:tasks object))
        constraint (task-sequential-constraint {:tasks tasks})]
    constraint))

;; plan-htn-task multi-methods --------------------------------------

(defmulti plan-htn-task
  "Dispatch based on object type"
  (fn [object] (:type object))
  :default nil)

(defmethod plan-htn-task nil
  [object]
  (throw
    (AssertionError.
      (str "cannot plan-htn-task for object type: " (:type object)))))

;; "Nothing to do for primitive tasks"
(defmethod plan-htn-task :htn-primitive-task
  [object])

(declare find-methods-that-expand-task)
(declare arg-mappings-for-task-and-method)

;;   "Expand"
(defmethod plan-htn-task :htn-expanded-nonprimitive-task
  [object]
  (let [task object
        methods (find-methods-that-expand-task task)]
    (binding [*htn-plan-ancestry* (cons task *htn-plan-ancestry*)]
      (println "PLAN-HTN-TASK w/" (count methods) "methods @" *htn-plan-ancestry*)
      (println "TASK" task)
      (doseq [method methods]
        (let [{:keys [pclass name subtasks]} method]
          (println "METHOD" name)
          (let [arg-mappings (arg-mappings-for-task-and-method task method)
                _ (println "  ARG-MAPPINGS" (pr-str arg-mappings))
                _ (println "  METHOD")
                _ (pprint method)
                subtasks (mapv #(make-expanded-task % arg-mappings) subtasks)
                _ (println "  SUBTASKS")
                _ (pprint subtasks)
                expanded-method (htn-expanded-method
                                  {:expansion-method method
                                   :argument-mappings arg-mappings
                                   :ancestry-path *htn-plan-ancestry*
                                   :subtasks subtasks})
                ;; conj onto :task-expansions of task
                task-expansions (conj (:task-expansions (get-htn-object task))
                                  expanded-method)
                task (assoc task :task-expansions task-expansions)]
            (update-htn-object! task)
            (println "EXPANSION BEGIN --------------------")
            (pprint expanded-method)
            (println "EXPANSION end --------------------")
            )))
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
              (update-htn-object! subtask)
              )
            )
          )
        )
      )))

;; Expansion -------------------------------------------------

(defn copy-temporal-constraints [from-temporal-constraints]
  (mapv
    #(temporal-constraint (dissoc % :uid))
    from-temporal-constraints))

;; NOTE assumes old-task is :htn-primitive-task or :htn-nonprimitive-task
(defn make-expanded-task
  "Make an expanded task"
  [old-task & [argument-mapping]]
  (let [task-type (:type old-task)
        old-task (dissoc old-task :uid)
        task (if (= task-type :htn-primitive-task)
               (htn-primitive-task old-task)
               (htn-expanded-nonprimitive-task old-task))
        {:keys [temporal-constraints arguments]} task
        new-arguments (if argument-mapping
                        (mapv #(get argument-mapping %) arguments)
                        arguments)]
    (println "MET arguments:" arguments)
    (println "MET new-arguments:" new-arguments)
    (assoc-if task
      :arguments arguments
      :temporal-constraints (and temporal-constraints
                              (copy-temporal-constraints temporal-constraints))
      )))

;; return methods vector
(defn find-methods-that-expand-task [task]
  (let [{:keys [type pclass name arguments]} task
        all-methods (seq (get @*htn-methods* pclass))]
    ;; DAN.. CLOSism: was (when (= type :htn-nonprimitive-task)
    (when (#{:htn-nonprimitive-task :htn-expanded-nonprimitive-task} type)
      (loop [methods [] [mname method] (first all-methods) more (rest all-methods)]
        (println "METHOD EXPANDS?" mname)
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
                _ (println "..." match?)
                methods (if match? (conj methods method) methods)]
            (recur methods (first more) (rest more))))))))

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
  (log/warn "root-task" root-task) ;; DEBUG or trace
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

;; RETURNS htn data structure in Clojure (optionally converted to JSON in cli.clj)
;; NOTE: root-task is a string (or nil)!!!
;; Examples
;;   "(isr-htn.main \"A\" \"B\" \"C\" \"D\")"
;;   "(isr-htn.get-data-and-interpret \"A\" \"B\")"
(defn plan-htn
  "Weaves a 'plan' from the root task, using the HTN methods.  Minimally hard-coded."
  [ir root-task]
  (reinitialize-htn-method-table)
  (reinitialize-htn-object-table)
  (transform-htn ir)
  (let [[pclass method args] (identify-root-task ir root-task)
        nonprimitive-root-task (htn-nonprimitive-task
                                 {:pclass pclass
                                  :name method
                                  :arguments args})
        expanded-root-task (make-expanded-task nonprimitive-root-task)]
    (plan-htn-task expanded-root-task)
    (println "PLAN-HTN COMPLETE")
    expanded-root-task))

;;     rt {:type :root-task
;;         :pclass pclass
;;         :method method
;;         :args args}]
;; (assoc ir 'root-task rt)))


;; will return subtasks, if any (and create htn-methods as a side effect)
(defn make-htn-methods [pclass mname margs body]
  (println "make-htn-methods for" pclass mname margs)
  (loop [subtasks [] f (first body) more (rest body)]
    (if-not f
      subtasks
      (let [{:keys [type name field method args body temporal-constraints]} f
            _ (println "SUBTASK" type "NAME" name "METHOD" method "BODY size" (count body))
            subtask (cond
                      ;; (#{:plant-fn-symbol :plant-fn-field} type)
                      ;; (htn-primitive-task {:pclass pclass :name method :arguments args})
                      ;; FIXME :plant-fn is WRONG.. is a user-fn

                      ;; if (symbol? mname) we need to make an htn-method
                      (= :plant-fn-symbol type)
                      (let [task
                            (if (= name 'this)
                              (htn-nonprimitive-task {:pclass pclass
                                                      :name method
                                                      :arguments args
                                                      :temporal-constraints temporal-constraints})
                              (htn-primitive-task {:pclass name
                                                   :name method
                                                   :arguments args
                                                   :temporal-constraints temporal-constraints}))
                            nt (if (symbol? mname)
                                 (htn-nonprimitive-task
                                   {:pclass pclass :name mname
                                    :arguments margs}))
                            st (if (symbol? mname) [task])]
                        (when (symbol? mname) ;; make htn-method
                          (htn-method {:pclass pclass
                                       :name mname
                                       :nonprimitive-task nt
                                       :subtasks st}))
                        task)

                      ;; if (symbol? mname) we need to make an htn-method
                      (= :plant-fn-field type)
                      (let [task (htn-primitive-task {:pclass field
                                                      :name method
                                                      :arguments args
                                                      :temporal-constraints temporal-constraints})
                            nt (if (symbol? mname)
                                 (htn-nonprimitive-task
                                   {:pclass pclass :name mname
                                    :arguments margs}))
                            st (if (symbol? mname) [task])]
                        (when (symbol? mname) ;; make htn-method
                          (htn-method {:pclass pclass
                                       :name mname
                                       :nonprimitive-task nt
                                       :subtasks st}))
                        task)

                      (#{:sequence :parallel} type)
                      (let [nonprimitive-task (htn-nonprimitive-task
                                                {:pclass pclass
                                                 :name mname
                                                 :arguments margs
                                                 :temporal-constraints temporal-constraints})
                            subtasks (make-htn-methods pclass type nil body)
                            subtask-constraints (if (= type :sequence)
                                                  [(task-sequential-constraint {:tasks subtasks})]
                                                  [])]
                        (println "  method:" mname "is an htn-method")
                        ;; work here
                        (htn-method {:pclass pclass :name mname
                                     :nonprimitive-task nonprimitive-task
                                     :subtasks subtasks
                                     :subtask-constraints subtask-constraints})
                        nil)

                      (= type :choose)
                      (let [nonprimitive-task (htn-nonprimitive-task ;; shared for all choices
                                                {:pclass pclass
                                                 :name mname
                                                 :arguments margs
                                                 :temporal-constraints temporal-constraints})]
                        (loop [i 0 b (first body) moar (rest body)]
                          (if-not b
                            nil
                            (let [{:keys [type body]} b
                                  cname (symbol (str mname "-choice-" i))
                                  subtasks (make-htn-methods pclass :choice nil body)]
                              (println "  method:" cname "is an htn-method choice" i)
                              (if (not= type :choice)
                                (println "HEY, I EXPECTED a :choice")) ;; FIXME
                              (htn-method {:pclass pclass :name cname
                                           :nonprimitive-task nonprimitive-task
                                           :subtasks subtasks})
                              (recur (inc i) (first moar) (rest moar))))))

                      :else
                      :TBD)
            subtasks (if subtask (conj subtasks subtask) subtasks)]
        (recur subtasks (first more) (rest more))
        )
      )
    )
  )

;; walk ir, look for pclasses that have non-empty methods
(defn transform-htn [ir]
  ;; (pprint ir)
  ;; (println "transform-htn")
  (let [ir-syms (seq ir)]
    (loop [[k v] (first ir-syms) more (rest ir-syms)]
      (if-not k
        nil ;; (println "done")
        (let [{:keys [type methods]} v]
          (println "key:" k "is a" type)
          (when (and (= type :pclass) methods)
            (println "  there are" (count methods) "methods")
            (let [method-syms (seq methods)]
              ;; (println "METHOD-SYMS" method-syms)
              (loop [[mname method] (first method-syms) moar (rest method-syms)]
                (println "METHOD NAME" mname)
                (if-not mname
                  (println "no more methods")
                  (let [{:keys [temporal-constraints args body]} method]
                    (if body
                      (make-htn-methods k mname args body)
                      (println "  METHOD:" mname "is NOT an htn-method"))
                    (recur (first moar) (rest moar))
                    )
                  )
                )
              )
            )
          (recur (first more) (rest more))
          )
        )
      )
    (println "HTN-METHODS BEGIN -------------------")
    (pprint @*htn-methods*)
    (println "HTN-METHODS END -------------------")
    )
  )

;; NOTE: root-task is a string (or nil)!!!
(defn isr-test []
  (let [htn "/home/tmarble/src/lispmachine/pamela/notes/language-examples/isr-htn.pamela"
        ir (parser/parse {:input [htn]})
        root-task "(isr-htn.main \"A\" \"B\" \"C\" \"D\")"]
    (plan-htn ir root-task)
    (println "HTN-OBJECTS -------------------")
    (pprint @*htn-objects*)
    ))
