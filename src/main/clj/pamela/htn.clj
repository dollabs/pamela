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

;; default HTN prefix "hid-"
;; :network :htn-network "net-"
;; :edge  "hedge-"
;; :temporal-constraint "cnstr-"
;; TPN's
;; :null-activity :activity :delay-activity "arc-"
;; :state :c/p-begin/end "node-"
;;
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
    :type :temporal-constraint
    :lb lb
    :ub ub
    :start-delay start-delay
    :start-time start-time
    :end-time end-time))

(defn inter-task-constraint
  "inter-task-constraint"
  [{:keys [uid]}]
  (assoc (htn-object {:uid uid})
    :type :inter-task-constraint))

(defn task-sequential-constraint
  "List of tasks that must be executed in sequential order.
   This is shorthand for a set of FS constraints."
  [{:keys [uid tasks]}]
  (assoc (inter-task-constraint {:uid uid})
    :type :task-sequential-constraint
    :tasks (vec (or tasks []))))

(defn htn-task
  "All HTN tasks inherit from HTN-task"
  [{:keys [uid pclass name arguments cost task-type probability temporal-constraint
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight]
    :or {task-type :communications}}]
  (let [network-flows (if (network-flow-types name)
                        (make-network-flows name flow-characteristics arguments)
                        network-flows)]
    (assoc (htn-object {:uid uid})
      :type :htn-task
      :pclass pclass
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
  [{:keys [uid pclass name arguments cost task-type probability temporal-constraint
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight]
    :as options}]
  (assoc (htn-task options)
    :type :htn-primitive-task))

(defn htn-nonprimitive-task
  "NonPrimitive tasks can be decomposed into other tasks, by user HTN methods"
  [{:keys [uid pclass name arguments cost task-type probability temporal-constraint
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight]
    :as options}]
  (assoc (htn-task options)
    :type :htn-nonprimitive-task))

(defn htn-expanded-nonprimitive-task
  "Expanded NonPrimitive tasks have already been decomposed into other tasks, by using HTN methods"
  [{:keys [uid pclass name arguments cost task-type probability temporal-constraint
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight task-expansions]
    :as options}]
  (assoc (htn-nonprimitive-task options)
    :type :htn-expanded-nonprimitive-task
    :task-expansions task-expansions))

(defn htn-method
  "An HTN-method is used to decompose a nonprimitive-task into one or more subtasks"
  [{:keys [pclass
           name
           nonprimitive-task ;;This is the HTN-nonprimitive-task that this method matches on
           preconditions
           subtasks ;;This is a simple list of the tasks
           subtask-constraints ;;A list of constraints, expressing all of the partial orders
           mission-task-combination
           choice-weight]}]
  (let [method (assoc (htn-object {})
                 :type :htn-method
                 :pclass pclass
                 :name name
                 :nonprimitive-task nonprimitive-task
                 :preconditions preconditions
                 :subtasks (vec (or subtasks []))
                 :subtask-constraints subtask-constraints
                 :mission-task-combination mission-task-combination
                 :choice-weight choice-weight)]
    (add-htn-method method)
    method))

(declare copy-constraint-into-expansion)

;; There will be a 1:1 mapping between task-expansions and methods
(defn htn-expanded-method
  "This captures the results of applying the HTN-method to a nonprimitive task."
  [{:keys [expansion-method
           argument-mappings
           subtasks
           subtask-constraints]}]
  (let [orig-method expansion-method
        orig-subtasks (:subtasks orig-method)
        new-subtasks (:subtasks expansion-method)
        subtask-constraints (map
                              #(copy-constraint-into-expansion %
                                 orig-method orig-subtasks new-subtasks)
                              (:subtask-constraints orig-method))
        hem (assoc (htn-object {})
              :type :htn-expanded-method
              :expansion-method expansion-method
              :argument-mappings argument-mappings
              :subtasks (vec (or subtasks []))
              :subtask-constraints subtask-constraints)]
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


;; Expansion -------------------------------------------------

(defn copy-temporal-constraint [from-temporal-constraint]
  (temporal-constraint (dissoc from-temporal-constraint :uid)))

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

(defn find-methods-that-expand-task [task]
  (let [{:keys [type pclass name arguments]} task
        all-methods (seq (get @*htn-methods* pclass))]
    (when (= type :htn-nonprimitive-task)
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

                      (let [nonprimitive-task (htn-nonprimitive-task
                                                {:pclass k :name name :arguments args})
                            subtasks (make-subtasks k body)]
                        (println "  method:" name "is an htn-method")
                        ;; work here
                        (htn-method {:pclass k :name name
                                     :nonprimitive-task nonprimitive-task
                                     :subtasks subtasks})
                        )

(defn make-htn-methods [pclass body]
  (loop [things [] f (first body) more (rest body)]
    (if-not f
      things
      (let [{:keys [type name field method args body]} f
            _ (println "SUBTASK" type "NAME" name "METHOD" method)
            thing (cond
                      (#{:plant-fn-symbol :plant-fn-field} type)
                      ;; subtasks [(htn-task {:pclass pclass :name method :arguments args})]
                      (= type :sequence)
                      (make-subtasks pclass body) ;; FIXME add temporal-constraints
                      ;; (htn-method ...)
                      (= type :parallel)
                      (make-subtasks pclass body)
                      ;; (htn-method ...)
                      (= type :choose)
                      ;; create an htn-method FOR EACH choice
                      ;; (htn-method ...)
                      ;; (htn-method ...)
                      ;; (htn-method ...)
                      [type]
                      :else
                      [:TBD])
            things (concatv things thing)]
        (recur things (first more) (rest more))
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
              (loop [[name method] (first method-syms) moar (rest method-syms)]
                (println "METHOD NAME" name)
                (if-not name
                  nil ;; (println "no more methods")
                  (let [{:keys [temporal-constraints args body]} method]
                    (when body
                      (make-htn-methods pclass body)
                      (println "  method:" name "is NOT an htn-method"))
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
    (println "HTN-METHODS")
    (pprint @*htn-methods*)
    )
  )

(defn isr-test []
  (let [htn "/home/tmarble/src/lispmachine/pamela/notes/language-examples/isr-htn.pamela"
        ir (parser/parse {:input [htn]})]
    (reinitialize-htn-method-table)
    (transform-htn ir)))
