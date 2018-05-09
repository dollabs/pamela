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
            [avenir.utils :refer [concatv assoc-if keywordize vec-index-of
                                  as-keyword]]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [camel-snake-kebab.core :as translate]
            [instaparse.core :as insta]
            [pamela.utils :refer [stdout? output-file display-name-string
                                  dbg-println clj19-boolean? default-bounds? merge-bounds]]
            [pamela.parser :as parser]
            [pamela.unparser :as unparser]
            [pamela.tpn :as tpn]))

;; local implementation of gensym so that we get predictable uids in
;; generated plans.
(defonce my-count (atom 0))

(defn my-gensym [prefix]
  (str prefix (swap! my-count inc)))

(defn reset-my-count []
  (reset! my-count 0))

(defn fatal-error
  "Log messages and throw an exception"
  [& msgs]
  (let [msg (apply str (interpose " " msgs))]
    (log/error msg)
    (throw (AssertionError. msg))))

;; definitions -----------------------------------------------

;; The following is intended (only) for the support of the EdgeCT program.
;; So, we'll likely move this somewhere else.
(def network-flow-types #{"VOIP" "File Xfer" "File Transfer" "VTC"
                          "VideoStream"})

;; vars -------------------------------------------------------

(def ^:dynamic *belief-state-manager-plant-id* "bsm1")

(def ^:dynamic *debug-ir* (atom {}))

;; HTN Methods
;; HTN Methods have a 1:1 correspondence with the :methods defined by the Pamela source code

;; {pclass-name {method-name method}}
(def ^:dynamic *htn-methods* (atom {}))

(defn reinitialize-htn-method-table []
  (reset! *htn-methods* {}))

(defn add-htn-method [method]
  (let [{:keys [pclass name]} method]
    (when (and pclass name)
      (swap! *htn-methods* assoc-in [pclass name] method))))

(defn find-htn-method [pclass name & [error-if-not-found?]]
  (let [method (get-in @*htn-methods* [pclass name])]
    (when (and (not method) error-if-not-found?)
      (fatal-error "method not found: " name " in pclass " pclass))
    method))

;; HTN Objects
;; HTN Objects are all of the UID-identified objects in our HTN world

;; {uid htn-object}
(def ^:dynamic *htn-objects* (atom {}))

(defn reinitialize-htn-object-table [] (reset! *htn-objects* {}))

(defn get-htn-object [uid-or-object]
  (let [uid (if (keyword? uid-or-object)
              uid-or-object
              (:uid uid-or-object))]
    (get @*htn-objects* uid)))

(defn update-htn-object! [object]
  (let [uid (:uid object)]
    (swap! *htn-objects* assoc uid object)
    object))

;; HTN Plan Map contains the expanded HTN structure, based on expansion of
;; a root task and a library of HTN methods
;; {uid htn-object} where htn-object has keys trimmed
(def ^:dynamic *htn-plan-map* (atom {}))

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

(def ^:dynamic *htn-plan-ancestry*
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

(def reserved-conditional-method-names
  "Pamela statements that include a condition but no body"
  '#{ask tell assert})

(defn reserved-conditional-method-name?
  "non-NIL if the argument is a name of one of the Pamela reserved method names"
  [method-name]
  (reserved-conditional-method-names method-name))

(defn reserved-conditional-method-type?
  "non-NIL if the argument is a type of one of the Pamela reserved method types (keywords)"
  [method-type]
  (let [reserved-types (set (map as-keyword reserved-conditional-method-names))]
    (reserved-types method-type)))

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

;; FUTURE: (optionally) add pclass to name-with-args
(def name-with-args-max-line-length
  "The (preferred) maximimum line length for Task/Method Names including arguments"
  25)

;; TODO: This will be OBE, once Planviz can dynamically hide/show the args,
;; based on user setting.
(def name-with-args-include-args?
  "Whether to include argument lists when displaying names of Tasks and Methods"
  true)

(def activity-name-content
  "Since the :name of a TPN activity has evolved over time, this specifies how the :name is used"
  ;;options are:
  ;; :verbose - includes the plant info along with the args
  ;; :plant-command - just the command name that is sent to the plant
  ;;   [THIS IS THE LONG TERM BEHAVIOR]
  ;; :display-name - use the :display-name along with the args
  :display-name)

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
  (let [{:keys [pclass name arguments]} object]
    (str pclass)
    ;; (add-newlines-if-needed
    ;;   (apply str "NWA-"
    ;;     pclass ;; use pclass for top HTN node instead of name
    ;;     (if name-with-args-include-args?
    ;;       (map #(if (map? %) (:param %) %)
    ;;         (or (seq arguments) '()))))
    ;;   (or max-line-length name-with-args-max-line-length))
    ))

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
        {:keys [name nonprimitive-task display-name]} expansion-method
        method-name (if-not (empty? display-name) display-name (str name))
        {:keys [name arguments]} nonprimitive-task
        get-dyn-arg (fn [arg]
                      (let [dyn-arg (get argument-mappings arg)]
                        (if (map? dyn-arg)
                          (:param dyn-arg)
                          dyn-arg)))
        argvals (apply str (interpose "," (map get-dyn-arg arguments)))]
    (add-newlines-if-needed
      (str
        method-name
        (if name-with-args-include-args? "(")
        (if name-with-args-include-args? argvals)
        (if name-with-args-include-args? ")"))
      (or max-line-length name-with-args-max-line-length))))

(defn unparse-arg-kv
  ([kv]
   (let [[k v] kv]
     (unparse-arg-kv k v)))
  ([k v]
   (let [rv (cond
              (symbol? v)
              v
              (and (map? v) (symbol? k)
                (or (= :pclass-ctor (:type v)) (= :field-ref (:type v))))
              v
              :else
              v ;; (unparser/unparse-cond-expr v)
              )]
     (dbg-println :trace "  unparse-arg-kv" k v "=>" rv)
     rv)))

(defn unparse-arg-v [v]
  (unparse-arg-kv nil v))

;; symbols is a vector of one or more symbols
;; returns one symbol with all symbols combined with "."
(defn coalesce-symbols [symbols]
  (apply str (interpose "." (map str symbols))))

(defn display-argument [arg]
  (let [new-arg (if (:field-ref arg)
                  (coalesce-symbols (:names arg))
                  (unparser/unparse-cond-expr arg))
        _ (dbg-println :trace "  DA new-arg" (pr-str new-arg))
        new-arg (if (and (= new-arg '(nil)) (:param arg))
                  (:param arg)
                  new-arg)]
    (dbg-println :trace "  DA argument" arg "=>" new-arg)
    new-arg))

;; The younger cousin to resolve-arguments
(defn resolve-argument [pargs arg arg-map]
  (let [new-arg (cond
                  (symbol? arg)
                  (get arg-map arg arg)
                  (and (map? arg) (= :pclass-arg-ref (:type arg))
                    (= 1 (count (:names arg)))
                    (first (filter #(= (-> arg :names first) (:param %)) pargs)))
                  (first (filter #(= (-> arg :names first) (:param %)) pargs))
                  (and (map? arg) (= :method-arg-ref (:type arg))
                    (= 1 (count (:names arg)))
                    (get arg-map (-> arg :names first)))
                  (get arg-map (-> arg :names first))
                  :else
                  arg)]
    (dbg-println :trace "  RA argument" arg "=>" new-arg)
    new-arg))

;; Resolve arguments based on the call flow via ancestry-path
(defn resolve-arguments [pargs arguments argument-mapping ancestry-path
                         & [prev-args]]
  (let [arg-map (or argument-mapping
                  (-> ancestry-path first get-htn-object :argument-mappings))
        _ (dbg-println :trace "RA (before)" arguments "ARG-MAP" arg-map
            "PARGS" pargs)
        args (mapv resolve-argument (repeat pargs) arguments (repeat arg-map))
        _ (dbg-println :trace "RA (after)" args)
        args (if (not= args prev-args)
               (resolve-arguments
                 pargs
                 args
                 argument-mapping
                 (if ancestry-path (nthrest ancestry-path 2))
                 args)
               args)]
    (dbg-println :trace "RA final" args)
    args))

(defn print-ancestry-path [ancestry-path]
  (mapv #(list (:uid %) (or (:display-name %) (:name %)))
    ancestry-path))

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

;; Resources are a domain-specific legacy attribute.
;; TODO: We may want to generalize this.
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

;; copy-constraint-into-expansion multi-methods ------------------------------

(defn copy-constraint-into-expansion-dispatch [object orig-method orig-subtasks
                                               new-subtasks]
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

;; IF all-methods empty
;; AND this is found to be a non-primitive method in another (plant) pclass
;; THEN consider for a match

(defn pprint-htn-methods []
  (let [htn-methods @*htn-methods*
        pclasses (sort (keys htn-methods))]
    (dbg-println :debug "PPRINT-HTN-METHODS")
    (doseq [pclass pclasses]
      (dbg-println :debug "PCLASS" pclass "METHODS"
        (sort (keys (get htn-methods pclass)))))))

;; returns a sequence of [k v] pairs containing
;; methods which match name in pclass
;; or, if empty, matches in all other pclasses
(defn find-candidate-methods [ir pclass name]
  (let [htn-methods @*htn-methods*
        pclasses (keys htn-methods)
        all-pclasses (set (keys ir))
        other-pclasses (set/difference all-pclasses
                         (conj (set pclasses) 'pamela/lvars))]
    (dbg-println :debug "FCM PCLASS" pclass "NAME" name "PCLASSES" pclasses
      "OTHER-PCLASSES" other-pclasses)
    (loop [in-pclass [] ;; methods inside pclass
           out-pclass [] ;; methods outside of pclass
           pc (first pclasses)
           more (rest pclasses)]
      (if-not pc
        (sort-by first ;; return the methods sorted by method name
          (if-not (empty? in-pclass)
            in-pclass
            (if-not (empty? out-pclass)
              out-pclass
              (do
                (dbg-println :debug "NO CANDIDATES!")
                []))))
        (let [pc-methods (get htn-methods pc)
              ms (keys pc-methods)
              _ (dbg-println :debug "  FCM PC" pc)
              matches
              (loop [matches [] m (first ms) moar (rest ms)]
                (dbg-println :debug "    FCM METHOD" m)
                (if-not m
                  matches
                  (let [method (get pc-methods m)
                        npt-name (get-in method [:nonprimitive-task :name])
                        match (if (= npt-name name)
                                [m method])
                        matches (if match
                                  (conj matches match)
                                  matches)]
                    (recur matches (first moar) (rest moar)))))
              in-pclass (if (= pc pclass)
                          (concatv in-pclass matches)
                          in-pclass)
              out-pclass (if (= pc pclass)
                           out-pclass
                           (concatv out-pclass matches))]
          (recur in-pclass out-pclass (first more) (rest more)))))))

;; return methods vector
(defn find-methods-that-expand-task [ir task]
  (let [{:keys [type pclass name arguments]} task
        all-methods (if (nil? pclass)
                      (do (dbg-println :error "FMTET pclass is nil!!!") [])
                      (if (htn-isa? type :htn-nonprimitive-task)
                        (find-candidate-methods ir pclass name)))]
    (dbg-println :debug "FMTET" name "TYPE" type
      "PCLASS" pclass "ALL" (map first all-methods))
    (when-not (empty? all-methods)
      (loop [methods [] [mname method] (first all-methods)
             more (rest all-methods)]
        (if-not mname
          (do
            (dbg-println :debug "  FMTETMETHODS" (map :name methods))
            methods)
          (let [{:keys [nonprimitive-task]} method
                compatible? (fn [[task-arg method-arg]]
                              (or (symbol? task-arg)
                                (symbol? method-arg)
                                (= task-arg method-arg)))
                match? (and (= name (:name nonprimitive-task))
                         (= (count arguments)
                           (count (:arguments nonprimitive-task)))
                         (every? compatible?
                           (map vector arguments
                             (:arguments nonprimitive-task))))
                _ (dbg-println :debug "  FM NAME" name
                    "NPT-NAME" (:name nonprimitive-task)
                    "ARGS" arguments "NTARGS" (:arguments nonprimitive-task)
                    "match?" match?)
                methods (if match? (conj methods method) methods)]
            (recur methods (first more) (rest more))))))))

;; return args-mappings map
(defn arg-mappings-for-task-and-method [task method]
  (let [static-task-args (get-in method [:nonprimitive-task :arguments])
        dynamic-task-args (:arguments task)]
    (dbg-println :debug "AMFTAM" (:uid method) "name" (:name method))
    (dbg-println :debug "STATIC-TASK-ARGS" (pr-str static-task-args))
    (dbg-println :debug "DYNAMIC-TASK-ARGS" (pr-str dynamic-task-args))
    (if (or (not (vector? static-task-args))
          (not (vector? dynamic-task-args))
          (not= (count static-task-args) (count dynamic-task-args)))
      (fatal-error "cannot create arg-mappings with static-task-args: "
        static-task-args " and dynamic-task-args: " dynamic-task-args))
    (zipmap static-task-args dynamic-task-args)))

(declare htn-primitive-task)
(declare htn-expanded-nonprimitive-task)
(declare copy-temporal-constraints)
(declare htn-expanded-method)

;; NOTE assumes original-task is :htn-primitive-task or :htn-nonprimitive-task
(defn make-expanded-task
  "Make an expanded task"
  [original-task & [argument-mapping]]
  (dbg-println :debug "MET" original-task "args:" (pr-str argument-mapping))
  (let [task-type (:type original-task)
        skeleton-task (dissoc original-task :uid :display-name :display-args)
        task (if (= task-type :htn-primitive-task)
               (htn-primitive-task skeleton-task)
               (htn-expanded-nonprimitive-task skeleton-task))
        {:keys [temporal-constraints arguments]} task
        new-arguments (if argument-mapping
                        (mapv (fn [arg]
                                (if (and (map? arg) (:param arg))
                                  (:param arg)
                                  arg))
                          (resolve-arguments nil arguments argument-mapping nil))
                        arguments)]
    (dbg-println :debug "  MET TASK" task "NEW-ARGUMENTS" new-arguments)
    (update-htn-object!
      (assoc-if task
        :arguments new-arguments
        :temporal-constraints
        (and temporal-constraints
          (copy-temporal-constraints temporal-constraints))))))

;; plan-htn-task multi-methods --------------------------------------

(defn ir-object-dispatch [ir object]
  (:type object))

(defmulti plan-htn-task
  "Dispatch based on object type"
  #'ir-object-dispatch
  :default :htn-object
  :hierarchy #'htn-hierarchy)

(defmethod plan-htn-task :htn-object
  [ir object]
  (fatal-error "cannot plan-htn-task for object type: " (:type object)))

;; "Nothing to do for primitive tasks"
(defmethod plan-htn-task :htn-primitive-task
  [ir object])

;; "Expand"
(defmethod plan-htn-task :htn-expanded-nonprimitive-task
  [ir object]
  (let [task object
        methods (find-methods-that-expand-task ir task)]
    (dbg-println :trace "PHT-TASK"
      (with-out-str (pprint (dissoc task :irks :ancestry-path))))
    (binding [*htn-plan-ancestry* (cons task *htn-plan-ancestry*)]
      (doseq [method methods]
        (dbg-println :debug "PHT METHOD" (:name method)
          "ANCESTRY" (mapv :uid *htn-plan-ancestry*))
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
          (dbg-println :debug "PHT PCLASS" pclass "NAME" name
            "METHOD" (dissoc method :subtasks))
          (update-htn-object! task)))
      (doseq [expanded-method (:task-expansions (get-htn-object task))]
        (let [{:keys [subtasks]} expanded-method]
          (doseq [subtask subtasks]
            (let [{:keys [cost task-type probability mission-effectiveness
                          priority]} subtask
                  cost (if (= 1 (count subtasks))
                         (or cost (:cost task))
                         cost)
                  task-type (or task-type (:task-type task))
                  probability (or probability (:probability task))
                  mission-effectiveness (or mission-effectiveness
                                          (:mission-effectiveness task))
                  priority (or priority (:priority task))
                  ;; THIS IS NOT YET PORTED
                  ;; (if (and (not *prune-tasks-without-resources-p*)
                  ;;       (null (task-resources child-task)))
                  ;;   (setf (task-resources child-task)
                  ;;      (task-resources task)))))
                  subtask (assoc-if subtask
                            :cost cost
                            :task-type task-type
                            :probability probability
                            :mission-effectiveness mission-effectiveness
                            :priority priority)]
              (update-htn-object! subtask)))))
      (doseq [expansion-method (:task-expansions (get-htn-object task))]
        (binding [*htn-plan-ancestry*
                  (cons expansion-method *htn-plan-ancestry*)]
          (let [{:keys [subtasks]} expansion-method]
            (doseq [subtask subtasks]
              (let [{:keys [ancestry-path]} subtask
                    subtask (assoc subtask
                              :ancestry-path *htn-plan-ancestry*)]
                (dbg-println :debug "about to PHT" (:name subtask))
                (plan-htn-task ir (update-htn-object! subtask))))))))))

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
       :uid (or uid (keyword (my-gensym prefix)))}
      :ancestry-path ancestry-path)))

;; NOTE this function is never called
;; (defn temporal-constraint
;;   "A task-local temporal constraint for that task."
;;   [{:keys [prefix uid ancestry-path lb ub
;;            start-delay ;; Wait for this long (sec) before starting this task (included in lb/ub time)
;;            start-time
;;            end-time]
;;     :or {prefix "tc-"}}]
;;   (update-htn-object!
;;    (assoc-if (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
;;              :type :temporal-constraint
;;              :lb lb
;;              :ub ub
;;              :start-delay start-delay
;;              :start-time start-time
;;              :end-time end-time)))

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
  [{:keys [prefix uid ancestry-path pclass pargs name arguments cost task-type probability temporal-constraints
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight
           label display-name display-args incidence-set edges]
    :or {prefix "task-"
         task-type :communications
         display-name label
         temporal-constraints []}}]
  (let [network-flows (if (network-flow-types name)
                        (make-network-flows name flow-characteristics arguments)
                        network-flows)]
    (if (and (not pclass) (not= name 'delay))
      (fatal-error "htn-task constructed w/o pclass: name " name))
    (update-htn-object!
      (assoc-if (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
        :type :htn-task
        :pclass pclass
        :pargs pargs
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
        :display-name display-name
        :display-args display-args
        :incidence-set incidence-set ;; NOTE should be a clojure set
        :edges edges))))

(defn htn-primitive-task
  "Primitive tasks can't be decomposed any further"
  [{:keys [prefix uid ancestry-path pclass pargs name arguments cost task-type probability temporal-constraints
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight
           label display-name display-args  incidence-set edges irks]
    :as options}]
  ;;We should never see nil arguments
  (assert (not (some #(nil? %) arguments)) "htn-primitive-task: Found nil arguments")
  (update-htn-object!
    (assoc-if (htn-task (assoc options :prefix (or prefix "hpt-")))
      :type :htn-primitive-task
      :irks irks)))

(defn htn-nonprimitive-task
  "NonPrimitive tasks can be decomposed into other tasks, by user HTN methods"
  [{:keys [prefix uid ancestry-path pclass pargs name arguments cost task-type probability temporal-constraints
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight
           label display-name display-args incidence-set edges irks]
    :as options}]
  (update-htn-object!
    (assoc-if (htn-task (assoc options :prefix (or prefix "hnpt-")))
      :type :htn-nonprimitive-task
      :irks irks)))

(defn htn-expanded-nonprimitive-task
  "Expanded NonPrimitive tasks have already been decomposed into other tasks, by using HTN methods"
  [{:keys [prefix uid ancestry-path pclass pargs name arguments cost task-type probability temporal-constraints
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight task-expansions
           label display-name display-args incidence-set edges irks]
    :or {task-expansions []}
    :as options}]
  (let [task (assoc-if (htn-nonprimitive-task (assoc options :prefix (or prefix "henpt-")))
               :type :htn-expanded-nonprimitive-task
               :task-expansions task-expansions)
        display-name (if (empty? display-name) (name-with-args task) display-name)]
    (update-htn-object! (assoc-if task
                          :display-name display-name
                          :display-args display-args
                          :label label))))

(defn htn-method
  "An HTN-method is used to decompose a nonprimitive-task into one or more subtasks"
  [{:keys [prefix
           uid
           ancestry-path
           pclass
           name
           display-name
           display-args
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
                 :display-name display-name
                 :display-args display-args
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
        display-name (if (empty? display-name) (name-with-args method) display-name)
        method (update-htn-object! (assoc-if method
                                     :display-name display-name
                                     :label label))]
    (add-htn-method method)
    method))

(defn htn-network
  "HTN network"
  [{:keys [prefix uid ancestry-path label display-name rootnodes parentid]
    :or {prefix "net-"
         rootnodes []}}]
  (update-htn-object!
    (assoc-if (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
      :type :htn-network
      :display-name display-name
      :label label
      :rootnodes rootnodes
      :parentid parentid)))

(defn htn-edge
  "HTN edge"
  [{:keys [prefix uid ancestry-path label display-name end-node edge-type] ;; edge-type is usually only :choice (if not nil)
    :or {prefix "hedge-"}}]
  (update-htn-object!
    (assoc-if (htn-object {:prefix prefix :uid uid :ancestry-path ancestry-path})
      :type :edge
      :display-name display-name
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
           label display-name display-args
           incidence-set network
           irks]
    :or {prefix "hem-"
         subtasks []
         subtask-constraints []}}]
  (let [orig-method expansion-method
        display-name (or display-name (:display-name expansion-method))
        display-args (or display-args (:display-args expansion-method))
        orig-subtasks (:subtasks orig-method)
        new-subtasks subtasks ;; expanded henpt's
        subtask-constraints (map
                              #(copy-constraint-into-expansion %
                                 orig-method orig-subtasks new-subtasks)
                              (:subtask-constraints orig-method))
        hem (assoc-if (htn-object {:prefix prefix :uid uid
                                   :ancestry-path ancestry-path})
              :type :htn-expanded-method
              :expansion-method expansion-method
              :argument-mappings argument-mappings
              :subtasks (vec subtasks)
              :subtask-constraints (vec subtask-constraints)
              :incidence-set incidence-set ;; NOTE should be a clojure set
              :network network
              :irks irks)
        display-name (if (empty? display-name) (name-with-args hem) display-name)
        hem   (update-htn-object! (assoc-if hem
                                    :display-name display-name
                                    :display-args display-args
                                    :label label))]
    hem))

;; Expansion -------------------------------------------------

;; NOTE we no longer use temporal-constraint objects
;; and so there is no need to "copy" them.
(defn copy-temporal-constraints [from-temporal-constraints]
  ;; Old implementation
  ;; (mapv
  ;;  #(temporal-constraint (dissoc % :uid))
  ;;  from-temporal-constraints)
  ;; New implementation
  from-temporal-constraints)

;; Dan says this was never used
;; (defn computed-duration [temporal-constraint]
;;   (let [{:keys [start-time end-time]} temporal-constraint]
;;     (and start-time end-time (- end-time start-time))))

;; (defn lb [temporal-constraint]
;;   (let [{:keys [lb]} temporal-constraint]
;;     (or lb (computed-duration temporal-constraint) 0)))

;; (defn ub [temporal-constraint]
;;   (let [{:keys [ub]} temporal-constraint]
;;     (or ub (computed-duration temporal-constraint) :infinity)))

;; (defn contains-non-default-values? [temporal-constraint]
;;   (let [{:keys [lb ub start-delay start-time end-time]} temporal-constraint]
;;     (or (and lb (not (zero? lb)))
;;         (and ub (not (= ub :infinity)))
;;         start-delay
;;         start-time
;;         end-time)))

;; (defn make-temporal-constraint-for-sequential-subtasks [from-temporal-constraint]
;;   (temporal-constraint
;;    (assoc (dissoc from-temporal-constraint :uid) :lb 0)))

;; -------------------------------------------------------------------------

;; pclass instances
;; Each pclass-instance "pci" keeps the instance specifics such as
;; plant-id plant-part and plant-interface as well as the
;; pclass ancestry "pca" which notes from which field this pclass
;; instance was initialized.

(def ^:dynamic *pclass-instances* (atom {}))

(defn reinitialize-pclass-instances []
  (reset! *pclass-instances* {}))

(defn get-pclass-instance [uid-or-object]
  (let [uid (if (keyword? uid-or-object)
              uid-or-object
              (:uid uid-or-object))]
    (get @*pclass-instances* uid)))

(defn update-pclass-instance! [object]
  (let [uid (:uid object)]
    (swap! *pclass-instances* assoc uid object)
    (dbg-println :trace "UPCI" (with-out-str (pprint object)))
    object))

;; find an existing instance of pclass (presumably a zero arg
;; pclass which is used as a root-task arg and/or intermediate field)
(defn find-pclass-instance [pclass]
  (let [pci (first
              (for [uid-pci (seq @*pclass-instances*)
                    :let [[uid pci] uid-pci]
                    :when (= pclass (:pclass pci))]
                pci))]
    (dbg-println :trace "find-pclass-instance PCLASS" pclass "UID" (:uid pci))
    pci))

;; if this is NOT a root instance then ancestry SHOULD have at least
;; one entry [[:plass-123 field456] ...] which gives the pamela class
;; instance from which this instance is in scope AND the field from
;; that class where this instance is defined.
;; NOTE: fields are only specified if the field is pclass-ctor
(defn pclass-instance
  "A specific instance of a pclass"
  [{:keys [prefix uid pclass args
           plant-id plant-part plant-interface
           fields param ancestry]
    :or {prefix "pclass-"}}]
  (let [pci (update-pclass-instance!
              (assoc-if
                {:type :pclass-ctor} ;; superset of pclass-ctor
                :uid (or uid (keyword (my-gensym prefix)))
                :pclass pclass
                :args (or args [])
                :plant-id plant-id
                :plant-part plant-part
                :plant-interface plant-interface
                :fields (or fields {})
                :param param ;; formal pclass arg symbol
                :ancestry ancestry
                ))]
    ;; (dbg-println :trace "PCI" pci)
    pci))

;; Since pclass constructors in field initializers may contain
;; references to pclass args or other fields all such args that
;; can be resolved *a priori* are done here
(defn static-resolve-args [pca parg-mapping args]
  (let [pci-id (if pca (-> pca first first))
        pci (if pci-id (get-pclass-instance pci-id))]
    (dbg-println :trace "STATIC-RESOLVE-ARGS PCI" (:uid pci))
    (loop [rargs [] a (first args) more (rest args)]
      (if (nil? a)
        rargs
        (let [{:keys [type names]} (if (map? a) a)
              n0 (first names)
              _ (dbg-println :trace " A" a "N0" n0)
              ra (cond
                   (nil? a) ;; handles the special case of unspecified
                   a ;; plant-id plant-part plant-interface
                   (parser/literal? a)
                   a
                   (= type :field-ref)
                   (if n0 (get-in pci [:fields n0 :initial]))
                   (= type :pclass-arg-ref)
                   (get parg-mapping n0)
                   :else
                   (fatal-error "cannot statically resolve arg:" a))
              rargs (conj rargs ra)]
          (recur rargs (first more) (rest more)))))))

;; recursively construct an instance of this pclass-ctor
;; pargs are the actual pargs for this instance
;; options is a map of plant-id plant-part plant-interface
;; pca is the (optional) pclass-instance ancestry
(defn pclass-construct [ir pclass-ctor pargs options & [pca]]
  (dbg-println :trace "PCLASS-CONSTRUCT" pclass-ctor
    "\n  PARGS" pargs
    "\n  OPTIONS " options
    "\n  PCA" pca)
  (let [[plant-id plant-part plant-interface] options
        {:keys [pclass]} pclass-ctor
        pclass-def (get ir pclass)
        _ (if-not pclass-def
            (fatal-error "invalid pclass " pclass " does not exist"))
        {:keys [args fields]} pclass-def
        formal-pargs args
        keys-fields (keys fields)
        _ (if-not (= (count formal-pargs) (count pargs))
            (fatal-error "pclass constructor arity mismatch for pclass"
              pclass "expecting" formal-pargs "provided" pargs))
        ;; add :param to pargs
        add-param (fn [arg param]
                    (if (map? arg)
                      ((if (= (:type arg) :pclass-ctor)
                         update-pclass-instance!
                         identity)
                       (assoc arg :param param))
                      arg))
        pargs (mapv add-param pargs formal-pargs)
        parg-mapping (zipmap formal-pargs pargs)
        _ (dbg-println :trace "  FORMAL-PARGS" formal-pargs
            "\n  PARGS/param" pargs
            "\n  PARG-MAPPING" parg-mapping)
        pci (pclass-instance (assoc-if pclass-ctor
                               :args pargs ;; pargs have been resolved here
                               :plant-id plant-id
                               :plant-part plant-part
                               :plant-interface plant-interface
                               :ancestry pca))
        ;; NOW construct any fields of this class, as needed
        pci (loop [pci pci f (first keys-fields) more (rest keys-fields)]
              (if-not f
                pci
                (let [f-pca [(:uid pci) f]
                      new-pca (if pca
                                (vec (conj (seq pca) f-pca)) ;; prepend
                                [f-pca])
                      field (get-in ir [pclass :fields f])
                      {:keys [initial access observable]} field
                      ;; NOTE pclass switches from parent to field initializer
                      {:keys [type pclass args names
                              plant-id plant-part plant-interface]} initial
                      formal-args (if pclass (get-in ir [pclass :args]))
                      _ (dbg-println :trace "Consider FIELD" f "TYPE" type
                          "\n  PCLASS" pclass
                          "\n  FORMAL-ARGS" formal-args
                          "\n  ARGS" args)
                      args (if args
                             (static-resolve-args new-pca parg-mapping args))
                      _ (dbg-println :trace "Resolved ARGS" args)
                      options [plant-id plant-part plant-interface]
                      _ (dbg-println :trace "  OPTIONS before" options)
                      options (static-resolve-args new-pca parg-mapping options)
                      _ (dbg-println :trace "  OPTIONS after" options)
                      [plant-id plant-part plant-interface] options
                      initial (cond
                                (= type :pclass-ctor) ;; field is a pclass
                                (pclass-construct ir initial args
                                  options new-pca)
                                (= type :pclass-arg-ref) ;; field is parg ref
                                (get parg-mapping (first names))
                                (or (nil? type) (#{:mode-ref :lvar} type))
                                nil ;; nothing to change here
                                :else
                                (fatal-error "not handled field type:" type))
                      pci (if initial
                            (update-pclass-instance!
                              (assoc-in pci [:fields f]
                                (assoc field :initial initial)))
                            pci)]
                  (recur pci (first more) (rest more)))))]
    (dbg-println :trace "  PCLASS-CONSTRUCT done for" (:uid pci))
    pci))

;; ------------------------------------------------------

(defn ir-root-task [& args]
  args)

(defn ir-symbols [a b]
  (if (symbol? b)
    [a b]
    (vec (cons a b))))

(def root-task-ir {:argval identity
                   :boolean parser/ir-boolean
                   :float parser/ir-float
                   :integer parser/ir-integer
                   :keyword #(keyword (subs % 1))
                   :natural parser/ir-integer
                   :number identity
                   :root-task ir-root-task
                   :safe-keyword identity
                   :string identity
                   :symbol symbol
                   :symbols ir-symbols})

;; expects the caller-artity to match the method which should
;;   be one of the method definitions (mdefs) provided
;; returns [mi mdef] if found
(defn match-method-arity [caller-arity method-mdefs]
  (loop [mi 0 mdef (first method-mdefs) more (rest method-mdefs)]
    (if (or (not mdef) (= caller-arity (count (:args mdef))))
      (if mdef
        [mi mdef]
        [])
      (recur (inc mi) (first more) (rest more)))))

;; returns vector of pclasses which contain the given method
;;   at the caller-arity
(defn match-pclasses-method-arity [ir method caller-arity]
  (let [ks (keys ir)]
    (loop [pclasses [] k (first ks) more (rest ks)]
      (if-not k
        pclasses
        (let [kdef (get ir k)
              method-mdefs (if (= (:type kdef) :pclass)
                             (get-in kdef [:methods method]))
              [mi mdef] (if method-mdefs
                          (match-method-arity caller-arity method-mdefs))]
          (recur (if mdef (conj pclasses k) pclasses)
            (first more) (rest more)))))))

;; given a vector of syms, where
;;   the first sym is the pclass to start with
;;   (if any) intermediate syms are fields in the previous pclass
;;   the last sym is method
;; return [pci pargs method mi]
;;   pci may be an error string (instead of a pclass instance
;;   pargs are the args required to instanciate the pclass
(defn root-task-pclass-method [ir syms rt-args]
  (dbg-println :trace "RTPM SYMS" syms
    "\n  RT-ARGS" (with-out-str (pprint rt-args)))
  (let [pclass (first syms)
        pargs []
        pci (pclass-construct ir {:pclass pclass} pargs [])
        rt-fields (butlast (rest syms))
        rt-method (last syms)]
    (loop [pci pci pa [] f (first rt-fields) more (rest rt-fields)]
      (if-not f
        (let [[mi mdef] (match-method-arity (count rt-args)
                          (get-in ir [(:pclass pci) :methods rt-method]))]
          [pci pa rt-method mi])
        (let [_ (dbg-println :trace "RTPM PCI" (:uid pci) "F" f)
              f-initial (get-in pci [:fields f :initial])
              f-type (:type f-initial)]
          (if (not= f-type :pclass-ctor)
            (fatal-error "Invalid root task: intermediate field "
              f " is not a pclass constructor"))
          (recur f-initial pa (first more) (rest more)))))))

;; given arg which is a vector of syms (first is pclass, subsequent fields)
;; return the value of the utimate field
;;   or {:error msg}
(defn root-task-arg [ir arg]
  (dbg-println :debug "RTA" arg)
  (let [pclass (first arg)
        pclass-def (get ir pclass)
        pci (or (find-pclass-instance pclass)
              (pclass-construct ir {:pclass pclass} [] []))
        fields (rest arg)]
    (if-not pclass-def
      {:error
       (str "invalid root-task argument pclass " pclass " does not exist")}
      (loop [err nil parg nil p pclass f (first fields) more (rest fields)]
        (if (or err (not f))
          (if err
            {:error err}
            parg)
          (let [f-initial (get-in ir [p :fields f :initial])
                {:keys [type pclass args]} f-initial
                [err parg]
                (cond
                  (not= type :pclass-ctor)
                  [(str "Invalid root task: intermediate field "
                     f " is not a pclass constructor")]
                  (not (zero? (count args)))
                  [(str "Invalid root task: intermediate field "
                     f " pclass constructor must not require arguments")]
                  :else
                  (let [prev-pci (or parg pci)
                        ;; prev-field MAY have already been constructed...
                        prev-field (get-in prev-pci [:fields f :initial])
                        next-parg (or prev-field
                                    (pclass-construct ir f-initial [] []))]
                    (when-not (= next-parg prev-field)
                      (update-pclass-instance!
                        (assoc-in prev-pci [:fields f :initial] next-parg)))
                    [nil next-parg]))]
            (recur err parg pclass (first more) (rest more))))))))

;; given args
;;   which may include a vector of syms (first is pclass, subsequent fields)
;;   replace the sym vector the the value of the referenced field
;; return args as a vector
;;   (args may be an error string)
(defn root-task-args [ir args]
  (loop [rtargs []
         arg (first args) more (rest args)]
    (if (or (string? rtargs) (not arg))
      rtargs
      (let [rtarg (if (vector? arg)
                    (root-task-arg ir arg)
                    arg)
            ;; _ (dbg-println :debug "RTA out" rtarg)
            rtargs (if (and (map? rtarg) (:error rtarg))
                     (:error rtarg)
                     (conj rtargs rtarg))]
        (recur rtargs
          (first more) (rest more))))))

;; return [pclass pargs method mi args pca]
;; where pargs are needed to instanciate pclass
;; method is the method name, mi is the method index
;; and args are passed to method
;; pca is the pclass-instance ancestry
(defn identify-root-task [ir root-task]
  (log/trace "root-task" root-task)
  (let [parser (parser/build-parser "root-task.ebnf")
        tree (if root-task
               (insta/parses parser root-task))
        rir
        (cond
          (nil? tree)
          nil ;; FIND main in ir w/ no args
          (insta/failure? tree)
          (let [msg (str "parse: invalid root-task: " root-task)]
            (log/error msg)
            (log/error (with-out-str (pprint (insta/get-failure tree))))
            [:error msg])
          (not= 1 (count tree))
          (let [msg (str "parse: grammar is ambiguous for root-task: " root-task)]
            (log/error msg)
            (log/error (with-out-str (pprint tree)))
            [:error msg])
          :else
          (insta/transform root-task-ir (first tree)))
        ir-syms (keys ir)]
    ;; (dbg-println :debug "IR" (with-out-str (pprint ir)))
    (if (and rir (= (first rir) :error))
      (fatal-error (second rir))
      (if rir ;; verify
        (let [_ (dbg-println :debug "RIR" (with-out-str (pprint rir)))
              main-pclass (-> rir first first)
              args (root-task-args ir (rest rir))
              _ (dbg-println :debug "RT-ARGS" args)
              [pci pargs method mi] (root-task-pclass-method ir
                                      (first rir) args)
              _ (if (string? pci)
                  (fatal-error "root-task pclass error:" pci))
              {:keys [uid pclass]} pci
              pca [[uid]]
              _ (dbg-println :debug "PCI>" (:uid pci) "PCLASS" pclass
                  "PARGS" pargs "METHOD" method "MI" mi "PCA" pca)
              caller-arity (count args)
              caller-arg-str (if (= 1 caller-arity) " arg" " args")
              pclass-def (get ir pclass)
              method-mdef (if pclass-def
                            (get-in pclass-def [:methods method mi]))
              method-args (:args method-mdef)
              _ (dbg-println :debug "MARGS" method-args)
              add-param (fn [arg param]
                          (if (map? arg)
                            ;; NOTE :param may not be required for method
                            ;; invocation (i.e. not pclass constructor)
                            (update-pclass-instance!
                              (assoc arg :param param))
                            arg))
              args (if (and method-args (= (count args) (count method-args)))
                     (mapv add-param args method-args))]
          (if-not pclass-def
            (fatal-error "root-task pclass not found:" pclass)
            (if (string? args)
              (fatal-error args)
              (if-not method-mdef
                (fatal-error "root-task pclass" pclass
                  "does not have a method" method)
                (if (or (not method-args) (not= (count args) (count method-args)))
                  (fatal-error "root-task args \"" args
                    "\" does not match arity of " pclass "." method
                    " " method-args)
                  [pclass pargs method mi args pca])))))
        (loop [pclass nil method nil mi nil
               k (first ir-syms) more (rest ir-syms)] ;; find main
          (if (or (and pclass method mi) (not k))
            (if-not (and pclass method mi)
              (fatal-error "root-task pclass with a zero arg main not found")
              (let [pci (pclass-construct ir {:pclass pclass} [] [])
                    pca [[(:uid pci)]]]
                [pclass [] method mi [] pca]))
            (let [k-def (get ir k)
                  {:keys [type methods]} k-def
                  [mi mdef] (if (= type :pclass)
                              (match-method-arity 0
                                (get-in ir [k :methods 'main])))
                  [pclass method mi] (if mdef
                                       [k 'main mi]
                                       [nil nil nil])]
              (recur pclass method mi (first more) (rest more)))))))))

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
(defn transform-htn [ir pargs]
  (doseq [[pclass-name v] (seq ir)]
    (when (= (:type v) :pclass)
      (assert pclass-name "Found a nil pclass in the IR")
      (let [{:keys [type methods]} v]
        (dbg-println :info "TRANSFORM-HTN" pclass-name)
        (doseq [[mname method-mdefs] (seq methods)]
          (if mname ;; else (dbg-println :debug "no more methods")
            (loop [mi 0 mdef (first method-mdefs) more (rest method-mdefs)]
              (when mdef
                (let [{:keys [temporal-constraints args primitive display-name
                              display-args body]} mdef]
                  (dbg-println :info "  METHOD" mname "MI" mi
                    "PRIMITIVE" primitive "ARGS" args
                    "DISPLAY-NAME" display-name "DISPLAY-ARGS" display-args)
                  (when (and (not primitive) body)
                    (if (and (= 1 (count body))
                          (= :choose (get-in body [0 :type])))
                      (dbg-println :info "  TOP LEVEL CHOOSE!"))
                    (make-htn-methods ir pargs true pclass-name mname mi
                      display-name display-args args body)))
                (recur (inc mi) (first more) (rest more))))))))))

;; consider memoizing or caching result
(defn find-plant-fn-bounds [ir plant-fn-pclass ctor-arg-i method margs]
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
                            [mi mdef] (match-method-arity
                                        (count margs)
                                        (get-in ir [plant :methods method]))
                            temporal-constraints (if plant
                                                   (get-in ir
                                                     [plant :methods method mi
                                                      :temporal-constraints]))
                            b (if temporal-constraints
                                (get-in temporal-constraints [0 :value]))]
                        (recur b (first moar) (rest moar)))))]
              (recur bounds (first more) (rest more)))
            (recur bounds (first more) (rest more))))))))

(defn irks->bounds [ir irks]
  (if irks
    (let [opts (get-in ir irks)
          {:keys [type field method args temporal-constraints]} opts
          bounds (if temporal-constraints
                   (get-in temporal-constraints [0 :value]))]
      (or bounds
        ;; look up from plant method options
        (if (and (= type :plant-fn-field) field method)
          (let [pclass (first irks)
                field-irks [pclass :fields field :initial]
                field-init (get-in ir field-irks)
                {:keys [type name]} field-init
                pargs (get-in ir [pclass :args])
                ctor-arg-i (if (and (= type :arg-reference) name)
                             (vec-index-of pargs name))
                bounds (if ctor-arg-i
                         (find-plant-fn-bounds ir pclass ctor-arg-i
                           method args))]
            bounds))))))

(defn bounds-from-mdef [mdef]
  (let [bounds (:value
                (first
                  (filter #(= (:type %) :bounds)
                    (:temporal-constraints mdef))))]
    (if-not (default-bounds? bounds)
      bounds)))

;; starting at pca try to find the field reference
;; on success return the pclass-ctor_ and ancestry for that pclass instance
(defn resolve-field-ref [ir f pca]
  (dbg-println :trace "RFR" f "PCA" pca)
  (let [[[pci-uid pci-field] & more] pca
        pci (if pci-uid (get-pclass-instance pci-uid))
        fpc_ (if pci (get-in pci [:fields f :initial]))
        [c_ a_] (cond
                  ;; do NOT dereference fields here!
                  ;; (string? fpc_) ;; handle all literals?
                  ;; [fpc_ pca]
                  (nil? (:type fpc_))
                  [f pca] ;; maintain as field reference
                  (= :pclass-ctor (:type fpc_))
                  [fpc_ (:ancestry fpc_)]
                  (= :pclass-arg-ref (:type fpc_))
                  ;; find first of (:names initial) in formal
                  ;; args and then take that one from actual args
                  ;; (get
                  ;;   (:args pci)
                  ;;   (vec-index-of
                  ;;     (get-in ir [(:pclass pci) :args])
                  ;;     (-> fpc_ :names first)))
                  (let [actual-args (:args pci)
                        formal-args (get-in ir [(:pclass pci) :args])
                        formal-arg (-> fpc_ :names first)
                        actual-arg (get actual-args
                                     (vec-index-of formal-args formal-arg))]
                    (dbg-println :trace "  RFR :pclass-arg-ref PCLASS" (:pclass pci)
                      "\n    ACTUAL-ARGS" actual-args
                      "\n    FORMAL-ARGS" formal-args
                      "\n    FORMAL-ARG" formal-arg
                      "\n    ACTUAL-ARG" actual-arg)
                    [actual-arg pca])
                  ;; NOTE :field-ref NOT handled here yet
                  :else
                  ;; get initial for types OTHER than pclass-ctor_ from the IR
                  (let [initial (get-in ir [(:pclass pci) :fields f :initial])]
                    (if initial ;; not nil value, e.g. string
                      [initial pca]
                      (if-not (empty? more)
                        (resolve-field-ref ir f more)))))
        rv [(if (map? c_) (dissoc c_ :fields) c_) a_]]
    (dbg-println :trace "  RFR rv =>"
      (if (map? rv)
        (dissoc rv :fields)
        rv))
    rv))

;; The purpose of this function is to resolve method arguments.
;; Future versions of this function certainly want to take advantage
;; of the new pci/pca infrastructure which should, beyond providing
;; direct instance information, facilitate nested field derefencing.
;; Currently supports one level of dereferencing.
(defn resolve-to-plant-instance [ir caller-pclass hem-pclass pargs args
                                 subtask_ pca]
  (dbg-println :debug "RTPI caller-pclass" caller-pclass "hem-pclass" hem-pclass
    "PCA" pca)
  (loop [rargs [] a (first args) more (rest args)]
    (if (nil? a)
      rargs
      (let [{:keys [type names]} (if (map? a) a)
            [n0 n1] names
            _ (dbg-println :debug "RTPI A" a)
            ra (cond
                 (= :pclass-arg-ref type)
                 (let [parg (first (filter #(= n0 (:param %)) pargs))]
                   (if (= :pclass-ctor (:type parg))
                     (assoc-if a
                       :pclass (:pclass parg)
                       :plant-id (:plant-id parg)
                       :plant-part (:plant-part parg)
                       :plant-interface (:plant-interface parg))
                     a))
                 (= :method-arg-ref type)
                 (let [{:keys [ancestry-path]} subtask_
                       hem (get-htn-object (first ancestry-path))
                       {:keys [argument-mappings]} hem
                       mval (get argument-mappings n0)
                       _ (dbg-println :debug "  RTPI :method-arg-ref N0"
                           n0 "-> mval" mval "N1" n1)]
                   (cond
                     (= :pclass-ctor (:type mval))
                     (assoc-if a
                       :pclass (:pclass mval)
                       :plant-id (:plant-id mval)
                       :plant-part (:plant-part mval)
                       :plant-interface (:plant-interface mval))
                     (= :pclass-arg-ref (:type mval))
                     (let [[n0 n1] (:names mval)
                           parg (first (filter #(= n0 (:param %)) pargs))]
                       (if (= :pclass-ctor (:type parg))
                         (assoc-if a
                           :pclass (:pclass parg)
                           :plant-id (:plant-id parg)
                           :plant-part (:plant-part parg)
                           :plant-interface (:plant-interface parg))
                         a))
                     (= :field-ref (:type mval))
                     (let [arg (-> mval :names first)
                           _ (dbg-println :debug "  RTPI MARG" n0
                               "MVAL" mval "ARG" arg)
                           ;; caller-subtask (second ancestry-path)
                           ;; caller-ancestry-path (:ancestry-path
                           ;;                       caller-subtask)
                           ;; caller-hem (get-htn-object
                           ;;              (first caller-ancestry-path))
                           ;; caller-argument-mappings (:argument-mappings
                           ;;                           caller-hem)
                           ;; aval (get caller-argument-mappings (or arg mval))
                           [aval _] (resolve-field-ref ir arg (vec (rest pca)))
                           _ (dbg-println :debug "  AVAL"
                               (if (map? aval)
                                 (dissoc aval :fields)
                                 aval))]
                       (if (= :pclass-ctor (:type aval))
                         (if n1 ;; dereference
                           (let [initial
                                 (get-in ir
                                   [(:pclass aval) :fields n1 :initial])]
                             (dbg-println :debug "    INITIAL" initial)
                             (cond
                               (= :pclass-arg-ref (:type initial))
                               ;; find first of (:names initial) in formal
                               ;; args and then take that one from actual args
                               (get
                                 (:args aval)
                                 (vec-index-of
                                   (get-in ir [(:pclass aval) :args])
                                   (-> initial :names first)))
                               :else ;; NOT handled yet :field-ref
                               initial))
                           (assoc-if a
                             :pclass (:pclass aval)
                             :plant-id (:plant-id aval)
                             :plant-part (:plant-part aval)
                             :plant-interface (:plant-interface aval)))
                         a))
                     :else
                     mval))
                 (= :field-ref type)
                 (let [[fpc_ fpca] (resolve-field-ref ir n0 pca)
                       this-initial_ (get-in ir
                                       [caller-pclass :fields n0 :initial])
                       caller-initial_ (get-in ir
                                         [caller-pclass :fields n0 :initial])
                       _ (dbg-println :debug "RTPI :field-ref fpc_" fpc_
                           "\n  this-initial_" this-initial_
                           "\n  caller-initial_" caller-initial_)]
                   (cond
                     (symbol? fpc_) ;; preserve field-ref
                     (assoc a :ancestry fpca)
                     fpc_
                     fpc_
                     (= :pclass-ctor (:type this-initial_))
                     (assoc-if a
                       :pclass (:pclass this-initial_)
                       :plant-id (:plant-id this-initial_)
                       :plant-part (:plant-part this-initial_)
                       :plant-interface (:plant-interface this-initial_))
                     (= :pclass-ctor (:type caller-initial_))
                     (assoc-if a
                       :pclass (:pclass caller-initial_)
                       :plant-id (:plant-id caller-initial_)
                       :plant-part (:plant-part caller-initial_)
                       :plant-interface (:plant-interface caller-initial_))
                     (= :pclass-arg-ref (:type this-initial_))
                     (let [param (-> this-initial_ :names first)
                           parg (first (filter #(= param (:param %)) pargs))]
                       (if (= :pclass-ctor (:type parg))
                         (assoc-if a
                           :pclass (:pclass parg)
                           :plant-id (:plant-id parg)
                           :plant-part (:plant-part parg)
                           :plant-interface (:plant-interface parg))
                         parg))
                     :else
                     (fatal-error
                       "RTPI not implemented yet: non :pclass-ctor field-ref")))
                 :else
                 a)
            _ (dbg-println :debug "  RTPI RA =>" ra)
            rargs (conj rargs ra)]
        (recur rargs (first more) (rest more))))))

;; returns plant details map with keys
;;   name display-name plant-id plant-part plant-interface args argvals
(defn plant-details [ir hem-pclass pargs subtask_ pclass-ctor_ primitive?
                     method-opts pca]
  (let [method-opts (select-keys method-opts ;; from call site
                      [:label :cost :reward :controllable])
        {:keys [type name arguments ancestry-path]} subtask_
        caller-pclass (if (> (count ancestry-path ) 3)
                        (-> ancestry-path (nth 3) :pclass))
        name-str (str name)
        display-name (display-name-string name-str)
        {:keys [pclass plant-id plant-part plant-interface]} pclass-ctor_
        plant-id (if (reserved-conditional-method-name? name)
                   *belief-state-manager-plant-id*
                   plant-id)
        details_ (assoc-if {:name name-str
                            :display-name display-name
                            :method-opts method-opts}
                   :plant-id plant-id
                   :plant-part plant-part
                   :plant-interface plant-interface)]
    (dbg-println :trace "PD primitive?" primitive?
      "ancestry-path" (print-ancestry-path ancestry-path)
      "caller-pclass" caller-pclass)
    (if (= name 'delay)
      details_
      (let [args (or arguments []) ;; now resolve args for non primitive?
            _ (dbg-println :trace "PD args before" args)
            args (resolve-arguments pargs args nil ancestry-path)
            display-args-before (mapv display-argument args)
            args (resolve-to-plant-instance ir caller-pclass hem-pclass
                   pargs args subtask_ pca)
            display-args (mapv
                           (fn [before-arg arg]
                             (let [da (display-argument arg)]
                               (if (and da (seq? da) (nil? (first da)))
                                 before-arg
                                 da)))
                           display-args-before
                           args)
            _ (dbg-println :trace "PD args after" args)
            _ (dbg-println :trace "PD display-args" display-args)
            _ (dbg-println :debug "PD pclass-ctor_" pclass-ctor_)
            _ (dbg-println :debug "PD get-in" pclass name)
            [mi mdef] (if (reserved-conditional-method-name? name)
                        [0 {:args '[condition]}] ;;TODO: Generalize this
                        (match-method-arity
                          (count args)
                          (get-in ir [pclass :methods name])))
            _ (dbg-println :debug "MDEF" mdef)
            formal-args (:args mdef)
            _ (dbg-println :debug "before FORMAL-ARGS" (pr-str formal-args))
            ;; convert formal arg symbols to strings
            formal-args (mapv #(if (symbol? %) (str %) %) formal-args)
            _ (dbg-println :debug "PD ARGS" (pr-str args)
                "FORMAL-ARGS" (pr-str formal-args)) ;;:: DEBUG
            argsmap (zipmap formal-args args)
            opt-bounds (bounds-from-mdef mdef) ;; here only from method def
            label (:label method-opts) ;; only from call site
            ;; For the following prefer call site, default to method def
            cost (or (:cost method-opts) (:cost mdef))
            reward (or (:reward method-opts) (:reward mdef))
            controllable (if (clj19-boolean? (:controllable method-opts))
                           (:controllable method-opts)
                           (:controllable mdef))]
        (assoc-if details_
          :args args
          :argsmap argsmap
          :display-name (or (:display-name mdef) display-name)
          :display-args display-args
          :method-opts (assoc-if {}
                         :opt-bounds opt-bounds
                         :label label
                         :cost cost
                         :reward reward
                         :controllable controllable)
          )))))


;; create new pclass instance ancestry by prepending the uid
;; if the pclass of pclass-ctor_ to it's ancestry
(defn prepend-pca [pclass-ctor_]
  (let [{:keys [uid ancestry]} pclass-ctor_]
    (vec (conj (seq ancestry) [uid])))) ;; prepend

;; if we call a method in this class we NEED to re-add this pclass
;; instance to the PCA
(defn local-method-pca [pca]
  (let [[pca0 & more] pca]
    (vec (conj (seq pca) pca0)))) ;; prepend

;; given the method function
;; return the apropos [pclass-ctor_ pca]
(defn resolve-method [ir method-fn_ pca pci argument-mappings pargs
                      ancestry-path]
  (dbg-println :debug "RM method-fn_ (w/o body)" (dissoc method-fn_ :body)
    "\n  PCA" pca)
  (let [m-type (:type method-fn_)]
    (cond
      (= m-type :method-fn) ;; this is a pclass method invocation
      (let [method-ref (:method-ref method-fn_)
            {:keys [type names]} method-ref
            [n0 n1] names];; first two names in call/indirection
        (cond
          ;; is this a field reference?
          (and (= type :field-ref) (get-in pci [:fields n0]))
          (let [initial (get-in pci [:fields n0 :initial])
                pc_ (dissoc initial :fields)]
            [pc_ (prepend-pca pc_)])
          ;; is this a function of this pclass? where n1 is the function
          (and (= type :symbol-ref) (= n0 'this))
          [(dissoc pci :fields) (local-method-pca pca)]
          (and (= type :pclass-arg-ref) n0)
          (let [parg (first (filter #(= n0 (:param %)) pargs))]
            (if (not= :pclass-ctor (:type parg))
              (fatal-error "pclass argument"
                n0 "is not a pamela class constructor" )
              [(dissoc parg :fields) (prepend-pca parg)]))
          (and (= type :method-arg-ref) n0)
          (let [mval (get argument-mappings n0)]
            (dbg-println :debug "  :method-arg-ref N0" n0 "MVAL" mval)
            (cond
              (symbol? mval)
              (let [arg (-> mval :names first)
                    _ (dbg-println :debug "N0" n0 "MVAL" mval "ARG" arg)
                    caller-subtask (second ancestry-path)
                    caller-ancestry-path (:ancestry-path caller-subtask)
                    caller-hem (get-htn-object (first caller-ancestry-path))
                    caller-argument-mappings (:argument-mappings caller-hem)
                    _ (dbg-println :trace "CALLER-ARGUMENT-MAPPINGS"
                        caller-argument-mappings)
                    pc_ (get caller-argument-mappings (or arg mval))]
                (dbg-println :debug "CALLER result" pc_)
                (if (not= :pclass-ctor (:type pc_))
                  (fatal-error "the method call using the argument"
                    n0 "is not a pamela class constructor" ))
                [pc_ (prepend-pca pc_)])
              (not (map? mval))
              (fatal-error "the method call using argument" n0
                (str "which evaluates to '" mval
                  "' (calling the function ") n1
                ") is not a pamela class constructor")
              (= :pclass-ctor (:type mval))
              [mval pca]
              (= :pclass-arg-ref (:type mval))
              (let [[n0 n1] (:names mval)
                    parg (first (filter #(= n0 (:param %)) pargs))]
                (if (not= :pclass-ctor (:type parg))
                  (fatal-error "pclass argument"
                    n0 "is not a pamela class constructor" )
                  [(dissoc parg :fields) (prepend-pca parg)]))
              (= :field-ref (:type mval))
              (let [[fpc_ fpca] (resolve-field-ref ir
                                  (-> mval :names first) (vec (rest pca)))]
                (if (not= :pclass-ctor (:type fpc_))
                  (fatal-error "pclass argument"
                    n0 "is not a pamela class constructor" ))
                [fpc_ fpca])
              :else
              (fatal-error "the method call using argument" n0
                (str "which evaluates to '" mval
                  "' (calling the function ") n1
                ") is not a pamela class constructor (unexpected type)")
              ))
          :else
          (fatal-error "RM do not know how to handle method-ref type" type)))
      ;; for built-in conditional expressions or delay there are
      ;; no plant specific options a so we return an empty pclass-ctor_
      (or (reserved-conditional-method-type? m-type) (= m-type :delay))
      [{} pca]
      :else
      (fatal-error "RM do not know how to handle method-fn_ m-type" m-type))))

;; root? is true to create the "synthetic" htn-network at the very
;; top of the HTN
;; begin is the state node of the parent task
(defn construct-hem-plan-map [ir pargs hem henpt root? parent-begin-uid
                              labels all-betweens pca choice-begin-end]
  (let [{:keys [uid display-name subtasks subtask-constraints edges
                ancestry-path argument-mappings irks]} hem
        _ (dbg-println :debug "CHPM PCA" pca)
        _ (dbg-println :debug "CHPM HEM" (pr-str
                                           (dissoc hem :subtasks :ancestry-path
                                             :expansion-method :subtask-constraints)))
        ;; _ (dbg-println :debug "CHPM HENPT" (dissoc henpt :subtasks :ancestry-path
        ;;                           :expansion-method :subtask-constraints))
        hem-irks irks
        [pclass kw-methods method mi kw-body int-zero more-irks] hem-irks
        hem-pclass pclass
        [pci-uid pci-field] (first pca) ;; get pclass instance ancestry
        pci (if pci-uid (get-pclass-instance pci-uid))
        _ (if (and pci (not= (:pclass pci) hem-pclass))
            (fatal-error "pca pclass" (:pclass pci)
              "does not match hem-pclass" hem-pclass))
        _ (dbg-println :debug "CHPM 0.9 PCI uid" (:uid pci)
            "PCLASS" (:pclass pci) "FIELD" pci-field)
        pargs (:args pci) ;; use ACTUAL pclass args!
        _ (dbg-println :debug "CHPM PARGS" pargs)
        _ (dbg-println :debug "CHPM 1" pclass method int-zero more-irks kw-methods)
        ;; Here we get the size of the hem irks vector to distinguish
        ;; a choice from anything else. Using first.pamela as an example, the
        ;; irks to the second choice is
        ;; 9 = [first-tpn-htn :methods do-choice 0 :body 0 :body 1 :body]
        ;; whereas the sequence in do-f-and-g is
        ;; 6 = [first-tpn-htn :methods do-f-and-g 0 :body 0]
        n-hem-irks (count hem-irks)
        ;; Here funcall_ is the "function" (choice or other) that may have a display-name
        fun-irks (vec (take 6 hem-irks))
        funcall_ (get-in ir fun-irks)
        hem-label (:label funcall_)
        betweens (get-in ir (conj hem-irks :betweens))
        _ (dbg-println :debug "CHPM 2 hem-label" hem-label "betweens" betweens)
        hem-uid uid
        ;; NOTE as the new args-mapping structure has not yet been implemented
        ;; we are "guessing" at the positions of the arg values here:
        args (if (empty? argument-mappings)
               []
               (mapv unparse-arg-kv (seq argument-mappings)))
        _ (dbg-println :debug "CHPM 2.2 args" args)
        hem-map (assoc
                  (dissoc hem
                    :ancestry-path :expansion-method :argument-mappings :subtasks
                    :subtask-constraints :irks)
                  :incidence-set #{}
                  :edges []
                  :args args
                  :display-args (mapv display-argument args))
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
        sequential? (or (not (nil? tsc)) (= 1 n-subtasks)) ;; could be a choice
        ;; distinguish choice from sequence using n-hem-irks (see binding above)
        hem-choice? (and sequential? (= 9 n-hem-irks))
        top-irks (if (and pclass method (zero? int-zero) (nil? more-irks))
                   [pclass kw-methods method mi])
        top-bounds (irks->bounds ir top-irks)
        bounds (irks->bounds ir hem-irks)
        hem-bounds (merge-bounds top-bounds bounds)
        _ (dbg-println :debug "CHPM 2.5 top-irks" top-irks
            "top-bounds" top-bounds
            "hem-irks" hem-irks "bounds" bounds
            "hem-bounds" hem-bounds)
        [label sequence-label] (if (or (not sequential?) hem-choice?)
                                 [hem-label nil] ;; parallel or choice
                                 [nil hem-label]) ;; sequence
        _ (dbg-println :debug "CHPM 3 n-hem-irks = " n-hem-irks
            "label" label "sequence-label" sequence-label)
        i-last-subtask (dec (count subtask-order))
        net-map (if (pos? (count subtasks))
                  (htn-network {:display-name display-name :rootnodes #{}}))
        parent-begin (tpn/get-tpn-plan-map parent-begin-uid) ;; TPN------
        {:keys [end-node]} parent-begin
        parent-end (tpn/get-tpn-plan-map end-node)
        hem-tc (if hem-bounds (tpn/tpn-temporal-constraint
                                {:value hem-bounds :end-node end-node}))
        parent-begin-constraints (:constraints parent-begin)
        parent-begin (assoc-if parent-begin
                       :htn-node (:uid hem-map)
                       ;; only if there isn't an override at the call site
                       :constraints (if (and (empty? parent-begin-constraints)
                                          hem-tc)
                                      #{(:uid hem-tc)}
                                      parent-begin-constraints)
                       :label (and (not hem-choice?) label)
                       :sequence-label sequence-label)
        ;; _ (when (and (not hem-choice?) (or label sequence-label))
        ;;     (dbg-println :trace "CHPM 4 parent-begin-uid" parent-begin-uid
        ;;       "getting :label" (and (not hem-choice?) label)))
        _ (dbg-println :trace "CHPM 4 parent-begin-uid" parent-begin-uid
            "parent-end-uid" end-node)
        first-begin-uid (atom nil)
        prev-end (atom nil)]
    (when (or label sequence-label)
      (when (and hem-choice? label (not (get @labels label)))
        (dbg-println :debug "ADD choice label" label
          "choice-begin-end" choice-begin-end)
        (swap! labels assoc label choice-begin-end)
        (tpn/update-tpn-plan-map!
          (assoc
            (tpn/get-tpn-plan-map (first choice-begin-end))
            :label label)))
      (when (not hem-choice?)
        (swap! labels assoc (or label sequence-label)
          [parent-begin-uid end-node])))
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
    (when betweens
      (swap! all-betweens concatv betweens))
    ;; HERE we need to recurse and, create a network of tasks, edges
    ;; to child hems
    (doseq [i (range n-subtasks)]
      (let [subtask-uid (get subtask-order i)
            edge (if (and sequential? (pos? i))
                   (htn-edge {:end-node subtask-uid}))
            subtask_ (get-htn-object subtask-uid)
            {:keys [type pclass name task-expansions arguments
                    irks ancestry-path]} subtask_
            subtask-name name
            method-fn_ (if irks (get-in ir irks))
            choice-bounds (if hem-choice? ;; (choice :bounds [1 2] (fn))
                            (irks->bounds ir (vec (take 8 hem-irks))))
            bounds (if (and irks (not= irks hem-irks))
                     (irks->bounds ir irks))
            bounds (merge-bounds choice-bounds bounds)
            m-task-expansions (count task-expansions)

            _ (dbg-println :debug "ST#" i "of" n-subtasks "IRKS" irks
                "type" type "pclass" pclass "name" name
                "m-task-expansions" m-task-expansions
                "bounds" bounds)
            ;; resolve pclass if (= type :htn-nonprimitive-task)
            ;; to determine if this was really an unresolved primitive task
            ;; DEBUG plant-class (if (htn-isa? type :htn-nonprimitive-task)
            primitive? (= type :htn-primitive-task)
            parallel? (not sequential?)
            choice? (and (not parallel?) (> m-task-expansions 1))
            bounds (if (and (not bounds) primitive?)
                     nil ;; look up default
                     bounds)
            _ (dbg-println :debug "SUBTASK primitive?" primitive?
                "type" type
                "parallel?" parallel?
                "choice?" choice?
                "sequential?" sequential?
                "\nsubtask_" (with-out-str
                               (pprint
                                 (dissoc subtask_
                                   :ancestry-path :task-expansions))))
            [pclass-ctor_ pca] (resolve-method ir method-fn_ pca pci
                                 argument-mappings pargs ancestry-path)
            _ (dbg-println :debug "Resolved method PCA" pca
                "\n  pclass-ctor_" pclass-ctor_)
            details_ (plant-details ir hem-pclass pargs subtask_
                       pclass-ctor_ primitive? method-fn_ pca)
            _ (dbg-println :trace "DETAILS_" (with-out-str (pprint details_)))
            {:keys [name display-name args display-args argsmap
                    plant-part plant-id plant-interface
                    method-opts]} details_
            {:keys [opt-bounds label cost reward controllable]} method-opts
            subtask-map (merge
                          (assoc
                            (dissoc subtask_ :pclass :pargs :arguments
                              :task-expansions
                              :task-type :ancestry-path :temporal-constraints
                              :irks)
                            :incidence-set (if edge #{(:uid edge)} #{})
                            :edges [])
                          (dissoc details_ :method-opts))
            ;; TPN ------------------
            se (tpn/tpn-state {})
            ;; if call site bounds are not specified use default
            bounds (or bounds opt-bounds)
            tc (if bounds
                 (tpn/tpn-temporal-constraint
                   {:value bounds :end-node (:uid se)}))
            _ (dbg-println :debug "TC" tc)
            activity (if primitive?
                       ((if (= subtask-name 'delay)
                          tpn/tpn-delay-activity
                          tpn/tpn-activity)
                        {:name (case activity-name-content
                                 :plant-command (str name
                                                  (seq args))
                                 :verbose (let [plant-id (or plant-id "plant")
                                                plant-interface (or plant-interface "RMQ")]
                                            (str name
                                              (seq args)
                                              "@"
                                              (if plant-part (str plant-part "."))
                                              (if plant-id (str plant-id "."))
                                              plant-interface))
                                 :display-name (str display-name
                                                 (if (or (= subtask-name 'delay)
                                                       (empty? args))
                                                   nil
                                                   ;; FIX (seq (to-pamela args))
                                                   ;; simple implementation
                                                   ;; pending #139
                                                   (pr-str (seq args))
                                                   )))
                         :command name
                         :display-name display-name
                         :display-args display-args
                         ;; NOTE: labels go on activities if primitive
                         :label label
                         :args args
                         :argsmap argsmap
                         :plant-id plant-id
                         :plant-part plant-part
                         :plant-interface plant-interface
                         ;; add constraints on the activity
                         :constraints (if tc #{(:uid tc)})
                         :htn-node (:uid hem-map)
                         :end-node (:uid se)
                         ;; method metadata
                         :cost cost
                         :reward reward
                         :controllable controllable}))
            _ (when activity
                (dbg-println :trace "activity" (:uid activity) (:name activity)
                  "getting :label" (and (not primitive?) label)))
            _ (dbg-println :debug "ACTIVITY TCs" (:constraints activity))
            sb (tpn/tpn-state {:activities (if activity #{(:uid activity)})
                               ;; do NOT add constraints on the node
                               ;; for primitive? activities
                               :constraints (if (and tc (not primitive?))
                                              #{(:uid tc)})
                               ;; WHILE we ONLY want :end-node for sequences, we
                               ;; need it during construction and will remove it
                               ;; during TPN optimization
                               ;; :end-node (if sequential? (:uid se))
                               :end-node (:uid se)
                               ;; re-use an activity se as a sequence-end
                               ;; :sequence-end (if (and sequential? (not activity))
                               ;;                 (:uid se))
                               ;; :sequence-end (if sequential? (:uid se))
                               :htn-node (:uid hem-map)})
            _ (dbg-println :debug "SB TCs" (:constraints sb))
            [begin end] (if parallel?
                          (if (zero? i)
                            (let [end (tpn/tpn-p-end {})
                                  ;; comment out for issue-120
                                  ;; tc (if (and tc (not primitive?))
                                  ;;      (tpn/update-tpn-plan-map!
                                  ;;        (assoc tc :end-node (:uid end))))
                                  begin (tpn/tpn-p-begin
                                          {:end-node (:uid end)
                                           ;; comment out for issue-120
                                           ;; :constraints
                                           ;; (if tc #{(:uid tc)})
                                           :htn-node
                                           (:uid hem-map)})]
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
                                           :htn-node (:uid hem-map)})]
                              [begin end])
                            [sb se]))]
        _ (dbg-println :debug "BEGIN TCs" (:constraints begin))
        (dbg-println :trace "begin" (:uid begin) "end" (:uid end)
          "sb" (:uid sb) "se" (:uid se))
        (when label
          (dbg-println :debug "STlabel" label
            (if primitive?
              "on ACTIVITY"
              (if choice? "on BEGIN" "on SB")))
          (swap! labels assoc label
            (if choice?
              [(:uid begin) (:uid end)]
              [(:uid sb) (:uid se)]))
          (when-not primitive?
            (tpn/update-tpn-plan-map!
              (assoc (if choice? begin sb) :label label))))
        (when (and (zero? i) choice?)
          (dbg-println :debug "NOTE choice-begin-end will be SET"
            [(:uid begin) (:uid end)]))
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
          (reset! first-begin-uid (:uid begin))
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
          (let [na (tpn/tpn-null-activity {:end-node (:uid begin)})
                {:keys [activities sequence-end]} @prev-end]
            (tpn/update-tpn-plan-map!
              (assoc @prev-end :activities (conj activities (:uid na))))
            (tpn/update-tpn-plan-map!
              (assoc (tpn/get-tpn-plan-map @first-begin-uid)
                :sequence-end (:uid end)))))
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
                display-name (if parallel? nil (str (inc i)))
                hedge (htn-edge {:end-node (:uid task-expansion)
                                 :display-name display-name
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
            (construct-hem-plan-map ir pargs task-expansion subtask_ false
              (:uid sb) labels all-betweens pca
              (if (and (zero? i) choice?)
                [(:uid begin) (:uid end)])))) ;; doseq task-expansions
        )) ;; doseq subtasks
    ))

;; pca is the pclass-instance ancestry
(defn construct-htn-plan-map [ir expanded-root-task pca]
  ;;(dbg-println :debug expanded-root-task)
  (let [{:keys [uid display-name pargs]} expanded-root-task
        ;; _ (dbg-println :debug "ERT PARGS" pargs)
        net (htn-network {:display-name display-name :rootnodes #{uid}})
        {:keys [task-expansions irks]} expanded-root-task
        betweens (get-in ir (conj irks :betweens))
        _ (dbg-println :debug "ROOT CHPM IRKS" irks "betweens" betweens)
        ert (assoc
              (dissoc expanded-root-task :pclass :pargs :arguments
                :task-expansions :name
                :task-type :ancestry-path :temporal-constraints
                :irks)
              :incidence-set #{}
              :edges [])
        _ (dbg-println :debug "ERT" ert
            "\nTASK-EXPANSIONS (" (count task-expansions)
            ") = " ;; (with-out-str (pprint task-expansions)
            (mapv :display-name task-expansions))
        labels (atom {})
        all-betweens (atom [])
        top-level-choose? (> (count task-expansions) 1)
        subtask-constraints (:subtask-constraints (first task-expansions))
        sequence? (and (= (count task-expansions) 1)
                    subtask-constraints
                    (= (count subtask-constraints) 1)
                    (= (:type (first subtask-constraints))
                      :task-sequential-constraint))
        end ((if top-level-choose?
               tpn/tpn-c-end
               tpn/tpn-state)
             {})
        begin ((if top-level-choose?
                 tpn/tpn-c-begin
                 tpn/tpn-state)
               ;; WHILE we ONLY want :end-node for sequences, we
               ;; need it during construction and will remove it
               ;; during TPN optimization
               ;; {:end-node (if (or top-level-choose? sequence?) (:uid end))})]
               {:end-node (:uid end)
                :sequence-end (if sequence? (:uid end))})]
    (update-htn-plan-map! ert)
    (update-htn-plan-map! net)
    (swap! *htn-plan-map* assoc :network (:uid net)) ;; ENTRY POINT
    (swap! tpn/*tpn-plan-map* assoc
      :network-id (:uid (tpn/tpn-network
                          {:begin-node (:uid begin)
                           :end-node (:uid end)})))
    (when betweens
      (swap! all-betweens concatv betweens))
    ;; now walk the graph
    (if (pos? (count task-expansions))
      (doseq [task-expansion task-expansions]
        (construct-hem-plan-map ir pargs task-expansion ert true (:uid begin)
          labels all-betweens pca nil)))
    _ (dbg-println :debug "ADD-BETWEEN-CONSTRAINTS"
        (with-out-str (pprint @labels)) "\nAB" @all-betweens)
    (tpn/add-between-constraints labels @all-betweens)
    (tpn/optimize-tpn-map)
    @*htn-plan-map*))

(defn get-pclass-for-field [ir mpclass field]
  ;;If the :initial map is an :arg-reference, then there is
  ;;no :pclass available and plant-class will be nil.  However, at
  ;;this point, we don't yet have a root task, so we can't expand
  ;;the :arg-reference
  (let [initial_ (get-in ir [mpclass :fields field :initial])]
    (case (:type initial_)
      :pclass-ctor (:pclass initial_)

      :arg-reference
      (let [arg (:name initial_)
            arg-position (vec-index-of (get-in ir [mpclass :args]) arg)
            all-pclasses (keys ir)]
        (loop [pclasses [] pc (first all-pclasses) more (rest all-pclasses)]
          (if-not pc
            (if (= (count pclasses) 1)
              (first pclasses)
              :unknown-pclass)
            (let [pclass (if (not= pc mpclass) pc)
                  fields (if pclass (keys (get-in ir [pclass :fields])))
                  pcs (vec
                        (remove nil?
                          (mapv
                            #(let [initial (get-in ir
                                             [pclass :fields % :initial])
                                   ctor-arg (if (and (= (:type initial)
                                                       :pclass-ctor)
                                                  (= (:pclass initial) mpclass))
                                              (get (:args initial)
                                                arg-position))]
                               (if ctor-arg
                                 (get-in ir
                                   [pclass :fields ctor-arg :initial :pclass])))
                            fields)))
                  pclasses (if (not (empty? pcs))
                             (concatv pclasses pcs)
                             pclasses)
                  pc (first more)
                  more (rest more)]
              (recur pclasses pc more)))))
      :unknown-pclass)))

;; returns [plant-class method non-primitive?]
(defn get-method-for-task [ir mpclass caller-arity method-ref]
  (dbg-println :trace "GMFT method-ref" method-ref)
  (let [{:keys [type names]} method-ref
        rv (case type
             :field-ref
             ;; assume field has a pclass-ctor
             ;; FIXME single deref only for the moment
             (let [[field method] names
                   initial (get-in ir [mpclass :fields field :initial])
                   _ (dbg-println :trace "  INITIAL" initial "METHOD" method)
                   pclass (if (= :pclass-ctor (:type initial))
                            (:pclass initial))
                   _ (dbg-println :trace "  PCLASS" pclass)
                   pclasses-with-method (if-not pclass
                                          (match-pclasses-method-arity
                                            ir method caller-arity))
                   _ (dbg-println :trace "  PWM0" pclasses-with-method)
                   ;; assume the method is NOT in this class if there is
                   ;; more than one
                   pclasses-with-method (if (= 2 (count pclasses-with-method))
                                          (remove #(= % mpclass)
                                            pclasses-with-method)
                                          pclasses-with-method)
                   _ (dbg-println :trace "  PWM1" pclasses-with-method)
                   pclass (if pclass
                            pclass
                            (if (= (count pclasses-with-method) 1)
                              (first pclasses-with-method)
                              ;;If more than one, the method is ambigious!
                              (fatal-error "cannot disambiguate method-ref"
                                method-ref "for arity" caller-arity)))
                   _ (dbg-println :trace "  in mpclass" mpclass "field" field
                       "initial" initial
                       "pclass" pclass "method" method)
                   [mi mdef] (match-method-arity
                               caller-arity
                               (get-in ir [pclass :methods method]))
                   non-primitive? (not
                                    (get-in ir
                                      [pclass :methods method mi :primitive]))]
               [pclass method non-primitive?])
             :symbol-ref ;; expect this to be a local method
             (let [_ (assert (= 'this (first names))
                       ":symbol-ref in :method-fn does not refer to this pclass")
                   method (second names)
                   [mi mdef] (match-method-arity
                               caller-arity
                               (get-in ir [mpclass :methods method]))
                   non-primitive? (not
                                    (get-in ir
                                      [mpclass :methods method mi :primitive]))]
               [mpclass method non-primitive?])
             :method-arg-ref
             ;; At this point in constructing the HTN we do not have the real
             ;; call graph so we must simply match based on method name
             ;; and arity (very weak).
             (let [[marg method] names
                   pclasses-with-method (match-pclasses-method-arity
                                          ir method caller-arity)
                   ;; assume the method is NOT in this class if there is
                   ;; more than one
                   pclasses-with-method (if (= 2 (count pclasses-with-method))
                                          (remove #(= % mpclass)
                                            pclasses-with-method)
                                          pclasses-with-method)
                   pclass (if (= (count pclasses-with-method) 1)
                            (first pclasses-with-method)
                            ;;If more than one, the method is ambigious!
                            (fatal-error "cannot disambiguate method-ref"
                              method-ref "for arity" caller-arity))
                   [mi mdef] (match-method-arity
                               caller-arity
                               (get-in ir [pclass :methods method]))
                   non-primitive? (not
                                    (get-in ir
                                      [pclass :methods method mi :primitive]))]
               [pclass method non-primitive?])
             :pclass-arg-ref
             ;; At this point in constructing the HTN we do not have the real
             ;; call graph so we must simply match based on method name
             ;; and arity (very weak).
             (let [[parg method] names
                   pclasses-with-method (match-pclasses-method-arity
                                          ir method caller-arity)
                   ;; assume the method is NOT in this class if there is
                   ;; more than one
                   _ (dbg-println :trace "  pclasses-with-method"
                       pclasses-with-method)
                   pclasses-with-method (if (= 2 (count pclasses-with-method))
                                          (remove #(= % mpclass)
                                            pclasses-with-method)
                                          pclasses-with-method)
                   pclass (if (= (count pclasses-with-method) 1)
                            (first pclasses-with-method)
                            ;;If more than one, the method is ambigious!
                            (fatal-error "cannot disambiguate method-ref"
                              method-ref "for arity" caller-arity))
                   [mi mdef] (match-method-arity
                               caller-arity
                               (get-in ir [pclass :methods method]))
                   non-primitive? (not
                                    (get-in ir
                                      [pclass :methods method mi :primitive]))]
               [pclass method non-primitive?])
             (fatal-error "not implemented yet method-ref type:" type
               "method-ref" method-ref)
             )]
    (dbg-println :trace "  rv=>" rv)
    rv))

;; will return subtasks, if any
;; irks are the ks to get-in the ir for the method in question (initially nil)
(defn get-htn-method-subtasks
  [ir pargs top-level-type mpclass mname mi display-name margs sub-body irks]
  (dbg-println :debug "\nGET-HTN-METHOD-SUBTASKS"
    "top-level-type" top-level-type "mpclass" mpclass "mname" mname "mi" mi
    "display-name" display-name "margs" margs "sub-body" sub-body "irks" irks)
  (let [number-of-subtasks (count sub-body)]
    (loop [subtasks []
           i 0]
      (if (= i number-of-subtasks)
        [subtasks (if (= top-level-type :sequence)
                    [(task-sequential-constraint {:tasks subtasks})]
                    [])]
        (let [subtask-definition (get sub-body i)
              irks-i (conj irks i)
              {:keys [type name field method-ref args condition
                      primitive body temporal-constraints]} subtask-definition
              caller-arity (count args)

              _ (dbg-println :debug "GET-HTN-METHOD-SUBTASKS" i "TYPE" type
                  "NAME" name "FIELD" field "METHOD-REF" method-ref
                  "\nMPCLASS" mpclass "MNAME" mname "NAME" name
                  "PRIMITIVE" primitive
                  "\nMARGS" margs "ARGS" args
                  "BODY" body
                  "TC" temporal-constraints)

              subtask
              (cond
                (= :method-fn type)
                ;; if the value of plant-fn-primitive? is nil we know
                ;; we have the wrong plant-class (because the IR always
                ;; has true or false)
                (let [_ (assert args "ARGS is nil")
                      [plant-class method non-primitive?]
                      (get-method-for-task ir mpclass caller-arity method-ref)]
                  ;; This is the subtask that will be returned
                  ;; (and added to the method)
                  ((if non-primitive?
                     htn-nonprimitive-task
                     htn-primitive-task)
                   {:pclass plant-class
                    :name method
                    :arguments args
                    :temporal-constraints temporal-constraints
                    :irks irks-i}))

                (reserved-conditional-method-type? type)
                (htn-primitive-task
                  {:name (symbol (clojure.core/name type))
                   :pclass mpclass
                   :display-name (display-name-string type)
                   :arguments [condition] ;;Condition is a map
                   :temporal-constraints temporal-constraints
                   :irks irks-i})

                (= :delay type)
                (htn-primitive-task
                  {:name 'delay
                   :display-name "Delay"
                   :temporal-constraints temporal-constraints
                   :irks irks-i})

                (#{:parallel :sequence :choose} type)
                (fatal-error (str mpclass "." mname) ":"
                  "\nEmbedding a" type
                  "is not supported within a defpmethod when used for HTN generation")

                :else
                (fatal-error "Unexpected type:" type))]
          (recur (if subtask (conj subtasks subtask) subtasks)
            (inc i))
          ))
      )))

;; create htn-methods as a side effect)
(defn make-htn-methods
  "Construct the HTN Method(s) for mname"
  [ir pargs top-level? mpclass mname mi display-name
   display-args margs mbody & [irks]]
  (dbg-println :debug "MHM " "TOP-LEVEL?" top-level? "MPCLASS" mpclass
    "MNAME" mname "MI" mi
    "DISPLAY-NAME" display-name
    "DISPLAY-ARGS" display-args
    "MARGS" margs
    "IRKS" irks "\nMBODY" mbody)
  (let [irks (conj (or irks [mpclass :methods mname mi]) :body)
        number-of-subtasks (count mbody)
        first-task-type (:type (get mbody 0))]
    (assert (or (not top-level?)
              (= number-of-subtasks 1))
      "There should only be one top-level form")
    (let [irks-0 (conj irks 0)
          nonprimitive-task (htn-nonprimitive-task
                              {:pclass mpclass
                               :name mname
                               :display-name display-name ;;Necessary???
                               :display-args display-args
                               :arguments margs
                               ;; No need for :temporal-constraints?
                               :irks irks-0 ;;TODO - verify
                               })

          methods
          (cond ;;returns a vector of methods
            (#{:method-fn :delay} first-task-type)
            (let [[subtasks subtask-constraints]
                  (get-htn-method-subtasks ir pargs first-task-type
                    mpclass mname mi
                    display-name margs mbody irks)
                  method (htn-method {:pclass mpclass
                                      :name mname
                                      :display-name display-name
                                      :display-args display-args
                                      :nonprimitive-task nonprimitive-task
                                      :subtasks subtasks
                                      :subtask-constraints subtask-constraints
                                      :irks irks-0 ;;TODO - verify
                                      })]
              [method])

            (#{:sequence :parallel} first-task-type)
            (let [[subtasks subtask-constraints]
                  (get-htn-method-subtasks ir pargs first-task-type
                    mpclass mname mi
                    display-name margs
                    (get-in mbody [0 :body])  ;;get the :body of the seq/par
                    (concatv irks [0 :body]))
                  method (htn-method {:pclass mpclass
                                      :name mname
                                      :display-name display-name
                                      :display-args display-args
                                      :nonprimitive-task nonprimitive-task
                                      :subtasks subtasks
                                      :subtask-constraints subtask-constraints
                                      :irks irks-0 ;;TODO - verify
                                      })]
              [method])

            (= first-task-type :choose)
            (let [choose-body (get-in mbody [0 :body])
                  num-of-choices (count choose-body)]
              (loop [methods []
                     i 0]
                (if (= i num-of-choices)
                  methods
                  (let [choice-body (get-in choose-body [i :body])
                        irks-choice (concatv irks [0 :body i :body])
                        choice-name (symbol (str mname "-choice-" i))
                        choice-display-name (str display-name "-" i) ;;FIX

                        [subtasks subtask-constraints]
                        (get-htn-method-subtasks ir pargs first-task-type
                          mpclass choice-name 0
                          choice-display-name margs choice-body irks-choice)

                        method (htn-method {:pclass mpclass
                                            :name choice-name
                                            :display-name choice-display-name
                                            :nonprimitive-task nonprimitive-task
                                            :subtasks subtasks
                                            :subtask-constraints subtask-constraints
                                            :irks irks-choice
                                            :order i})]
                    (recur (conj methods method) (inc i))))))

            :else
            (do
              (fatal-error "TBD: :else")
              []))]
      (dbg-println :trace "HTN-METHODS " (with-out-str (pprint methods)))
      methods)))

;; Creates an htn data structure in Clojure (optionally converted to JSON in cli.clj)
;; NOTE: root-task is a string (or nil)!!!
;; Examples
;;   "(isr-htn.main \"A\" \"B\" \"C\" \"D\")"
;;   "(isr-htn.get-data-and-interpret \"A\" \"B\")"
(defn plan-htn
  "Weaves a 'plan' from the root task, using the HTN methods."
  [ir root-task file-format output]
  (reset! *debug-ir* ir)
  (reinitialize-htn-method-table)
  (reinitialize-htn-object-table)
  (reinitialize-htn-plan-map)
  (tpn/reinitialize-tpn-plan-map)
  (reinitialize-pclass-instances)
  (let [[pclass pargs method mi args pca] (identify-root-task ir root-task)
        _ (dbg-println :info "PAMIA" pclass pargs "METHOD" method mi
            "\n  ARGS" (with-out-str (pprint args))
            "\n  PCA" pca)
        _ (dbg-println :trace "PCLASS-INSTANCES===\n"
            (with-out-str (pprint @*pclass-instances*)) "===")
        _ (transform-htn ir pargs)
        nonprimitive-root-task (htn-nonprimitive-task
                                 {:pclass pclass
                                  :pargs pargs
                                  :name method
                                  :arguments args
                                  :irks [pclass :methods method mi]})
        expanded-root-task (make-expanded-task nonprimitive-root-task)
        is-stdout? (stdout? output)
        output-prefix (if is-stdout? "STDOUT" output)
        htn-filename (if is-stdout? "-" (str output-prefix ".htn." file-format))
        tpn-filename (if is-stdout? "-" (str output-prefix ".tpn." file-format))
        _ (plan-htn-task ir expanded-root-task)
        ;; _ (pprint-htn-methods)
        htn (construct-htn-plan-map ir (get-htn-object expanded-root-task) pca)
        tpn @tpn/*tpn-plan-map*]
    (log/info "Saving HTN to" htn-filename "and TPN to" tpn-filename)
    (output-file htn-filename file-format htn)
    (output-file tpn-filename file-format tpn)
    0)) ;; exit code
