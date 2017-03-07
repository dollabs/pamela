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
            [camel-snake-kebab.core :as translate]
            [instaparse.core :as insta]
            [pamela.utils :refer [stdout? output-file display-name-string dbg-println]]
            [pamela.parser :as parser]
            [pamela.tpn :as tpn]))

; local implementation of gensym so that we get predictable uids in generated plans.
(defonce my-count (atom 0))

(defn my-gensym [prefix]
  (str prefix (swap! my-count inc)))

(defn reset-my-count []
  (reset! my-count 0))

;; definitions -----------------------------------------------

;; The following is intended (only) for the support of the EdgeCT program.
;; So, we'll likely move this somewhere else.
(def network-flow-types #{"VOIP" "File Xfer" "File Transfer" "VTC" "VideoStream"})

;; vars -------------------------------------------------------

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
      ;; (dbg-println :debug "ADDING HTN-METHOD" pclass name) ;; DEBUG
      (swap! *htn-methods* assoc-in [pclass name] method))))

(defn find-htn-method [pclass name & [error-if-not-found?]]
  (let [method (get-in @*htn-methods* [pclass name])]
    (when (and (not method) error-if-not-found?)
      (throw (AssertionError. (str "method not found: " name " in pclass " pclass))))
    method))

;; HTN Objects
;; HTN Objects are all of the UID-identified objects in our HTN world

;; {uid htn-object}
(def ^:dynamic *htn-objects* (atom {}))

(defn reinitialize-htn-object-table [] (reset! *htn-objects* {}))

(defn get-htn-object [uid-or-object] (let [uid (if (keyword? uid-or-object) uid-or-object (:uid
  uid-or-object))] (get @*htn-objects* uid)))

(defn update-htn-object! [object] (let [uid (:uid object)] (swap! *htn-objects* assoc uid object)
  object))

;; HTN Plan Map contains the expanded HTN structure, based on expansion of a root task and a library
;; of HTN methods

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

;; FUTURE: (optionally) add pclass to name-with-args
(def name-with-args-max-line-length
  "The (preferred) maximimum line length for Task/Method Names including arguments"
  25)

;; TODO: This will be OBE, once Planviz can dynamically hide/show the args, based on user setting.
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
  (let [{:keys [pclass name arguments]} object]
    (str pclass)
    ;; (add-newlines-if-needed
    ;;   (apply str "NWA-" ;; DEBUG
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
      ;; "NWAM-" ;; DEBUG
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
        get-dyn-arg (fn [arg]
                      (let [dyn-arg (get argument-mappings arg)]
                        (if (map? dyn-arg)
                          (:param dyn-arg)
                          dyn-arg)))
        argvals (apply str (interpose " " (map get-dyn-arg arguments)))]
    (add-newlines-if-needed
      ;; DEBUG
      (str
        ;; "NWAH- " ;; DEBUG
        method-name
        (if name-with-args-include-args? "(")
        (if name-with-args-include-args? argvals)
        (if name-with-args-include-args? ")"))
      (or max-line-length name-with-args-max-line-length))))



(defn resolve-arguments [arguments ancestry-path pargs]
  (let [hem (get-htn-object (first ancestry-path))
        {:keys [argument-mappings]} hem
        resolve-arg (fn [arg]
                      (if (variable? arg)
                        (get argument-mappings arg)
                        arg))
        _ (dbg-println :trace "RA (before)" arguments)
        arguments (map resolve-arg arguments)
        _ (dbg-println :trace "RA (after)" arguments)
        unresolved (count (filter variable? arguments))
        get-dyn-arg (fn [dyn-arg]
                      (if (map? dyn-arg)
                        (:param dyn-arg)
                        dyn-arg))
        argvals (if (pos? unresolved)
                  (resolve-arguments arguments (nthrest ancestry-path 2) pargs)
                  (mapv get-dyn-arg arguments))]
    (dbg-println :trace "RA UNRESOLVED" unresolved "ARGVALS" argvals)
    argvals))

;; The younger cousin to resolve-arguments
(defn resolve-argument [argument argument-mapping]
  (if (variable? argument)
    (get argument-mapping argument :unresolved-argument)
    argument))


;; returns pclass_ctor_ map
;; (defn resolve-plant-class [ir hem-pclass pargs pclass method
;;                            task-type irks henpt ancestry-path]
(defn resolve-plant-class [ir hem-pclass pargs henpt subtask_]
  (let [{:keys [type pclass name arguments irks ancestry-path]} subtask_
        task-type type
        hem (get-htn-object (first ancestry-path))
        {:keys [argument-mappings]} hem
        ;; DEBUG
        _ (dbg-println :debug "  RPC0" pclass "METHOD" name "IRKS" irks
            "\n    RPCHEM" (pr-str (dissoc hem :subtasks :ancestry-path
                                 :expansion-method :subtask-constraints))
            "\n    hem-pclass" hem-pclass
            "pargs" pargs
            "pclass" pclass
            "task-type" task-type
            "(:pclass henpt)" (:pclass henpt)
            ;;"ancestry-path" ancestry-path
            )
        _ (if-not (htn-isa? task-type :htn-nonprimitive-task)
            (dbg-println :debug "  RPC HENPT" (dissoc henpt :subtasks :ancestry-path
                                     :expansion-method :subtask-constraints
                                     :task-expansions)))
        plant-fn_ (get-in ir irks)
        _ (dbg-println :debug "plant-fn_" plant-fn_ "argument-mappings"
            (pr-str argument-mappings))
        {:keys [type name field method args]} plant-fn_
        pclass-ctor_ (cond
                       (= type :plant-fn-field)
                       (let [initial_ (get-in ir [hem-pclass :fields field :initial])
                             {:keys [type name]} initial_]
                         (case type
                           :pclass-ctor initial_
                           :arg-reference (first (filter #(= (:param %) name) pargs))
                           {:error :unresolved-field}))

                       (= type :plant-fn-symbol)
                       (let [method-arg (get argument-mappings name)
                             pclass-arg (if-not method-arg
                                          (first (filter #(= (:param %) name) pargs)))
                             henpt-irks (:irks henpt)
                             caller_ (get-in ir henpt-irks)
                             caller-pclass (first henpt-irks)]
                         (dbg-println :trace "RPC name" name "method-arg"
                                      method-arg "pclass-arg" pclass-arg)
                         (or method-arg pclass-arg
                             (if (= (:type caller_) :plant-fn-field)
                               (get-in ir [caller-pclass :fields (:field caller_) :initial]))
                             (if (= name 'this)
                               ;;Use the ancestry-path to resolve the pclass of the caller
                               (let [[this-hem
                                      parent-subtask_
                                      parent-hem
                                      parent-henpt] ancestry-path
                                     ;; parent-subtask_ (nth ancestry-path 1)
                                     ;; parent-hem (nth ancestry-path 2)
                                     ;; parent-henpt (nth ancestry-path 3)
                                     parent-hem-pclass (get-in parent-hem [:irks 0])
                                     ;; st 0
                                     ;; parent-subtask_ (get-in parent-hem [:subtasks st])

                                     _ (dbg-println :debug "RPC Recursing on ancestry-path"
                                         (mapv #(list (:uid %) (or (:label %) (:name %)))
                                           ancestry-path))
                                     _ (dbg-println :debug "parent-hem-pclass" parent-hem-pclass)
                                     ;; {:error :unresolved-pclass-ctor-need-ancestry-path}
                                     pc_ (resolve-plant-class
                                           ir parent-hem-pclass pargs parent-henpt parent-subtask_)

                                     ]

                                 ;; (dbg-println :debug "RPC this"
                                 ;;              (with-out-str (pprint ancestry-path)))
                                 ;; (dbg-println :debug "RPC parent-hem"
                                 ;;              (with-out-str (pprint parent-hem))
                                 ;;              "\nparent-henpt"
                                 ;;              (with-out-str (pprint parent-henpt)))
                                 _ (dbg-println :debug "RPC Recursion result" pc_)
                                 pc_
                                 ))
                             ;;In the case of this.methodname, we need to find the pclass-ctor
                             ;; (if (and (= (:type caller_) :plant-fn-symbol)
                             ;;          (= (:name caller_) 'this))
                             ;;   )
                             {:error :unresolved-pclass-ctor}))

                       (= type :delay)
                       {}

                       :else
                       (do
                         (dbg-println :error "ERROR, not currently supported plant-fn type=" type)
                         nil))
        _ (dbg-println :trace "RPC pclass-ctor_" pclass-ctor_)
        ;; pclass-ctor_ (cond
        ;;                (= pclass ::unknown) ;; matching method in pargs?
        ;;                ;; (let [plant-fn_ (get-in ir irks)
        ;;                ;;       param (:name plant-fn)
        ;;                ;;       pc (first (filter #(= (:param %) param) pargs))
        ;;                ;;       plant (:pclass pc)
        ;;                ;;       plant-method (get-in ir [plant :methods name])]
        ;;                ;;   (println "UNKNOWN PARAM" param ;; DEBUG
        ;;                ;;     "PLANT" plant
        ;;                ;;     "PLANT-METHOD" plant-method
        ;;                ;;     "PC" pc)
        ;;                ;;   (if (and plant-method pc)
        ;;                ;;     pc ;; plant parameter
        ;;                ;;     param)) ;; must be a method parameter
        ;;                (do
        ;;                  (println "ERROR, not currently supported" pclass)
        ;;                  nil)
        ;;                (keyword? pclass) ;; field
        ;;                (let [initial (get-in ir
        ;;                                [hem-pclass :fields
        ;;                                 pclass :initial])
        ;;                      {:keys[type name id plant-part interface]} initial
        ;;                      initial-pclass (:pclass initial)]
        ;;                  (cond
        ;;                    (= type :arg-reference) ;; look in pargs
        ;;                    ;; (first (filter #(= (:param %) name) pargs))
        ;;                    (do
        ;;                      (println "ERROR, not currently supported :arg-reference")
        ;;                      nil)
        ;;                    (= type :pclass-ctor)
        ;;                    ;; initial-pclass
        ;;                    initial
        ;;                    :else
        ;;                    ;; pclass
        ;;                    (do
        ;;                      (println "ERROR, not currently supported keyword pclass without ctor" pclass)
        ;;                      nil)

        ;;                    ))
        ;;                (and (not (htn-isa? task-type :htn-nonprimitive-task))
        ;;                  henpt (:pclass henpt))
        ;;                ;; (:pclass henpt)
        ;;                (do
        ;;                  (println "ERROR, not currently supported henpt" pclass)
        ;;                  nil)
        ;;                :else
        ;;                ;; pclass
        ;;                (do
        ;;                  (println "ERROR, not currently supported raw pclass" pclass)
        ;;                  nil))
        ;;  {:keys [argument-mappings]} hem
        ;; _ (println "  RPC1" pclass "argument-mappings" argument-mappings)
        ;; resolve-pclass (fn [arg]
        ;;                  (if (and (variable? arg)
        ;;                        (not (empty? argument-mappings)))
        ;;                    (get argument-mappings arg arg)
        ;;                    arg))
        ;; pclass (resolve-pclass pclass)
        ;; _ (println "  RPC2" pclass) ;; DEBUG
        ;; pclass (if (variable? pclass)
        ;;          (resolve-pclass ir hem-pclass pargs p name task-type
        ;;            irks nil (nthrest ancestry-path 2))
        ;;          pclass)
        ;; primitive? (if pclass (get-in ir [pclass :names name :primitive]))
        ;; _ (println "  RPC3" pclass "primitive?" primititive?) ;; DEBUG
        ;; REVIEW BELOW...
        ;; ;; if name is not primitive in p then return nil to
        ;; ;; indicate this is NOT actually a primitive method
        ;; p (if (and (map? p)
        ;;         (= (:type p) :pclass-ctor)
        ;;         (get-in ir [(:pclass p) :methods name :primitive]))
        ;;     p)
        ;; FIX recursive argument resolution later
        ]
    pclass-ctor_))

;; name-with-argvals multi-methods --------------------------------------

;; (defn name-with-argvals-dispatch [object]
;;   (:type object))

;; (defmulti name-with-argvals
;;   "Dispatch based on object type"
;;   #'name-with-argvals-dispatch
;;   :default :htn-object
;;   :hierarchy #'htn-hierarchy)

;; (defmethod name-with-argvals :htn-object
;;   [object]
;;   (str (:uid object)))


;; ;;Commented out since we've change the signature for resolve-arguments
;; ;; (defmethod name-with-argvals :htn-task
;; ;;   [object]
;; ;;   (let [{:keys [name arguments ancestry-path]} object
;; ;;         arguments (or arguments [])
;; ;;         unresolved (count (filter variable? arguments))
;; ;;         argvals (if (pos? unresolved)
;; ;;                   (resolve-arguments arguments ancestry-path)
;; ;;                   arguments)]
;; ;;     (apply str name (conj (vec (conj (interpose ", " argvals) "(")) ")"))))

;; (defmethod name-with-argvals :htn-method
;;   [object]
;;   (let [{:keys [name nonprimitive-task]} object
;;         method-name (str name)
;;         {:keys [name arguments]} nonprimitive-task]
;;     (str
;;       method-name
;;       (str name (or (seq arguments) '())))))

;; (defmethod name-with-argvals :htn-expanded-method
;;   [object]
;;   (let [{:keys [expansion-method argument-mappings]} object
;;         {:keys [name nonprimitive-task]} expansion-method
;;         method-name (str name)
;;         {:keys [name arguments]} nonprimitive-task
;;         argvals (apply str (interpose " " (map #(get argument-mappings %) arguments)))]
;;     (str method-name "(" argvals ")")))


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

;; Resources are a domain-specific legacy attribute.  TODO: We may want to generalize this.
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

    ;; IF all-methods empty
    ;; AND this is found to be a non-primitive method in another (plant) pclass
    ;; THEN consider for a match

(defn pprint-htn-methods []
  (let [htn-methods @*htn-methods*
        pclasses (sort (keys htn-methods))]
    (dbg-println :debug "PPRINT-HTN-METHODS")
    (doseq [pclass pclasses]
      (dbg-println :debug "PCLASS" pclass "METHODS" (sort (keys (get htn-methods pclass)))))))

;; returns a sequence of [k v] pairs containing
;; methods which match name in pclass
;; or, if empty, matches in all other pclasses
(defn find-candidate-methods [ir pclass name]
  (let [htn-methods @*htn-methods*
        pclasses (keys htn-methods)
        all-pclasses (set (keys ir))
        other-pclasses (set/difference all-pclasses (set pclasses))]
    ;; DEBUG
    (dbg-println :debug "FCM PCLASS" pclass "NAME" name "PCLASSES" pclasses "OTHER-PCLASSES" other-pclasses)
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
                (dbg-println :debug "NO CANDIDATES!") ;; DEBUG
                []
                )
              )))
        (let [pc-methods (get htn-methods pc)
              ms (keys pc-methods)
              _ (dbg-println :debug "  FCM PC" pc) ;; DEBUG
              matches
              (loop [matches [] m (first ms) moar (rest ms)]
                (dbg-println :debug "    FCM METHOD" m) ;; DEBUG
                (if-not m
                  matches
                  (let [method (get pc-methods m)
                        npt-name (get-in method [:nonprimitive-task :name])
                        ;; npt-args (get-in method [:nonprimitive-task :arguments])
                        ;; _ (dbg-println :debug "FCM ARGUMENTS" arguments
                        ;;     "NPT-ARGS" npt-args
                        ;;     "MATCH?" (= arguments npt-args))
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
        ;; all-methods (seq (get @*htn-methods* pclass))]
        ;; all-methods (if (htn-isa? type :htn-nonprimitive-task)
        ;;               (find-candidate-methods ir pclass name))
        all-methods (if (nil? pclass)
                      (do (dbg-println :error "FMTET pclass is nil!!!") [])
                      (if (htn-isa? type :htn-nonprimitive-task)
                        (find-candidate-methods ir pclass name)))]
    (dbg-println :debug "FMTET" name "TYPE" type ;; DEBUG
             "PCLASS" pclass "ALL" (map first all-methods))
    (when-not (empty? all-methods)
      (loop [methods [] [mname method] (first all-methods) more (rest all-methods)]
        (if-not mname
          (do
            (dbg-println :debug "  FMTETMETHODS" (map :name methods)) ;; DEBUG
            methods
            )
          (let [{:keys [nonprimitive-task]} method
                compatible? (fn [[task-arg method-arg]]
                              (or (variable? task-arg)
                                  (variable? method-arg)
                                  (= task-arg method-arg)))
                match? (and (= name (:name nonprimitive-task))
                            (= (count arguments) (count (:arguments nonprimitive-task)))
                            (every? compatible?
                                    (map vector arguments (:arguments nonprimitive-task))))
                ;; DEBUG
                _ (dbg-println :debug "  FM NAME" name "NPT-NAME" (:name nonprimitive-task)
                           "ARGS" arguments "NTARGS" (:arguments nonprimitive-task)
                           "match?" match?)
                methods (if match? (conj methods method) methods)]
            (recur methods (first more) (rest more))))))))

;; return args-mappings map
(defn arg-mappings-for-task-and-method [task method]
  (let [static-task-args (get-in method [:nonprimitive-task :arguments])
        dynamic-task-args (:arguments task)]
    (dbg-println :debug "AMFTAM" (:uid method) "name" (:name method))
    (dbg-println :debug "STATIC-TASK-ARGS" (pr-str static-task-args)) ;; DEBUG
    (dbg-println :debug "DYNAMIC-TASK-ARGS" (pr-str dynamic-task-args)) ;; DEBUG
    (if (or (not (vector? static-task-args))
            (not (vector? dynamic-task-args))
            (not= (count static-task-args) (count dynamic-task-args)))
      (throw
       (AssertionError.
        (str "cannot create arg-mappings with static-task-args: "
             static-task-args " and dynamic-task-args: " dynamic-task-args))))
    ;; (dbg-println :debug (:name task) static-task-args dynamic-task-args (map type dynamic-task-args))
    (zipmap static-task-args dynamic-task-args)))

(declare htn-primitive-task)
(declare htn-expanded-nonprimitive-task)
(declare copy-temporal-constraints)
(declare htn-expanded-method)

;; NOTE assumes original-task is :htn-primitive-task or :htn-nonprimitive-task
(defn make-expanded-task
  "Make an expanded task"
  [original-task & [argument-mapping]]
  (dbg-println :debug "MET" original-task "args:" (pr-str argument-mapping)) ;; DEBUG
  (let [task-type (:type original-task)
        skeleton-task (dissoc original-task :uid :label)
        task (if (= task-type :htn-primitive-task)
               (htn-primitive-task skeleton-task)
               (htn-expanded-nonprimitive-task skeleton-task))
        {:keys [temporal-constraints arguments]} task
        new-arguments (if argument-mapping
                        ;;(mapv #(get argument-mapping %) arguments)
                        (mapv #(resolve-argument % argument-mapping) arguments)
                        arguments)]
    (dbg-println :debug "  MET TASK" task "NEW-ARGUMENTS" new-arguments) ;; DEBUG
    (update-htn-object!
     (assoc-if task
               :arguments new-arguments
               :temporal-constraints (and temporal-constraints
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
  (throw
   (AssertionError.
    (str "cannot plan-htn-task for object type: " (:type object)))))

;; "Nothing to do for primitive tasks"
(defmethod plan-htn-task :htn-primitive-task
  [ir object])

;;   "Expand"
(defmethod plan-htn-task :htn-expanded-nonprimitive-task
  [ir object]
  (let [task object
        methods (find-methods-that-expand-task ir task)]
    (dbg-println :debug "PHT-TASK" (with-out-str (pprint task)))
    ;; (dbg-println :debug "PHT-TASK" (:uid task) (:type task)) ;; DEBUG
    (binding [*htn-plan-ancestry* (cons task *htn-plan-ancestry*)]
      (doseq [method methods]
        (dbg-println :debug "PHT METHOD" (:name method)
                 "ANCESTRY" (mapv :uid *htn-plan-ancestry*)) ;; DEBUG
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
          (dbg-println :debug "PHT PCLASS" pclass "NAME" name ;; DEBUG
                   "METHOD" (dissoc method :subtasks))
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
                (dbg-println :debug "about to PHT" (:name subtask)) ;; DEBUG
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
  [{:keys [prefix uid ancestry-path pclass pargs name arguments cost task-type probability temporal-constraints
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight
           label display-name incidence-set edges]
    :or {prefix "task-"
         task-type :communications
         display-name label
         temporal-constraints []}}]
  (let [network-flows (if (network-flow-types name)
                        (make-network-flows name flow-characteristics arguments)
                        network-flows)]
    (if (and (not pclass) (not= name 'delay))
      (throw (Exception. (str "htn-task constructed w/o pclass: name " name))))
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
               :incidence-set incidence-set ;; NOTE should be a clojure set
               :edges edges))))

(defn htn-primitive-task
  "Primitive tasks can't be decomposed any further"
  [{:keys [prefix uid ancestry-path pclass pargs name arguments cost task-type probability temporal-constraints
           mission-effectiveness priority resources network-flows
           flow-characteristics mission-task-weight
           label display-name incidence-set edges irks]
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
           label display-name incidence-set edges irks]
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
           label display-name incidence-set edges irks]
    :or {task-expansions []}
    :as options}]
  (let [task (assoc-if (htn-nonprimitive-task (assoc options :prefix (or prefix "henpt-")))
                       :type :htn-expanded-nonprimitive-task
                       :task-expansions task-expansions)
        label (if (empty? label) (name-with-args task) label)]
    (update-htn-object! (assoc task
                               :display-name (if (empty? display-name) label display-name)
                               :label label))))

(defn htn-method
  "An HTN-method is used to decompose a nonprimitive-task into one or more subtasks"
  [{:keys [prefix
           uid
           ancestry-path
           pclass
           name
           display-name
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
        display-name (if (empty? display-name) label display-name)
        method (update-htn-object! (assoc method
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
             :display-name (if (empty? display-name) label display-name)
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
           label display-name
           incidence-set network
           irks]
    :or {prefix "hem-"
         display-name (:display-name expansion-method)
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
        display-name (if (empty? display-name) label display-name)
        hem   (update-htn-object! (assoc hem
                                         :display-name display-name
                                         :label label))]
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

(defn ir-symbols [a b]
  (if (symbol? b)
    [a b]
    (vec (cons a b))))

(def root-task-ir {
                   :argval identity
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
                   :symbols ir-symbols
                   })

;; given a vector of syms, where
;;   the first sym is the pclass to start with
;;   (if any) intermediate syms are fields in the previous pclass
;;   the last sym is method
;; return [pclass pargs method]
;;   pclass may be an error string (instead of a pclass symbol)
;;   pargs are the args required to instanciate the pclass
(defn root-task-pclass-method [ir syms]
  (let [pclass (first syms)
        pclass-def (get ir pclass)
        valid-fields (set (keys (:fields pclass-def)))
        fields (butlast (rest syms))
        method (last syms)]
    (if-not pclass-def
      [(str "invalid root-task: pclass " pclass " does not exist") method]
      (loop [err nil p pclass pa [] f (first fields) more (rest fields)]
        ;; (dbg-println :debug "LOOP ERR" err "P" p "F" f) ;; DEBUG
        (if (or err (not f))
          (if err
            [err [] method]
            [p pa method])
          (let [f-initial (get-in ir [p :fields (keyword f) :initial])
                ;; _ (dbg-println :debug "F-INITIAL" f-initial) ;; DEBUG
                f-type (:type f-initial)
                p (:pclass f-initial)
                p-args (get-in ir [p :args])
                ;; _ (dbg-println :debug "P-ARGS" p-args) ;; DEBUG
                f-args (:args f-initial)
                ;; _ (dbg-println :debug "F-ARGS" f-args) ;; DEBUG
                deref-field-arg (fn [arg p-arg]
                                  ;; (dbg-println :debug "  DFA" arg "VF" valid-fields)
                                  (if (and
                                       (or (keyword? arg)
                                           (symbol? arg))
                                       (valid-fields (keyword arg)))
                                    ;; deref
                                    (assoc
                                     (get-in ir
                                             [pclass :fields (keyword arg) :initial])
                                     :param p-arg) ;; remember formal param
                                    ;; normal arg
                                    arg))
                pa (if (vector? f-args)
                     ;; expand reference to other fields to field-init
                     (mapv deref-field-arg f-args p-args)
                     pa)
                err (if (not= f-type :pclass-ctor)
                      (str "Invalid root task: intermediate field "
                           f " is not a pclass constructor"))]
            (recur err p pa (first more) (rest more))))))))

;; given arg which is a vector of syms (first is pclass, subsequent fields)
;; return the value of the utimate field
;;   or {:error msg}
(defn root-task-arg [ir arg]
  ;; (dbg-println :debug "RTA in" arg) ;; DEBUG
  (let [pclass (first arg)
        pclass-def (get ir pclass)
        fields (rest arg)]
    (if-not pclass-def
      {:error
       (str "invalid root-task argument pclass " pclass " does not exist")}
      (loop [err nil arg nil p pclass f (first fields) more (rest fields)]
        (if (or err (not f))
          (if err
            {:error err}
            arg)
          (let [f-initial (get-in ir [p :fields (keyword f) :initial])
                f-type (:type f-initial)
                p (:pclass f-initial)
                err (if (not= f-type :pclass-ctor)
                      (str "Invalid root task: intermediate field "
                           f " is not a pclass constructor"))
                f (first more)
                the-rest (rest more)
                arg (if (and (not err) (nil? f))
                      f-initial)]
            (recur err arg p f the-rest)))))))

;; given args
;;   which may include a vector of syms (first is pclass, subsequent fields)
;;   replace the sym vector the the value of the referenced field
;; return args as a vector
;;   (args may be an error string)
(defn root-task-args [ir args]
  (loop [rtargs [] arg (first args) more (rest args)]
    (if (or (string? rtargs) (not arg))
      rtargs
      (let [rtarg (if (vector? arg)
                    (root-task-arg ir arg)
                    arg)
            ;; _ (dbg-println :debug "RTA out" rtarg) ;; DEBUG
            rtargs (if (and (map? rtarg) (:error rtarg))
                     (:error rtarg)
                     (conj rtargs rtarg))]
        (recur rtargs (first more) (rest more))))))

;; return [pclass pargs method args]
;; where pargs are needed to instanciate pclass
;; and args are passed to method
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
    ;; (dbg-println :debug "IR" (with-out-str (pprint ir))) ;; DEBUG
    (if (and rir (= (first rir) :error))
      (throw (AssertionError. (second rir)))
      (if rir ;; verify
        (let [;; _ (dbg-println :debug "RIR" (with-out-str (pprint rir)))
              [pclass pargs method] (root-task-pclass-method ir (first rir))
              ;; _ (dbg-println :debug "PCLASS" pclass "PARGS" pargs "METHOD" method)
              args (root-task-args ir (rest rir))
              ;; _ (dbg-println :debug "ARGS" args)
              pclass-def (get ir pclass)
              method-def (if pclass-def (get-in pclass-def [:methods method]))
              method-args (if method-def (get method-def :args))
              ;; _ (dbg-println :debug "MARGS" method-args)
              add-param (fn [arg param]
                          (if (map? arg)
                            (assoc arg :param param)
                            arg))
              args (if (and method-args (= (count args) (count method-args)))
                     (mapv add-param args method-args))
              ;; _ (dbg-println :debug "ARGS" args)
              ]
          (if-not pclass-def
            (let [msg (str "root-task pclass not found: " pclass)]
              (log/error msg)
              (throw (AssertionError. msg)))
            (if (string? args)
              (do
                (log/error args)
                (throw (AssertionError. args)))
              (if-not method-def
                (let [msg (str "root-task pclass " pclass
                               " does not have a method " method)]
                  (log/error msg)
                  (throw (AssertionError. msg)))
                (if (or (not method-args) (not= (count args) (count method-args)))
                  (let [msg (str "root-task args \"" args "\" does not match arity of " pclass "." method " " method-args)]
                    (log/error msg)
                    (throw (AssertionError. msg)))
                  [pclass pargs method args])))))
        (loop [pclass nil method nil k (first ir-syms) more (rest ir-syms)] ;; find main
          (if (or (and pclass method) (not k))
            (if-not (and pclass method)
              (let [msg "root-task pclass with a zero arg main not found"]
                (log/error msg)
                (throw (AssertionError. msg)))
              [pclass [] method []])
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
(defn transform-htn [ir pargs]
  (reset! *debug-ir* ir) ;;DEBUG
  (doseq [[pclass-name v] (seq ir)]
    (assert pclass-name "Found a nil pclass in the IR")
    (let [{:keys [type methods]} v]
      (dbg-println :info "TRANSFORM-HTN" pclass-name)
      (when (= type :pclass) ;;Could it be anything else?
        (doseq [[mname method] (seq methods)]
          (if mname ;; else (dbg-println :debug "no more methods")
            (let [{:keys [temporal-constraints args primitive display-name body]} method]
              (dbg-println :info "  METHOD" mname "PRIMITIVE" primitive "ARGS" args)
              (if (and (not primitive) body)
                (make-htn-methods ir pargs true pclass-name mname display-name args body))
              )))))))

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

;; returns plant details map with keys
;;   name display-name plantid plant-part interface args argvals
(defn plant-details [ir pargs subtask_ pclass-ctor_ primitive?]
  (let [{:keys [type name arguments ancestry-path]} subtask_
        name-str (str name)
        display-name (display-name-string name-str)
        details {:name name-str
                 :label (str "TEMP-" name-str) ;; TEMPORARY
                 :display-name display-name}]
    (if (or (not primitive?) (= name 'delay))
      details
      (let [{:keys [pclass id plant-part interface]} pclass-ctor_
            plantid id
            _ (dbg-println :debug "PD ARGS" arguments)
            arguments (or arguments [])
            unresolved (count (filter variable? arguments))
            _ (dbg-println :trace "PD UNRESOLVED" unresolved "ARGUMENTS" arguments)
            args (if (pos? unresolved)
                   (resolve-arguments arguments ancestry-path pargs)
                   arguments)
            _ (dbg-println :debug "PD pclass-ctor_" pclass-ctor_)
            _ (dbg-println :debug "PD get-in" pclass name)
            formal-args (mapv str (get-in ir [pclass :methods name :args]))
            _ (dbg-println :debug "PD ARGS" (pr-str args)
                       "FORMAL-ARGS" (pr-str formal-args)) ;;:: DEBUG
            argsmap (zipmap formal-args args)]
        (assoc-if details
                  :args args
                  :argsmap argsmap
                  :plantid plantid
                  :plant-part plant-part
                  :interface interface)))))

;; root? is true to create the "synthetic" htn-network at the very
;; top of the HTN
;; begin is the state node of the parent task
(defn construct-hem-plan-map [ir pargs hem henpt root? parent-begin-uid]
  (let [{:keys [uid label subtasks subtask-constraints edges
                ancestry-path argument-mappings irks]} hem
        ;; DEBUG
        _ (dbg-println :debug "CHPM PARGS" pargs)
        _ (dbg-println :debug "CHPM HEM" (pr-str
                               (dissoc hem :subtasks :ancestry-path
                                       :expansion-method :subtask-constraints)))
        ;; _ (dbg-println :debug "CHPM HENPT" (dissoc henpt :subtasks :ancestry-path
        ;;                           :expansion-method :subtask-constraints))
        hem-irks irks
        [pclass kw-methods method kw-body int-zero more-irks] hem-irks
        hem-pclass pclass
        _ (dbg-println :debug "CHPN 1" pclass method int-zero more-irks kw-methods)
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
            subtask_ (get-htn-object subtask-uid)
            {:keys [type pclass name task-expansions arguments
                    irks ancestry-path]} subtask_
            bounds (if (and irks (not= irks hem-irks))
                     (irks->bounds ir irks))
            m-task-expansions (count task-expansions)
            ;; DEBUG
            _ (dbg-println :debug "ST#" i "of" n-subtasks "IRKS" irks
                       "type" type "pclass" pclass "name" name
                       "m-task-expansions" m-task-expansions)
            ;; resolve pclass if (= type :htn-nonprimitive-task)
            ;; to determine if this was really an unresolved primitive task
            ;; DEBUG plant-class (if (htn-isa? type :htn-nonprimitive-task)
            primitive? (= type :htn-primitive-task)
            parallel? (not sequential?)
            choice? (and (not parallel?) (> m-task-expansions 1))
            ;; DEBUG
            _ (dbg-println :debug "SUBTASK primitive?" primitive?
                       "type" type
                       "parallel?" parallel?
                       "choice?" choice?
                       "\nsubtask_" (with-out-str
                                      (pprint
                                       (dissoc subtask_
                                               :ancestry-path :task-expansions))))
            pclass-ctor_ (if primitive?
                           (resolve-plant-class ir hem-pclass pargs
                                                henpt subtask_))
            ;; DEBUG
            _ (dbg-println :debug "pclass-ctor_" pclass-ctor_)
            ;; add in details for the plant for primitive tasks...
            ;;_ (dbg-println :debug "plant-details" pargs subtask pclass-ctor_ primitive?) ;;DEBUG
            details_ (plant-details ir pargs subtask_ pclass-ctor_ primitive?)
            ;; OLD: label (name-with-argvals subtask)
            subtask-map (merge
                         (assoc
                          (dissoc subtask_ :pclass :pargs :arguments
                                  :task-expansions
                                  :task-type :ancestry-path :temporal-constraints
                                  :irks)
                          :incidence-set (if edge #{(:uid edge)} #{})
                          :edges #{})
                         details_)
            ;; TPN ------------------
            se (tpn/tpn-state {})
            tc (if bounds (tpn/tpn-temporal-constraint
                           {:value bounds :end-node (:uid se)}))
            activity (if primitive?
                       ((if (= name 'delay)
                          tpn/tpn-delay-activity
                          tpn/tpn-activity)
                        {:name (str (:name details_)
                                    (if (:plantid details_) ":")
                                    (:plantid details_)
                                    (if (:plant-part details_) "%")
                                    (:plant-part details_)
                                    (if (:interface details_) "@")
                                    (:interface details_)
                                    "(" (:argsmap details_) ")")
                         :command (:name details_)
                         :display-name (:display-name details_) ;; to be removed
                         :label (:label details_) ;; to be removed
                         :args (:args details_)
                         :argsmap (:argsmap details_)
                         :plantid (:plantid details_)
                         :plant-part (:plant-part details_)
                         :interface (:interface details_)
                         ;; add constraints on the activity
                         :constraints (if tc #{(:uid tc)})
                         :htn-node (:uid hem-map)
                         :end-node (:uid se)}))
            sb (tpn/tpn-state {:activities (if activity #{(:uid activity)})
                               ;; do NOT add constraints on the node
                               ;; :constraints (if tc #{(:uid tc)})
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
            (construct-hem-plan-map ir pargs task-expansion subtask_ false
                                    (:uid sb))))
        ))
    ))

(defn construct-htn-plan-map [ir expanded-root-task]
  ;;(dbg-println :debug expanded-root-task)
  (let [{:keys [uid label pargs]} expanded-root-task
        ;; _ (dbg-println :debug "ERT PARGS" pargs) ;; DEBUG
        net (htn-network {:label label :rootnodes #{uid}})
        {:keys [task-expansions irks]} expanded-root-task
        ert (assoc
             (dissoc expanded-root-task :pclass :pargs :arguments
                     :task-expansions :name
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
        (construct-hem-plan-map ir pargs task-expansion ert true (:uid begin))))
    (tpn/optimize-tpn-map)
    @*htn-plan-map*))

;; rt {:type :root-task
;;         :pclass pclass
;;         :method method
;;         :args args}]
;; (assoc ir 'root-task rt)))

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
                  pcs (vec (remove nil? (mapv
                                         #(let [initial (get-in ir [pclass :fields % :initial])
                                                ctor-arg (if (and (= (:type initial) :pclass-ctor)
                                                                  (= (:pclass initial) mpclass))
                                                           (get (:args initial) arg-position))]
                                            (if ctor-arg
                                              (get-in ir [pclass :fields (keyword ctor-arg) :initial :pclass])))
                                         fields)))
                  pclasses (if (not (empty? pcs))
                             (concatv pclasses pcs)
                             pclasses)
                  pc (first more)
                  more (rest more)]
              (recur pclasses pc more)))))
      :unknown-pclass)))



;; will return subtasks, if any
;; irks are the ks to get-in the ir for the method in question (initially nil)
(defn get-htn-method-subtasks [ir pargs top-level-type mpclass mname display-name margs sub-body irks]
  (dbg-println :debug "\nGET-HTN-METHOD-SUBTASKS"
           "top-level-type" top-level-type "mpclass" mpclass "mname" mname
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
              {:keys [type name field method args
                      primitive body temporal-constraints]} subtask-definition
              ;; DEBUG
              _ (dbg-println :debug "GET-HTN-METHOD-SUBTASKS" i "TYPE" type
                         "NAME" name "FIELD" field "METHOD" method
                         "\nMPCLASS" mpclass "MNAME" mname "NAME" name
                         "PRIMITIVE" primitive
                         "\nMARGS" margs "ARGS" args
                         "BODY" body
                         "TC" temporal-constraints)

              subtask
              (cond
                (#{:plant-fn-symbol :plant-fn-field} type)
                ;; if the value of plant-fn-primitive? is nil we know
                ;; we have the wrong plant-class (because the IR always
                ;; has true or false)
                (let [plant-class (if (= :plant-fn-symbol type)
                                    (if (= name 'this)
                                      mpclass
                                      ;;name might (coincidentally) be a pclass, but probably not
                                      ;;Let's see if there in one (and only one) method amongst all pclasses
                                      (let [pclasses-with-method
                                            (filter #(get-in ir [% :methods method]) (keys ir))]
                                        (if (= (count pclasses-with-method) 1)
                                          (first pclasses-with-method)
                                          ;;If more than one, the method is ambigious!
                                          (do (dbg-println :warn "GHMS Can't resolve name" name "to a pclass")
                                              name))))
                                    ;;else :plant-fn-field
                                    ;;(get-in ir [mpclass :fields field :initial :pclass])
                                    (get-pclass-for-field ir mpclass field)
                                    )
                      plant-fn-primitive?
                      (get-in ir [plant-class :methods method :primitive])
                      _ (dbg-println :debug "plant-class" plant-class "plant-fn-primitive?" plant-fn-primitive?)
                      ;;Special handling for plant-fn-primitive? = nil
                      [plant-class non-primitive?]
                      (if (nil? plant-fn-primitive?)
                        (let [plant-class (:pclass (first (filter #(= (:param %) name) pargs)))
                              primitive? (get-in ir [plant-class :methods name :primitive])]
                          [plant-class (not primitive?)])
                        [plant-class (not plant-fn-primitive?)])
                      ;; The following would help expand non local plant
                      ;; functions, but the HEM logic does not support it.
                      ;; make-htn-method? (or (symbol? mname)
                      ;;                    (nil? plant-fn-primitive?))
                      ;; NOTE: We will make an htn-method, if (not plant-fn-primitive?)
                      ;;make-htn-method? (symbol? mname)
                      ;; DEBUG
                      ;; _ (dbg-println :debug "  make-htn-method?" make-htn-method?
                      ;;     "plant-class" plant-class
                      ;;     "plant-fn-primitive?" plant-fn-primitive?
                      ;;     "non-primitive?" non-primitive?)
                      ;; task-args (if plant-fn-primitive?
                      ;;             margs
                      ;;             (or args margs))
                      _ (assert args "ARGS is nil")
                      task-args (or args margs)
                      _ (dbg-println :debug "TASK-ARGS" task-args
                                     "PLANT-FN-PRIMITIVE?" plant-fn-primitive?)
                      ;;This is the subtask that will be returned (and added to the method)
                      task ((if non-primitive?
                              htn-nonprimitive-task
                              htn-primitive-task)
                            {:pclass plant-class
                             :name method
                             :arguments task-args
                             :temporal-constraints temporal-constraints
                             :irks irks-i})
                      ]
                  task)

                (= :delay type)
                (htn-primitive-task
                 {:name 'delay
                  :display-name "Delay"
                  :temporal-constraints temporal-constraints
                  :irks irks-i})

                ;;Might need to handle :choice here

                :else
                (log/error "Unexpected type:" type))]
          (recur (if subtask (conj subtasks subtask) subtasks)
                 (inc i))
          ))
      )))

;; create htn-methods as a side effect)
(defn make-htn-methods "Construct the HTN Method(s) for mname"
  [ir pargs top-level? mpclass mname display-name margs mbody & [irks]]
  (dbg-println :debug "MHM " "TOP-LEVEL?" top-level? "MPCLASS" mpclass "MNAME" mname
               "DISPLAY-NAME" display-name
               "MARGS" margs
               "IRKS" irks "\nMBODY" mbody)
  (let [irks (conj (or irks [mpclass :methods mname]) :body)
        number-of-subtasks (count mbody)
        first-task-type (:type (get mbody 0))]
    (assert (or (not top-level?)
                (= number-of-subtasks 1)) "There should only be one top-level form")
    (let [irks-0 (conj irks 0)
          nonprimitive-task (htn-nonprimitive-task
                             {:pclass mpclass
                              :name mname
                              :display-name display-name ;;Necessary???
                              :arguments margs
                              ;; No need for :temporal-constraints?
                              :irks irks-0 ;;TODO - verify
                              })

          methods
          (cond ;;returns a vector of methods
            (#{:plant-fn-symbol :plant-fn-field :delay} first-task-type)
            (let [[subtasks subtask-constraints]
                  (get-htn-method-subtasks ir pargs first-task-type
                                           mpclass mname display-name margs mbody irks)
                  method (htn-method {:pclass mpclass
                                      :name mname
                                      :display-name display-name
                                      :nonprimitive-task nonprimitive-task
                                      :subtasks subtasks
                                      :subtask-constraints subtask-constraints
                                      :irks irks-0 ;;TODO - verify
                                      })]
              [method])

            (#{:sequence :parallel} first-task-type)
            (let [[subtasks subtask-constraints]
                 (get-htn-method-subtasks ir pargs first-task-type
                                          mpclass mname display-name margs
                                          (get-in mbody [0 :body])  ;;get the :body of the seq/par
                                          (concatv irks [0 :body]))
                  method (htn-method {:pclass mpclass
                                      :name mname
                                      :display-name display-name
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
                                          mpclass choice-name choice-display-name margs
                                          choice-body
                                          irks-choice)

                        method (htn-method {:pclass mpclass
                                            :name choice-name
                                            :display-name choice-display-name
                                            :nonprimitive-task nonprimitive-task
                                            :subtasks subtasks
                                            :subtask-constraints subtask-constraints
                                            :irks irks-choice})]
                    (recur (conj methods method) (inc i))))))

            :else
            (do
              (log/error "TBD: :else")
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
  (reinitialize-htn-method-table)
  (reinitialize-htn-object-table)
  (reinitialize-htn-plan-map)
  (tpn/reinitialize-tpn-plan-map)
  (let [[pclass pargs method args] (identify-root-task ir root-task)
        _ (transform-htn ir pargs)
        _ (dbg-println :info "PAMA" pclass pargs method args)
        nonprimitive-root-task (htn-nonprimitive-task
                                 {:pclass pclass
                                  :pargs pargs
                                  :name method
                                  :arguments args
                                  :irks [pclass :methods method]})
        expanded-root-task (make-expanded-task nonprimitive-root-task)
        is-stdout? (stdout? output)
        output-prefix (if is-stdout? "STDOUT" output)
        htn-filename (if is-stdout? "-" (str output-prefix ".htn." file-format))
        tpn-filename (if is-stdout? "-" (str output-prefix ".tpn." file-format))
        ;; _ (pprint-htn-methods) ;; DEBUG
        _ (plan-htn-task ir expanded-root-task)
        htn (construct-htn-plan-map ir (get-htn-object expanded-root-task))
        tpn @tpn/*tpn-plan-map*]
    (log/info "Saving HTN to" htn-filename "and TPN to" tpn-filename)
    (output-file htn-filename file-format htn)
    (output-file tpn-filename file-format tpn)
    0)) ;; exit code
