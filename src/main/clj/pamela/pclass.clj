;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;;; Acknowledgement and Disclaimer:
;;; This material is based upon work supported by the Army Contracting
;;; and DARPA under contract No. W911NF-15-C-0005.
;;; Any opinions, findings and conclusions or recommendations expressed
;;; in this material are those of the author(s) and do necessarily reflect the
;;; views of the Army Contracting Command and DARPA.

(ns pamela.pclass
  "The defpclass macro (and ancillary helper functions)
  are defined here to implement the PAMELA lanuage DSL."

  (:refer-clojure :exclude [assert when sequence delay]) ;; try catch

  (:require [clojure.core :as clj]
            [clojure.set :as set]
            [clojure.java.io :refer :all] ;; for as-file
            [clojure.walk :refer [prewalk postwalk]]
            [riddley.walk :refer [walk-exprs]]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [clojure.set :as set]
            [environ.core :refer [env]]
            [pamela.utils :refer [make-url get-url]]
            [avenir.utils :refer [and-fn assoc-if vec-index-of concatv]])
  (:import [clojure.lang
            Symbol PersistentList PersistentArrayMap]))

;; generic functions

(defn boolean?
  "Returns true of x is either true or false"
  {:added "0.e.0"}
  [x]
  (or (true? x) (false? x)))

(defn list-or-cons?
  "Returns true of x is a list"
  {:added "0.2.0"}
  [x]
  (or (instance? clojure.lang.PersistentList x)
    (instance? clojure.lang.Cons x)))

;; current values of lvar's -------------------------------

(def #^{:dynamic true :added "0.2.0" :doc/format :markdown}
  *world*
  "The state of the world (i.e. lvar values)

  NOTE: as this is a dynamic var it can be used with **binding**"
  (atom nil))

(def #^{:dynamic true :added "0.2.0" :doc/format :markdown}
  *url*
  "The current provenance (e.g. file) for reading PAMELA code.

  NOTE: as this is a dynamic var it can be used with **binding**"
  nil)

(def #^{:dynamic true :added "0.2.0" :doc/format :markdown}
  *pclasses*
  "A vector of pclass symbols which have been defined using defpclass.

  NOTE: as this is a dynamic var it can be used with **binding**"
  (atom []))

(def #^{:added "0.3.0"}
  valid-defpclass-opts
  "Valid options for defpclass"
  #{:meta :inherit :fields :modes :methods :transitions})

(def #^{:added "0.3.0"}
  valid-meta-keys
  "Valid keys for the defpclass :meta map"
  #{:version :icon :depends :doc})

(def #^{:added "0.3.0"}
  valid-pclass-ctor-opts
  "Valid pclass constructor options"
  #{:id :interface})

(def #^{:added "0.3.0"}
  default-bounds
  "Default bounds"
  [0 :infinity])

(def #^{:added "0.3.0"}
  pclass-types
  "Set of valid pclass types"
  #{:pclass :pclass-enumeration})

(def #^{:added "0.3.0"}
  pamela-reserved-words
  "Reserved words in the PAMELA language"
  #{"access"
    "and"
    "ask"
    "assert"
    "between"
    "between-ends"
    "between-starts"
    "bounds"
    "catch"
    "choice"
    "choose"
    "controllable"
    "cost"
    "defpclass"
    "defpmethod"
    "delay"
    "depends"
    "doc"
    "false"
    "fields"
    "guard"
    "icon"
    "id"
    "implies"
    "infinity"
    "inherit"
    "initial"
    "interface"
    "label"
    "lvar"
    "maintain"
    "meta"
    "methods"
    "modes"
    "mode-of"
    "not"
    "observable"
    "optional"
    "or"
    "parallel"
    "pclass"
    "post"
    "pre"
    "probability"
    "reward"
    "sequence"
    "slack-parallel"
    "slack-sequence"
    "soft-parallel"
    "soft-sequence"
    "tell"
    "transitions"
    "true"
    "try-form"
    "unless"
    "version"
    "when"
    "whenever"})

(defn get-model-vars
  "Return models has a map from symbol -> var."
  {:pamela :models-helper :added "0.2.0"}
  ([]
   (get-model-vars true))
  ([models?]
   (let [ptype= (if models? not= =)
         models-ns (the-ns 'pamela.models)
         only-models (fn [e]
                       (let [me (meta (.getValue e))
                             nse (get me :ns)
                             pamela (get me :pamela)]
                         (and (= nse models-ns) (ptype= pamela :models-helper))))
         models (filter only-models (ns-publics models-ns))]
     (reduce (fn [m [k v]] (assoc m k v)) {} models))))

(defn list-models
  "Return a vector of model symbols.
   If only-persistent? then return only DB models (not those simply loaded)."
  {:pamela :models-helper :added "0.2.0"}
  ([]
   (list-models true))
  ([only-persistent?]
   (let [select (if only-persistent?
                  #(:id (meta %)) ;; must have :id
                  #(identity %))
         vars (filter select (vals (get-model-vars)))
         names (map (comp :name meta) vars)]
     (vec (sort names)))))

(defn delete-model
  "Removes a model (from the pamela.models namespace)."
  {:pamela :models-helper :added "0.2.0"}
  [model]
  (let [model (if (symbol? model) model (symbol model))]
    (ns-unmap (the-ns 'pamela.models) model)))

(defn delete-all-models
  "Removes all models (from the pamela.models namespace)."
  {:pamela :models-helper :added "0.2.0"}
  [& [all?]]
  (doseq [model (list-models (not all?))]
    (delete-model model)))

(defn get-model-var
  "Return a model var by symbol (or name)

   If the second argument is true simply return nil
   if the model-var does not exist."
  {:pamela :models-helper :added "0.2.0"}
  [model-name & [exists?]]
  (if (nil? model-name)
    (throw (AssertionError. "cannot get-model-var for nil\n"))
    (let [model-name (if (symbol? model-name) model-name (symbol model-name))
          ;; the following doesn't work with late binding
          ;; model-var (resolve model-name)
          ;; instead we'll query the namespace here...
          models (get-model-vars)
          model-var (get models model-name)
          _ (if (and (not exists?) (nil? model-var))
              (throw (AssertionError.
                       (str "new-class: model-name does not exist: "
                         model-name "\n"))))]
      model-var)))

(defn get-model
  "Return a model by symbol (or name)

   If the second argument is true simply return nil
   if the model does not exist."
  {:pamela :models-helper :added "0.2.0"}
  [model-name & [exists?]]
  (let [model-var (get-model-var model-name exists?)]
    (if model-var
      (deref model-var))))

;; everything should be able to explain itself
(defprotocol ^{:added "0.2.0"} IExplain
  "Signature for method for objects to explain themselves"
  (explain [this]))

;; Logical Variable
(defrecord LVar [^String lvar]
  IExplain
  (explain [this]
    (str "Logic var named: " lvar)))

(defn lvar
  "Create an LVar (logic variable). All logic variables have a name
   (or will have one gensym'd if not provided). Each logic variable
   refers to name in the **world** which hold it's value.
   LVar's may be bound to other LVar's."
  {:tag LVar :pamela :lvar :added "0.2.0" :doc/format :markdown}
  ([] (lvar `lvar#))
  ([lvar-name] (lvar lvar-name nil))
  ([lvar-name meta] (lvar lvar-name meta nil))
  ([lvar-name meta extra] (LVar.
                            (name lvar-name)
                            (merge meta {:pamela :lvar})
                            extra)))

(defn lvar?
  "Returns true if v is an LVar"
  {:tag Boolean :added "0.2.0"}
  [v]
  (instance? pamela.pclass.LVar v))

(defn pclass?
  "Returns true if pclass is an pclass."
  {:pamela :models-helper :added "0.2.0"}
  [pclass]
  (cond
    (or (nil? pclass) (lvar? pclass))
    false
    (map? pclass) ;; this is an instance
    (let [p (:pclass pclass)
          pvar (get-model-var p)
          model-meta (meta pvar)]
      (boolean (pclass-types (:pamela model-meta))))
    (fn? pclass) ;; a pclass function
    (let [p (:name (meta pclass))
          pvar (get-model-var p)
          model-meta (meta pvar)]
      (boolean (pclass-types (:pamela model-meta))))
    (or (symbol? pclass) (string? pclass))
    (let [p (symbol pclass)
          pvar (get-model-var p)
          model-meta (meta pvar)]
      (boolean (pclass-types (:pamela model-meta))))
    :else
    false))

(defn pclass-instance?
  "Returns true if pclass is an instance pclass."
  {:pamela :models-helper :added "0.2.0"}
  [pclass]
  (cond
    (or (nil? pclass) (lvar? pclass))
    false
    (and (map? pclass) (:pclass pclass)) ;; this is an instance
    (let [p (:pclass pclass)
          pvar (get-model-var p)
          model-meta (meta pvar)
          ;;_ (print "PVAR nil?" (nil? pvar) "MM>" model-meta "<MM")
          ]
      (boolean (pclass-types (:pamela model-meta))))
    :else
    false))

(defn enumeration-pclass?
  "Returns true if pclass is an enumeration class."
  {:pamela :models-helper :added "0.2.0"}
  [pclass]
  (cond
    (or (nil? pclass) (lvar? pclass))
    false
    (map? pclass) ;; this is an instance
    (let [p (:pclass pclass)
          pvar (get-model-var p)
          model-meta (meta pvar)]
      (= (:pamela model-meta) :pclass-enumeration))
    (fn? pclass) ;; a pclass function
    (let [p (:name (meta pclass))
          pvar (get-model-var p)
          model-meta (meta pvar)]
      (= (:pamela model-meta) :pclass-enumeration))
    (or (symbol? pclass) (string? pclass))
    (let [p (symbol pclass)
          pvar (get-model-var p)
          model-meta (meta pvar)]
      (= (:pamela model-meta) :pclass-enumeration))
    :else
    false))

(defn lvar-or-pclass?
  "Return true of x is a lvar or pclass"
  {:added "0.2.0"}
  [x]
  (or (lvar? x) (pclass? x)))

;; set an lvar value
(defn set-mode!
  "Set the value for an LVar in the **world**."
  {:added "0.2.0"}
  [v value]
  (let [lvar-name (if (lvar? v) (:lvar v) v)
        lvar-value (get-in @*world* [:lvars lvar-name])]
    (if (lvar? lvar-value)
      (set-mode! lvar-value value)
      (if (string? lvar-name)
        (swap! *world* assoc-in [:lvars lvar-name] value)))))

;; get an lvar value or pclass mode
(defn mode-of
  "Get the current value for a LVar or pclass *(assigned :mode)*."
  {:added "0.2.0" :doc/format :markdown
   :pamela :pclass-builder}
  [x]
  (let [value
        (if (or (lvar? x) (string? x))
          (let [lvar-name (if (lvar? x) (:lvar x) x)]
            (get-in @*world* [:lvars lvar-name]))
          (if (pclass-instance? x)
            [(:pclass x) (:mode x)]))]
    (if (lvar? value)
      (mode-of value)
      value)))

;; returns true of pclass is in mode
(defn in-mode?
  "Return true if pclass is determined to be in mode
*(mode-of function evaluated)*."
  {:tag Boolean :added "0.2.0" :doc/format :markdown}
  [pclass mode]
  (let [f (get-in pclass [:modes mode])]
    (cond
      (true? f) (= (:mode pclass) mode) ;; enum must be explicitly set
      (fn? f) (f pclass)
      :else false)))

(defn determine-mode
  "Determine current mode for pclass.

   There may be many possible modes. If there is only one nontrivial
   (function based) mode then select that one, else take the first
   possible mode."
  {:added "0.2.0"}
  [pclass]
  (let [possible
        (remove nil?
          (for [mode (keys (:modes pclass))]
            (if (in-mode? pclass mode) mode)))
        p1 (first possible)]
    (if (= (count possible) 1)
      p1
      (let [nontrivial
            (remove nil?
              (for [mode possible]
                (let [f (get-in pclass [:modes mode])]
                  (if (true? f) nil mode))))
            n1 (first nontrivial)]
        (if (= (count nontrivial) 1) ;; what if > 1??
          n1
          p1)))))

(defn validate-meta-depend
  "A defpclass helper: validate meta data dependency."
  {:added "0.2.0"}
  [pclass-name depend]
  (if-not (vector? depend)
    (throw
      (AssertionError.
        (str "defpclass " pclass-name
          " meta :depends component must be a vector (not \"" depend "\")")))
    (if-not (= 2 (count depend))
      (throw
        (AssertionError.
          (str "defpclass " pclass-name
            " meta :depends component must be a vector of length 2")))
      (let [pclass (first depend)
            version (second depend)]
        (when-not (symbol? pclass)
          (throw
            (AssertionError.
              (str "defpclass " pclass-name
                " meta :depends entry must start with a symbol (not \""
                pclass "\")"))))
        (when-not (string? version)
          (throw
            (AssertionError.
              (str "defpclass " pclass-name
                " meta :depends entry must end with a string (not \""
                version "\")"))))
        (let [p (get-model-var pclass true)
              pclass-version (if p (:version (meta p)))]
          (clj/when (nil? p)
            (throw
              (AssertionError.
                (str "defpclass " pclass-name
                  " meta :depends upon a non-existent model: " pclass))))
          (when-not (= version pclass-version)
            (throw
              (AssertionError.
                (str "defpclass " pclass-name
                  " meta :depends upon " depend
                  " but the available version is: \"" pclass-version "\""))))
          [(list 'quote pclass) version])))))

(defn validate-meta-kv
  "A defpclass helper: validate meta data key and value."
  {:added "0.2.0"}
  [pclass-name]
  (fn [m k v]
    (when-not (contains? valid-meta-keys k)
      (throw
        (AssertionError.
          (str "defpclass " pclass-name
            " meta key \"" k "\" invalid, must be one of: " valid-meta-keys))))
    (case k
      :version
      (do
        (when-not (string? v)
          (throw
            (AssertionError.
              (str "defpclass " pclass-name
                " meta :version must be a string (not \"" v "\")"))))
        (assoc m k v))
      :doc
      (do
        (when-not (string? v)
          (throw
            (AssertionError.
              (str "defpclass " pclass-name
                " meta :doc must be a string (not \"" v "\")"))))
        (assoc m k v))
      :depends
      (do
        (when-not (vector? v)
          (throw
            (AssertionError.
              (str "defpclass " pclass-name
                " meta :depends must be a vector (not \"" v "\")"))))
        (assoc m k (mapv (partial validate-meta-depend pclass-name) v)))
      :icon
      (do
        (when-not (string? v)
          (throw
            (AssertionError.
              (str "defpclass " pclass-name
                " meta :icon must be a pathname string (not \"" v "\")"))))
        (let [abs-path (if (.startsWith v "/")
                         v ;; absolute
                         (str (:user-dir env) "/" v)) ;; relative
              path-file (as-file abs-path)
              icon (if (.exists path-file)
                     (str (as-url path-file))
                     (if-let [url *url*]
                       (let [url (as-url url)
                             protocol (.getProtocol url)
                             host  (.getHost url)
                             port  (.getHost url)
                             path (.getPath url)
                             sibling (as-file
                                       (str (.getParent (as-file path)) "/" v))]
                         (if (and (= protocol "file") (.exists sibling))
                           (str (as-url sibling))
                           (let [icon-url (make-url
                                            protocol host port
                                            (.getPath sibling))
                                 icon-data (get-url icon-url)]
                             (if icon-data
                               (str icon-url)))))))]
          (assoc m :icon (or icon "file:///tmp/unknown.icon")))))))

(defn validate-meta
  "A defpclass helper: validate meta data."
  {:added "0.2.0"}
  [pclass-name meta]
  (if (nil? meta)
    {}
    (do
      (when-not (map? meta)
        (throw (AssertionError. "defpclass :meta must be a map")))
      (reduce-kv (validate-meta-kv pclass-name) {} meta))))

(defn validate-name
  "A defpclass/defpmethod helper: validate name is not a reserved word."
  {:added "0.2.0"}
  [name]
  (if (pamela-reserved-words (str name))
    (throw
      (AssertionError.
        (str "cannot define a pclass or pmethod with the reserved word: "
          name)))))

(defn validate-args
  "A defpclass/defpmethod helper: validate args is a vector of symbols."
  {:added "0.2.0"}
  [call args]
  (when-not (vector? args)
    (throw (AssertionError. (str call " expects a vector of args."))))
  (clj/when (not (or (empty? args) (reduce and-fn (map symbol? args))))
    (throw (AssertionError. (str "All " call " args must be symbols")))))


(defn get-pclass-meta
  "Returns the meta data for a symbol in 'pamela.models or 'pamela.pclass
if found, else nil"
  {:added "0.2.0"}
  [pclass]
  (let [pclass-sym (symbol (str pclass))
        pclass-var (or (get-model-var pclass-sym true)
                     (ns-resolve 'pamela.pclass pclass-sym))]
    (if pclass-var
      (meta pclass-var))))

(defn valid-mode-of?
  "Validate initial mode in mode-of expression"
  {:added "0.2.0"}
  [pclass initial]
  (let [modes (:modes (get-pclass-meta pclass))]
    (if modes
      (modes initial))))

(defn get-field
  "Return the initial value of a field in a pclass instance"
  {:added "0.2.0"}
  [pclass field]
  (if (pclass-instance? pclass)
    (get-in pclass [:fields field :initial])))

(defn get-field-modes
  "Return a list of (mode-of pclass :mode-of-pclass) for
this field initializer value"
  {:added "0.2.0"}
  [val]
  (if (and (map? val) (:initial val))
    (get-field-modes (:initial val))
    (if-not (symbol? val) ;; ignore symbols
      (if (pclass-instance? val)
        (let [pclass (:pclass val)
              modes (keys (:modes val))]
          (doall (map #(list 'mode-of pclass %) modes)))
        (if (list-or-cons? val)
          (let [v0 (first val)
                v1 (second val)
                pclass (if (= 'mode-of v0) v1 v0)
                modes (:modes (get-pclass-meta pclass))]
            (map #(list 'mode-of pclass %) modes))
          ;; (println "UNKNOWN field modes for" val)
          )))))

;; returns [field-names field-modes]
;; where field-names is a set of the field name keywords
;; and field-modes is set of (mode-of pclass :mode-for-pclass)
(defn validate-fields
  "A defpclass helper: validate fields are one of: args, lvars, pclass, pclass-builder."
  {:added "0.2.0"}
  [args fields]
  (let [fields-seq (seq fields)]
    (loop [field-names #{} field-modes '()
           field-val (first fields-seq) more (rest fields-seq)]
      (if-not field-val
        [field-names (set field-modes)]
        (let [[field val] field-val
              field-ms (remove nil? (get-field-modes val))]
        (if-not (keyword? field)
          (throw (AssertionError.
                   (str "field name " field " must be a keyword"))))
        (condp #(= %1 %2) (type val)
          Symbol ;; must be in the args to defpclass
          (if (not-any? #(= val %) args)
            (throw (AssertionError.
                     (str "Symbol " val " not in args " args))))
          PersistentList  ;; must be a lvar or pclass ctor
          (let [f (first val)
                pamela? (:pamela (get-pclass-meta f))
                [pclass initial] (if (and pamela? (= f 'mode-of)) (rest val))]
            (if (and pclass initial (not (valid-mode-of? pclass initial)))
              (throw (AssertionError.
                       (str "mode-of " pclass
                         " specfies illegal initial mode: " initial))))
            (if-not pamela?
              (throw (AssertionError.
                       (str "Function " f
                         " does not return an lvar or pclass")))))
          PersistentArrayMap
          (doseq [[fik fiv] (seq val)]
            (case fik
              :initial
              (condp #(= %1 %2) (type fiv)
                Symbol ;; must be in the args to defpclass
                (if (not-any? #(= fiv %) args)
                  (throw (AssertionError.
                           (str "Symbol " fiv " not in args " args))))
                PersistentList  ;; must be a lvar or pclass ctor
                (let [f (first fiv)
                      pamela? (:pamela (get-pclass-meta f))
                      [pclass initial]
                      (if (and pamela? (= f 'mode-of)) (rest fiv))]
                  (if (and pclass initial (not (valid-mode-of? pclass initial)))
                    (throw (AssertionError.
                             (str "mode-of " pclass
                               " specfies illegal initial mode: " initial))))
                  (if-not pamela?
                    (throw (AssertionError.
                             (str "Function " f
                               " does not return an lvar or pclass")))))
                (throw (AssertionError.
                         (str "Field initializer :initial" fiv
                           " is not an arg nor returns an lvar or pclass"))))
              :access
              (if-not (#{:public :private} fiv)
                (throw
                  (AssertionError.
                    (str "Field initializer :access must be :public or :private, not: " fiv))))
              :observable
              (if-not (or (true? fiv) (false? fiv))
                (throw
                  (AssertionError.
                    (str "Field initializer :observable must boolean, not: "
                      fiv))))
              ;; :else
              (throw (AssertionError.
                       (str "Field initializer key is not valid: " fik)))))
          (throw
            (AssertionError.
              (str "Field initializer " val
                " is not an arg nor returns an lvar or pclass nor is a field initialzer map"))))
        (recur (conj field-names field) (concat field-modes field-ms)
          (first more) (rest more)))))))

(defn parse-cond-operand
  "Disambiguates an operand to a cond-expr using the field-names and field-modes.
Returns nil if undetermined."
  {:added "0.3.0"}
  [args field-names field-modes operand]
  (let [rv
        (if (keyword? operand)
          (if (field-names operand)
            (list operand 'this)
            (let [matches (filter #(= operand (nth % 2)) field-modes)]
              (if (= 1 (count matches))
                (first matches)
                ;; NOTE lvar problem, must use explicit mode-of
                )))
          (if (list-or-cons? operand)
            (let [[op pclass mode] operand
                  modes (if (and pclass mode)
                          (:modes (get-pclass-meta pclass)))]
              (if (and (keyword? op) (symbol? pclass) (nil? mode))
                (if (or (= pclass 'this) (some #(= pclass %) args))
                  (list op pclass))
                (if (and (= op 'mode-of) (modes mode))
                  (list 'mode-of pclass mode))))))]
    ;; (println "  PCO=>" rv)
    rv))

(defn parse-cond-expr
  "Parses and disambiguates a cond-expr using the field-names and field-modes.
Returns nil if undetermined."
  {:added "0.3.0"}
  [args field-names field-modes condition]
  (let [rv
        (if (keyword? condition) ;; FIXME condition must be a valid mode
          (list 'mode-of 'this condition)
          (if (and (list-or-cons? condition) (symbol? (first condition)))
            (let [op (first condition)]
              (if (#{'and 'or 'implies 'not} op)
                (let [operands (doall (map (partial parse-cond-expr args
                                             field-names field-modes)
                                        (rest condition)))]
                  (if-not (or (some nil? operands)
                            (and (= op 'not) (> 1 (count operands))))
                    (cons op operands)))
                (if (= op '=)
                  (let [operands (doall (mapv (partial parse-cond-operand args
                                                field-names field-modes)
                                          (rest condition)))]
                    (if (some nil? operands)
                      (throw
                        (AssertionError.
                          (str "operand for conditional expression invalid: "
                            (nth condition (inc (vec-index-of operands nil))))))
                      (cons op operands))))))))]
    ;; (println "  PCE=>" rv)
    rv))

(defn validate-options
  "A helper class to validate options given to defpclass or a pclass constructor"
  {:added "0.3.0"}
  [call-site valid-options options]
  (if (and options (even? (count options)))
    (doseq [opt (take-nth 2 options)]
      (if-not (keyword? opt)
        (throw (AssertionError.
                 (str call-site " option is not a keyword: " opt))))
      (if-not (valid-options opt)
        (throw (AssertionError.
                 (str call-site " option is not valid: " opt
                   ", must be one of: " valid-options)))))))

(defn validate-superclasses
  "Future support for inheritence. Current throws an exception"
  {:added "0.3.0"}
  [inherit]
  (if inherit
    (throw (AssertionError.
             (str "superclasses not yet supported: " inherit)))))

(defn validate-modes
  "A defpclass helper: validate modes"
  {:added "0.2.0"}
  [args field-names field-modes modesmap]
  (doseq [[mode condition] modesmap]
    (let [condition (or (true? condition)
                      (parse-cond-expr args field-names field-modes
                        condition))]
      (if-not condition
        (throw (AssertionError.
                 (str "condition for mode " mode " invalid: " condition)))))))

(defn validate-transitions
  "A defpclass helper: validate transitions"
  {:added "0.2.0"}
  [args field-names field-modes valid-modes transitions]
  (doseq [transition transitions]
    (let [[fromto trans-map] transition
          [from to] (map keyword (string/split (name fromto) #":"))
          {:keys [pre post probability]} trans-map
          pre (or (nil? pre)
                (and (keyword? pre) (valid-modes pre)
                  (list 'mode-of 'this pre))
                (parse-cond-expr args field-names field-modes pre))
          post (or (nil? post)
                 (and (keyword? post) (valid-modes post)
                   (list 'mode-of 'this post))
                 (parse-cond-expr args field-names field-modes post))]
      (if (and (not= from :*) (not (valid-modes from)))
        (throw
          (AssertionError.
            (str "Invalid transition :FROM:TO \"" fromto
              "\" where the :FROM is not one of: " valid-modes))))
      (if (not (valid-modes to))
        (throw
          (AssertionError.
            (str "Invalid transition :FROM:TO \"" fromto
              "\" where the :TO is not one of: " valid-modes))))
      (if-not pre
        (throw
          (AssertionError.
            (str "pre-condition for transition " fromto " invalid: " pre))))
      (if-not post
        (throw
          (AssertionError.
            (str "post-condition for transition " fromto " invalid: " post))))
      (if (and probability
            (not (or (and (symbol? probability) (some #(= probability %) args))
                   (and (list-or-cons? probability)
                     (= 'lvar (first probability)))
                   (number? probability))))
        (throw
          (AssertionError.
            (str "probabiliity for transition " fromto
              " invalid: " probability)))))))

(defn validate-arg-types
  "A defpclass helper: validate (at instanciation time) args are lvars."
  {:added "0.2.0"}
  [pclass-name args options]
  (if-not (empty? args)
    (loop [arg (first args) more (rest args)]
      (if-not arg
        true ;; valid
        (if (or
              (and (list-or-cons? arg) (= 'lvar (first arg)))
              (lvar-or-pclass? arg))
          (recur (first more) (rest more))
          (throw (AssertionError.
                   (str "All pclass args must be LVars or pclasses in: ("
                     pclass-name
                     " " (apply str (interpose " "args))
                     " " (apply str (interpose " " options)) ")"))))))))

(defn build-fields-pclass
  "A defpclass helper: resolve arguments to a pclass constructor and invoke it."
  {:added "0.3.0"}
  [field-map pclass args]
  (loop [new-args [] symbols? true arg (first args) more (rest args)]
    (if-not arg
      (do
        (if (= pclass 'mode-of)
          (first new-args)
          (apply (ns-resolve 'pamela.models pclass) new-args)))
      (if (and symbols? (symbol? arg))
        (if (= pclass 'mode-of) ;; arg is pclass to construct
          (let [initial (first more)
                val (assoc ((ns-resolve 'pamela.models arg))
                      :mode initial)] ;; construct in initial mode
            (recur (conj new-args val) false nil nil))
          (let [field (keyword (name arg)) ;; lookup arg symbol
                val (get-in field-map [field :initial])]
            (if-not val
              (throw
                (AssertionError.
                  (str "pclass constructor arg " arg
                    " used before it was defined.")))
              (recur (conj new-args val) true (first more) (rest more)))))
          (recur (conj new-args arg) false (first more) (rest more))))))

;; argvals should be a vector
(defn build-fields
  "A defpclass helper: construct each field."
  {:added "0.2.0"}
  [args argvals fields]
  (if (pos? (count fields))
    (loop [field-map {} field-val (first fields) more (rest fields)]
      (if-not field-val
        field-map
        (let [[field val] field-val
              access :private
              observable false
              [initial access observable]
              (cond
                (symbol? val) ;; must be an arg
                [(get argvals (vec-index-of args val)) access observable]
                (or (lvar? val) (pclass-instance? val))
                [val access observable]
                (and (fn? val) (:pamela (meta val))) ;; :pamela builder fn?
                [(val nil field) access observable] ;; call builder WITHOUT p!!!
                (list-or-cons? val)
                (let [p (first val)
                      pamela? (:pamela (get-pclass-meta p))]
                  (when-not pamela?
                    (throw (AssertionError.
                             (str "Function " p " is not a defined pclass"))))
                  [(build-fields-pclass field-map p (rest val))
                   access observable])
                (map? val)
                (let [initial (:initial val)
                      access (or (:access val) access)
                      observable (or (:observable val) observable)
                      p (first initial)
                      pamela? (:pamela (get-pclass-meta p))]
                  (when-not pamela?
                    (throw (AssertionError.
                             (str "Function " p " is not a defined pclass"))))
                  [(build-fields-pclass field-map p (rest initial))
                   access observable])
                :else
                (throw (AssertionError. (str "Field value " val " illegal"))))
              new-val  {:initial initial
                        :access access
                        :observable observable}
              field-map (assoc field-map field new-val)]
          (recur field-map (first more) (rest more)))))))

(defn build-mode
  "A defpclass helper: build the mode."
  {:added "0.2.0"}
  [p mode value]
  ^{:added "0.2.0"}
  (assoc-in p [:modes mode] value))

;; early idea: applicative
;;   each mode should be a function of this pclass instance
;;   which returns true if it's in the mode
;; current idea: make explicit symbols that can be output
(defn build-modes
  "A defpclass helper: disambiguate mode conditions."
  {:added "0.2.0"}
  [args argvals field-names field-modes modesmap]
  (let [mode-conditions (seq modesmap)]
    (loop [modesmap {} mode-condition (first mode-conditions)
           more (rest mode-conditions)]
      (if-not mode-condition
        modesmap
        (let [[mode condition] mode-condition
              condition (or (true? condition)
                          (parse-cond-expr args field-names field-modes
                            condition))
              modesmap (assoc modesmap mode condition)]
          (recur modesmap (first more) (rest more)))))))

(defn build-transitions
  "A defpclass helper: disambiguate pre and post conditions."
  {:added "0.2.0"}
  [args argvals field-names field-modes valid-modes transitions]
  (let [fromto-trans-maps (seq transitions)]
    (loop [transmap {} fromto-trans-map (first fromto-trans-maps)
           more (rest fromto-trans-maps)]
      (if-not fromto-trans-map
        transmap
        (let [[fromto trans-map] fromto-trans-map
              {:keys [pre post probability]} trans-map
              pre (or (nil? pre)
                    (and (keyword? pre) (valid-modes pre)
                      (list 'mode-of 'this pre))
                    (parse-cond-expr args field-names field-modes pre))
              post (or (nil? post)
                     (and (keyword? post) (valid-modes post)
                       (list 'mode-of 'this post))
                     (parse-cond-expr args field-names field-modes post))
              probability (if probability
                            (cond
                              (number? probability)
                              probability
                              (symbol? probability)
                              (get argvals (vec-index-of args probability))
                              :else
                              (lvar (second probability))))
              trans-map (assoc-if trans-map
                          :pre (if-not (true? pre) pre)
                          :post (if-not (true? post) post)
                          :probability probability)
              transmap (assoc transmap fromto trans-map)]
          (recur transmap (first more) (rest more)))))))

(defn validate-label-fn
  "Validate label"
  {:added "0.2.2"}
  [pclass-name m labels]
  (fn [form]
    (let [method (first form)
          opts (second form)
          label (:label opts)]
      (clj/when label
        (swap! labels update-in [label]
          ;; NOTE count could be nil if the label was not used in a between
          (fn [count]
            (if (or (nil? count) (zero? count))
              1
              (throw (AssertionError.
                       (str "label " label " defined multiple times "
                         " in pclass: " pclass-name " method: " m "\n")))))))
      form)))

(defn validate-labels
  "Validate labels"
  {:added "0.2.2"}
  [pclass-name methods]
  (doseq [method methods]
    (let [mf (eval method)
          [m f] (first (seq mf))
          {:keys [args pre post bounds doc betweens body]} f]
      (when-not (empty? betweens)
        (let [labels (atom {})
              all-labels (set (doall
                                (apply concat
                                  (map
                                    #(vector (second %) (nth % 2)) betweens))))
              validate-label (validate-label-fn pclass-name m labels)]
          (doseq [label all-labels]
            (swap! labels assoc label 0))
          (prewalk #(if (list-or-cons? %) (validate-label %) %) body)
          (let [labels-undefined
                (if (pos? (count @labels))
                  (map first (filter #(zero? (second %)) (seq @labels))))]
            (if-not (empty? labels-undefined)
              (throw (AssertionError.
                       (str "pclass: " pclass-name " method: " m
                         " has the following undefined labels: "
                         (string/join " " labels-undefined)
                         "\n"))))))))))

(defn build-method
  "A defpclass helper: build the method."
  {:pamela :pclass-builder-fn :added "0.2.0"}
  [field-names field-modes valid-modes pclass method]
  (let [[m f] (first (seq method))
        {:keys [args pre post bounds doc betweens body]} f
        pclass-args (:args pclass)
        both-args (if args (concatv pclass-args args) pclass-args)
        pre (or (nil? pre)
                (and (keyword? pre) (valid-modes pre)
                  (list 'mode-of 'this pre))
                (parse-cond-expr both-args field-names field-modes pre))
          post (or (nil? post)
                 (and (keyword? post) (valid-modes post)
                   (list 'mode-of 'this post))
                 (parse-cond-expr both-args field-names field-modes post))]
      (if-not pre
        (throw (AssertionError.
                 (str "for defpmethod " m " invalid pre-condition:" pre))))
      (if-not post
        (throw (AssertionError.
                 (str "for defpmethod " m " invalid post-condition: " post))))
      [m (assoc-if f
           :pre (if-not (true? pre) pre)
           :post (if-not (true? post) post))]))

(defn fuse-methods
  "A build-methods helper: fuse all methods by name"
  {:pamela :pclass-builder-fn :added "0.2.0"}
  [methods method]
  (let [[method-name method-body] method
        bodies (get methods method-name)
        bodies (if-not bodies
                 method-body ;; first body
                 (if-not (vector? bodies) ;; second body
                   (vector bodies method-body)
                   (conj bodies method-body)))]
    (assoc methods method-name bodies)))

(defn build-methods
  "A defpclass helper: build the methods"
  {:pamela :pclass-builder-fn :added "0.2.0"}
  [pclass field-names field-modes valid-modes]
  (let [methods (:methods pclass)]
    (if (pos? (count methods))
      (let [new-methods
            (reduce fuse-methods {}
              (map (partial build-method field-names
                     field-modes valid-modes pclass) methods))]
        (assoc pclass :methods new-methods))
      pclass)))

(defn- add-to-pclasses [pclass]
  (if-not (vector? @*pclasses*)
    (throw (AssertionError. "*pclasses* must be an atom with vector")))
  (swap! *pclasses* conj pclass))

(defmacro defpclass
  "Define a pclass. The pclass will be defined as a function
   in the models namespace. To create (instanciate) the pclass
   that function must, later, be called with any required LVar
   arguments.

  **(defpclass name [args*] meta? fields? modes? methods? transitions?)**

  *where*

  **args** are a vector of optional LVar's to be passed in when instanciating the pclass.

  *... and the options take the form of a keyword value pairs...*

  **:meta** {:version \"0.2.0\" :icon \"myclass.svg\"}

  **:fields** {:fieldname arg-lvar-or-pclass ...}

  **:modes** {:mode0 mode-fn} *or* [:mode0 :mode1 ...]

  where mode-fn is either a constant **true** *or* a fn taking the pclass
  and returning a truthy value if in that mode or a vector where each
  mode-fn is assigned **true**.

  **:methods** [(defpmethod method0 ...) (defpmethod method1 ...) ...]

  *see defpmethod for detailed signature*

  **:transitions** {:from-mode:to-mode
                 {:pre pre-condition
                  :post post-condition
                  :probability probability
                  :doc docstring}
                ...}

  *where*

  * **:from-mode** may be :* meaning from any mode (otherwise :from-mode and :to-mode must be defined in modes).
  * **pre-condition** is the mode prior to this transition
  * **post-condition** is the mode after this transition
  * **probability** is the real number [0.0 .. 1.0] for spontaneous transitions
  * **docstring** is the documentation for the transition
  "
  {:added "0.2.0" :doc/format :markdown}
  [name args & opts]
  (validate-name name)
  (validate-args "defpclass" args)
  (validate-options (str "defpclass " name) valid-defpclass-opts opts)
  (let [{:keys [meta inherit fields modes methods transitions]} opts
        {:keys [version icon depends doc]} meta
        url *url* ;; bound in models.clj
        pclass-type (if (or (empty? modes) (map? modes))
                      :pclass :pclass-enumeration)
        argstrs (mapv str args)
        [field-names field-modes] (validate-fields args fields)
        modesmap (if-not (nil? modes)
                   (if (map? modes) modes
                       (zipmap modes (repeat true))))
        valid-modes (if modesmap (set (keys modesmap)))]
    (validate-superclasses inherit)
    (validate-modes args field-names field-modes modesmap)
    (validate-transitions args field-names field-modes valid-modes transitions)
    (validate-labels name methods)
    (add-to-pclasses name)
    `(def ~(with-meta name
             (merge (assoc-if {:args (list 'mapv 'symbol argstrs)
                               :pamela pclass-type
                               :url url}
                      :modes valid-modes) ;; valid modes
               (validate-meta name meta)))
       ^{:pamela ~pclass-type :name '~name}
       (fn [~@args & ~(symbol 'options)]
         (validate-arg-types '~name ~args ~(symbol 'options))
         (let [valid?# (validate-options (str '~name " pclass constructor")
                         valid-pclass-ctor-opts ~(symbol 'options))
               {:keys [~(symbol 'id) ~(symbol 'interface)]} ~(symbol 'options)
               fields# (build-fields
                         (quote ~args)
                         (apply vector ~args)
                         (quote ~fields))
               modes# (build-modes
                        (quote ~args)
                        (apply vector ~args)
                        (quote ~field-names)
                        (quote ~field-modes)
                        (quote ~modesmap))
               transitions# (build-transitions
                              (quote ~args)
                              (apply vector ~args)
                              (quote ~field-names)
                              (quote ~field-modes)
                              (quote ~valid-modes)
                              (quote ~transitions))
               pclass# (-> {:pclass '~name}
                         (assoc-if :doc ~doc)
                         (assoc-if :args (quote ~args))
                         (assoc-if :fields fields#)
                         (assoc-if :modes modes#)
                         (assoc-if :methods ~methods)
                         (assoc-if :transitions transitions#)
                         (assoc-if :id ~(symbol 'id))
                         (assoc-if :interface ~(symbol 'interface)))
               pclass# (build-methods pclass#
                         (quote ~field-names)
                         (quote ~field-modes)
                         (quote ~valid-modes))
               initial# (determine-mode pclass#)]
           (with-meta
             (assoc-if pclass# :mode initial#)
             {:pamela :pclass-instance}))))))

(defn unknown?
  "True if x is an unresolved symbol"
  {:added "0.2.0"}
  [x]
  (and (symbol? x) (nil? (resolve x))))

(defn method-call?
  "Return true if x is a method-call form"
  {:added "0.2.0"}
  [x]
  (and (instance? clojure.lang.PersistentList x) (unknown? (first x))))

(defn legal-bounds? [bounds]
  (and
    (vector? bounds)
    (= 2 (count bounds))
    (number? (first bounds))
    (or (= :infinity (second bounds)) (number? (second bounds)))))

;; NOTE: grammar
;; plant-fn = <LP> symbol <'$'> symbol plant-opt* argval* <RP>
;; plant-opt = ( opt-label | opt-bounds | opt-cost | opt-reward | opt-controllable )
(defn pamela-method
  "Return a function for the pamela method name"
  {:added "0.2.0"}
  [method]
  (fn [& args]
    (loop [opts {} args args]
      (cond
        (zero? (count args))
        (list method opts)
        (= :label (first args))
        (if (not (keyword? (second args)))
          (throw (AssertionError.
                   (str "label: not a keyword for method: " method "\n")))
          (if (not (nil? (:label opts)))
            (throw (AssertionError.
                     (str "label: may only be specified once for method: "
                       method "\n")))
            (recur (assoc opts :label (second args)) (nthrest args 2))))
        (= :bounds (first args))
        (if (not (legal-bounds? (second args)))
          (throw (AssertionError.
                   (str "bounds: not a vector for method: " method "\n")))
          (if (not (nil? (:bounds opts)))
            (throw (AssertionError.
                     (str "bounds: may only be specified once for method: "
                       method "\n")))
            (recur (assoc opts :bounds (second args)) (nthrest args 2))))
        (= :cost (first args))
        (if (not (number? (second args)))
          (throw (AssertionError.
                   (str "cost: value must be a number: " method "\n")))
          (if (not (nil? (:cost opts)))
            (throw (AssertionError.
                     (str "cost: may only be specified once for method: "
                       method "\n")))
            (recur (assoc opts :cost (second args)) (nthrest args 2))))
        (= :reward (first args))
        (if (not (number? (second args)))
          (throw (AssertionError.
                   (str "reward: value must be a number: " method "\n")))
          (if (not (nil? (:reward opts)))
            (throw (AssertionError.
                     (str "reward: may only be specified once for method: "
                       method "\n")))
            (recur (assoc opts :reward (second args)) (nthrest args 2))))
        (= :controllable (first args))
        (if (not (boolean? (second args)))
          (throw (AssertionError. (str "controllable: value must be true or false\n")))
          (if (not (nil? (:controllable opts)))
            (throw (AssertionError.
                     (str "controllable: may only be specified once for method: "
                       method "\n")))
            (recur (assoc opts :controllable (second args)) (nthrest args 2))))
        :else
        (cons method (cons opts args))))))

(defn pamela-call
  "Create a pamela method call"
  {:added "0.2.0"}
  [x]
  (cons (list 'pamela-method (list 'symbol (str (first x)))) (rest x)))

(defn pamela-arg
  "Keywordize x as an arg"
  {:added "0.2.0"}
  [x]
  (keyword (str "arg:" x)))

(defn pamela-arg?
  "Returns true if x is a pamela-arg"
  {:added "0.2.0"}
  [x]
  (and (keyword? x)
    (.startsWith (str x) ":arg:")))

(defn un-pamela-arg
  "Convert from keyword to symbol for pamela arg"
  {:added "0.2.0"}
  [x]
  (let [arg (subs (str x) 5)]
    (symbol arg)))

(defn replace-pamela-calls
  "Replace pamela method calls"
  {:added "0.2.0"}
  [body]
  (if body
    (walk-exprs method-call? pamela-call body)))

(defn replace-pamela-args
  "Replace pamela args with keywordized equivalent"
  {:added "0.2.0"}
  [body]
  (if body
    (prewalk #(if (unknown? %) (pamela-arg %) %) body)))

(defn restore-pamela-args
  "Unkeywordize pamela args"
  {:added "0.2.0"}
  [body]
  (if body
    (prewalk #(if (pamela-arg? %) (un-pamela-arg %) %) body)))

(defn prepare-betweens [betweens]
  (let [b (atom {:betweens []
                 :labels {}})]
    (doseq [between betweens]
      (cond
        (< (count between) 5)
        (throw (AssertionError.
                 (str "between form invalid")))
        (not (symbol? (first between)))
        (throw (AssertionError.
                 (str "expected between symbol: " (first between) "\n")))
        (nil? (#{'between 'between-starts 'between-ends} (first between)))
        (throw (AssertionError.
                 (str "invalid symbol (expected between): " (first between) "\n")))
        :else
        (let [bopts (eval between)]
          (swap! b update-in [:betweens]
            conj (vec (cons (keyword (first bopts)) (rest bopts))))
          (swap! b update-in [:labels]
            assoc (second bopts) 0 (nth bopts 2) 0))))
    @b))

;; NOTE: these :pre and :post conditions are NOT clojure fn conditions
(defmacro defpmethod
  "Define a pclass method (within a defpclass body).

  **(defpmethod name conds [args*] body)**

  *where*

  **conds** is a map of conditions:
  {:pre pre-conditions
   :post post-conditions
   :bounds [lb ub]
   :doc docstring}

  **args** is a list of method arguments (currently no arguments are supported)

  **body** is the method body which may either be :primitive
  for primitive methods or comprised of PAMELA functions."
  {:added "0.2.0" :doc/format :markdown}
  [name conds & args-body-betweens]
  (validate-name name)
  (let [[conds args body-betweens]
        (if (vector? conds)
          [{} conds args-body-betweens]
          [conds (first args-body-betweens) (rest args-body-betweens)])
        body (first body-betweens)
        b (:betweens (prepare-betweens (rest body-betweens)))
        safe-body (restore-pamela-args (replace-pamela-calls body))
        argstrs (mapv str args)]
    `(apply
       (fn [~@args]
         {'~name
          (assoc ~conds
            :args (quote ~args)
            :body ~safe-body
            :betweens ~b)})
       (map symbol ~argstrs))))

(defn describe-pclass-methods
  "Prints the methods of pclass."
  {:added "0.2.0"}
  [pclass]
  (clj/when (map? pclass)
    (let [methods (:methods pclass)
          ms (keys methods)
          n (count ms)]
      (clj/when (pos? n)
        (println "pclass" (:pclass pclass) "has" n
          (str "method" (if (> n 1) "s")))
        (doseq [m ms]
          (let [method (get methods m)
                {:keys [pre post bounds doc body]} method
                primitive? (= body :primitive)]
            (println m (str " (" (if primitive? "primitive" "pamela") ")"))
            (if doc (println " :doc\t" doc))
            (if pre (println " :pre\t" pre))
            (if post (println " :post\t" post))
            (if bounds (println " :bounds\t" bounds))
            ))))))

(defn describe-pclass-transitions
  "Prints the transitions of pclass."
  {:added "0.2.0"}
  [pclass]
  (clj/when (map? pclass)
    (let [transitions (:transitions pclass)
          n (count transitions)]
      (if (pos? n)
        (let [fromtos (keys transitions)]
          (println "pclass" (:pclass pclass) "has" n
            (str "transition" (if (> n 1) "s")))
          (doseq [fromto fromtos]
            (let [[from to] (map keyword (string/split (name fromto) #":"))
                  doc (get-in transitions [fromto :doc])]
              (println from "->" to "   " doc))))))))

(defn- get-lvars
  "Returns the lvars in a pclass (recursively).

   NOTE: this is a helper function for get-all-lvars and shouldn't
   be called directly."
  {:added "0.2.0"}
  [p & [context]]
  (let [{:keys [pclass fields transitions]} p
        context (if context (conj context pclass) [pclass])]
    ;; (println "checking pclass:" pclass)
    (apply merge
      (concat
        (for [field (keys fields)]
          (let [v (get-in fields [field :initial])]
            ;; (println "checking field:" field "v" v "lvar?" (lvar? v))
            (if (lvar? v)
              (let [x {(gensym)
                       {(:lvar v)
                        (conj context :field field)}}]
                ;; (println "found field:" x)
                x)
              (if (pclass-instance? v)
                (get-lvars v (conj context :field field))
                ))))
        (for [transition (keys transitions)]
          (let [tv (get transitions transition)]
            ;; (println "checking transition:" transition)
            (apply merge
              (concat
                (for [predicate (keys tv)]
                  (let [v (get tv predicate)]
                    ;; (println "checking predicate:" predicate
                    ;;   "v" v "lvar?" (lvar? v))
                    (if (lvar? v)
                      (let [x
                            {(gensym) {(:lvar v)
                                       (conj context :transition transition predicate)}}]
                        ;; (println "found predicate:" x)
                        x))))))))))))

(defn get-all-lvars
  "Returns the lvars in a pclass (recursively)."
  {:added "0.2.0"}
  [pclass]
  (let [lvar-map (get-lvars pclass)
        combine #(let [k (first (keys %3))
                       v (first (vals %3))
                       prev (get %1 k)]
                   (assoc %1 k
                     (if prev
                       (conj prev v)
                       [v])))]
    (reduce-kv combine {} lvar-map)))

(defn describe-pclass-lvars
  "Prints the lvars in a pclass."
  {:added "0.2.0"}
  [pclass]
  (clj/when (map? pclass)
    (pp/pprint (get-all-lvars pclass))))

(defn model-depends
  "Return dependencies of a model."
  {:added "0.2.0"}
  [model-name]
  (let [model-name (if (symbol? model-name) model-name (symbol model-name))
        model-var (get-model-var model-name)
        model-meta (meta model-var)
        version (:version model-meta)
        depends (:depends model-meta)]
    (if depends
      (apply conj [model-name version]
        (for [d depends
              :let [m (first d)]]
          (model-depends m)))
      [model-name version])))

(defn model-reverse-depends
  "Return reverse dependencies of a model."
  {:added "0.2.0"}
  []
  (let [model-vars (get-model-vars)
        get-deps #(assoc %1 %2 {:version (:version (meta %3))
                                :depends (:depends (meta %3))})
        model-deps (reduce-kv get-deps {} model-vars)
        get-rdeps #(let [version (:version %3)
                         depends (:depends %3)
                         rdeps (for [d depends
                                     :let [m (first d)]]
                                 {m {%2 [%2 version]}})]
                     (if (empty? rdeps)
                       %1
                       (assoc %1 :rdepends
                         (apply merge
                           (cons (:rdepends %1) rdeps)))))
        model-rdeps (reduce-kv get-rdeps model-deps model-deps)
        merge-rdeps #(if (= %2 :rdepends)
                       %1
                       (let [rdeps-map (get (:rdepends %1) %2)
                             rdeps (vec (vals rdeps-map))]
                         (assoc %1 %2 (assoc %3 :rdepends rdeps))))
        rdepends (reduce-kv merge-rdeps model-rdeps model-rdeps)
        ]
    ;; model-indexes (apply merge
    ;;                 (for [i (range (count models))
    ;;                       :let [m (get models i)]]
    ;;                   {(:lvar m) {:i i :depends (:depends m)}}))
    (dissoc rdepends :rdepends)))

(defn describe-fields
  "Describes PAMELA class fields (helper function for describe-model)."
  {:added "0.2.0"}
  [fields]
  (clj/when fields
    (->> (cons ""
           (for [field (keys fields)]
             (let [value (get fields field)]
               ;; (str "  " field "=" value)
               (str "  " field " is "
                 (cond
                   (nil? value)
                   "nil ERROR"
                   (lvar? value)
                   (str "lvar \"" (:lvar value) "\"")
                   (enumeration-pclass? value)
                   (str "an enumeration \"" (:pclass value)
                     "\" currently: " (:mode value))
                   (pclass? value)
                   (str "an instance of pclass \"" (:pclass value) "\"")
                   :else (str "unknown: " value))))))
      (string/join \newline))
    ))

(defn describe-modes
  "Describes PAMELA class modes (helper function for describe-model)."
  {:added "0.2.0"}
  [modes]
  (clj/when modes
    (->> (cons ""
           (for [mode (keys modes)]
             (let [value (get modes mode)]
               (str "  " mode " is "
                 (cond
                   (nil? value)
                   "nil ERROR"
                   (list? value)
                   (str "a conditional: " value)
                   (true? value)
                   "an unconditional or enumeration mode"
                   :else (str "unknown: " value))))))
      (string/join \newline))))

(defn describe-methods
  "Describes PAMELA class methods (helper function for describe-model)."
  {:added "0.2.0"}
  [methods]
  (clj/when methods
    (->> (cons ""
           (for [method-name (keys methods)]
             (let [method (get methods method-name)
                   {:keys [pre post bounds doc body]} method
                   primitive? (= body :primitive)]
               (str "  " method-name " is a "
                 (if primitive? "primitive" "pamela")
                 " method: " doc))))
      (string/join \newline))))

(defn describe-transitions
  "Describes PAMELA class transitions (helper function for describe-model)."
  {:added "0.2.0"}
  [transitions]
  (clj/when transitions
    (->> (cons ""
           (for [fromto (keys transitions)]
             (let [[from to] (map keyword (string/split (name fromto) #":"))
                   doc (get-in transitions [fromto :doc])]
               (str "  " from " -> " to "   " doc))))
      (string/join \newline))))

(defn describe-lvars
  "Prints the lvars in a pclass (helper function for describe-model)."
  {:added "0.2.0"}
  [pclass]
  (let [all-lvars (get-all-lvars pclass)
        lvars (sort (keys all-lvars))]
    (clj/when lvars
      (->> (cons ""
             (for [lv lvars]
               (str "  \"" lv "\" is used at " (get all-lvars lv))))
        (string/join \newline)))))

(defn describe-pclass-instance
  "Describes a PAMELA class. The pclass may be given as string
  (pclass name) a symbol (for the pclass) or an instance of the pclass."
  {:added "0.2.0"}
  [model model-var pclass]
  (when-not (symbol? model)
    (println "ERROR: first argument to describe-pclass-instance should be a symbol"))
  (when-not (var? model-var)
    (println "ERROR: second argument to describe-pclass-instance should be a var"))
  (when-not (map? pclass)
    (println "ERROR: third argument to describe-pclass-instance should be a pclass instance"))
  (if (and (symbol? model) (var? model-var) (map? pclass))
    (let [model-meta (meta model-var)
          rdepends (model-reverse-depends)
          rdeps (:rdepends (get rdepends model))
          header (str "DESCRIBE MODEL: " model)
          pdoc (str "  doc: " (or (:doc model-meta) ""))
          picon (str "  icon: " (or (:icon model-meta) ""))
          ptype (str "  type: " (name (:pamela model-meta)))
          pversion (str "  version: \"" (or (:version model-meta) "") "\"")
          pdepends (str "  depends: " (or (:depends model-meta) ""))
          prdepends (str "  rdepends: " (or rdeps ""))
          {:keys [fields modes methods transitions id interface]} pclass
          prdepends-options (if (or id interface)
                              (str prdepends
                                (if id
                                  (str "\n  id: " id))
                                (if interface
                                  (str "\n  interface: " interface)))
                              prdepends)
          pfields (str "fields:" (describe-fields fields))
          pmodes (str "modes:" (describe-modes modes))
          pmethods (str "methods:" (describe-methods methods))
          ptransitions (str "transitions:" (describe-transitions transitions))
          pvars (str "lvars:" (describe-lvars pclass))
          lines [header pdoc picon ptype
                 pversion pdepends prdepends-options
                 pfields pmodes pmethods ptransitions
                 pvars
                 ""]
          model-desc (string/join \newline lines)]
      model-desc)))

(defn describe-model
  "Describes a PAMELA class. The pclass may be given as string
  (pclass name) a symbol (for the pclass) or an instance of the pclass."
  {:added "0.2.0"}
  [pclass]
  (cond
    (map? pclass) ;; this is an instance
    (let [p (:pclass pclass)
          pvar (get-model-var p)]
      (describe-pclass-instance p pvar pclass))
    (fn? pclass) ;; a pclass function
    (let [p (:name (meta pclass))
          pvar (get-model-var p)
          arglist (:args (meta pvar))
          args (map lvar arglist)
          pclass (apply pclass args)] ;; create an instance
      (describe-pclass-instance p pvar pclass))
    (or (symbol? pclass) (string? pclass))
    (let [p (symbol pclass)
          pvar (get-model-var p)]
      (if pvar
        (let [pfun (deref pvar)
              arglist (:args (meta pvar))
              args (map lvar arglist)
              pclass (apply pfun args)] ;; create an instance
          (describe-pclass-instance p pvar pclass))
        (println "ERROR: model not found:" pclass)))
    :else
    (println "ERROR: argument to describe-model should be a PAMELA class instance, pclass (function), pclass name, or pclass symbol")))
