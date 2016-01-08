;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

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
            [cheshire.core :as json]
            [pamela.utils :refer [and-f assoc-if make-url get-url]])
  (:import [clojure.lang Symbol PersistentList]))

;; current values of lvar's -------------------------------

(def #^{:dynamic true :added "0.2.0" :doc/format :markdown}
  world
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

(def valid-meta-keys #{:version :icon :depends :doc})

(defn get-model-vars
  "Return models."
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
         ;; models (filter only-models (.getMappings models-ns))]
         models (filter only-models (ns-publics models-ns))]
     (reduce (fn [m kv] (assoc m (first kv) (second kv))) {} models))))

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
  []
  (doseq [model (list-models)]
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
                       (str "new-class: model-name does not exist: " model-name "\n"))))]
      model-var)))

(defn get-helper-var
  "FIXME Return a model var by symbol (or name)

   If the second argument is true simply return nil
   if the model-var does not exist."
  {:pamela :models-helper :added "0.2.0"}
  [helper-name]
  (if (nil? helper-name)
    (throw (AssertionError. "cannot get-helper-var for nil\n"))
    (let [helper-name (if (symbol? helper-name) helper-name (symbol helper-name))
          ;; the following doesn't work with late binding
          ;; helper-var (resolve helper-name)
          ;; instead we'll query the namespace here...
          helpers (get-model-vars false)
          helper-var (get helpers helper-name)]
      helper-var)))

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

(declare pclass?)

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
  (let [name (if (lvar? v) (:lvar v) v)]
    (if (string? name)
      (swap! world #(assoc-in % [:lvars name] value)))))

;; get an lvar value or pclass mode
(defn mode
  "Get the current value for a LVar or pclass *(assigned :mode)*."
  {:added "0.2.0" :doc/format :markdown}
  [x]
  (let [value
        (if (or (lvar? x) (string? x))
          (let [name (if (lvar? x) (:lvar x) x)]
            (get-in @world [:lvars name]))
          (if (map? x)
            (:mode x)))]
    (if (lvar? value)
      (mode value)
      value)))

(defn get-field
  "Return field in pclass"
  {:added "0.2.0"}
  [pclass field]
  (if (map? pclass)
    (get-in pclass [:fields field])))

;; a function taking the pclass
;; which will return pclass if the field in pclass == value
;; if pclass is false or nil return false
(defn field=
  "Return pclass (true) if field in pclass = val, otherwise returns false.

   As the pclass itself is returned these functions maybe
   combined in the treading macro -> to form an effective logical and.

   Consider a lambda function which takes a pclass as an argument:
   #(-> %
      (field= :illumination :bright)
      (field= :sensed-illumination :bright))
   Will return pclass (true) if both :illumination and :sensed-illumination
   fields have the value :bright.

   A logical or could be constructed as follows:
   #(or
      (field= % :illumination :bright)
      (field= % :sensed-illumination :bright))"
  {:added "0.2.0"}
  [pclass field value]
  (if (map? pclass)
    (if (= (mode (get-field pclass field)) value)
      pclass
      false)
    false))

;; returns true of pclass is in mode
(defn in-mode?
  "Return true if pclass is determined to be in mode *(mode function evaluated)*."
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

(defn mode=
  "Return pclass (true) if pclass mode = value, otherwise returns false.

   As the pclass itself is returned these functions maybe
   combined in the treading macro -> to form an effective logical and.

   Consider a lambda function which takes a pclass as an argument:
   #(-> %
      (mode= :off)
      (field= :anode :none)
      (field= :sensed-illumination :dark))
   Will return pclass (true) if the :off mode function for pclass
   is true and the :anode field mode is and the :sensed-illumination
   field is :dark."
  {:added "0.2.0"}
  [pclass mode value]
  (if (map? pclass)
    (let [assigned-mode (mode pclass)
          determined-mode (determine-mode pclass)
          pclass-mode (or assigned-mode determined-mode)]
      (if (= pclass-mode value)
        pclass
        false)
      false)))

(defn validate-meta-depend
  "A defpclass helper: validate meta data dependency."
  {:added "0.2.0"}
  [depend]
  (if-not (vector? depend)
    (throw (AssertionError. (str "defpclass meta :depends component must be a vector (not \"" depend "\")")))
    (if-not (= 2 (count depend))
      (throw (AssertionError. (str "defpclass meta :depends component must be a vector of length 2")))
      (let [pclass (first depend)
            version (second depend)]
        (when-not (symbol? pclass)
          (throw (AssertionError. (str "defpclass meta :depends entry must start with a symbol (not \"" pclass "\")"))))
        (when-not (string? version)
          (throw (AssertionError. (str "defpclass meta :depends entry must end with a string (not \"" version "\")"))))
        (let [p (get-model-var pclass true)
              pclass-version (if p (:version (meta p)))]
          (clj/when (nil? p)
            (throw (AssertionError. (str "defpclass meta :depends upon a non-existent model: " pclass))))
          (when-not (= version pclass-version)
            (throw (AssertionError. (str "defpclass meta :depends upon "
                                      depend " but the available version is: \""
                                      pclass-version "\""))))
          [(list 'quote pclass) version])))))

(defn validate-meta-kv
  "A defpclass helper: validate meta data key and value."
  {:added "0.2.0"}
  [m k v]
  (when-not (contains? valid-meta-keys k)
    (throw (AssertionError. (str "defpclass meta key \"" k "\" invalid, must be one of: " valid-meta-keys))))
  (case k
    :version
    (do
      (when-not (string? v)
        (throw (AssertionError. (str "defpclass meta :version must be a string (not \"" v "\")"))))
      (assoc m k v))
    :doc
    (do
      (when-not (string? v)
        (throw (AssertionError. (str "defpclass meta :doc must be a string (not \"" v "\")"))))
      (assoc m k v))
    :depends
    (do
      (when-not (vector? v)
        (throw (AssertionError. (str "defpclass meta :depends must be a vector (not \"" v "\")"))))
      (assoc m k (mapv validate-meta-depend v)))
    :icon
    (do
      (when-not (string? v)
        (throw (AssertionError. (str "defpclass meta :icon must be a pathname string (not \"" v "\")"))))
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
                           sibling (as-file (str (.getParent (as-file path)) "/" v))]
                       (if (and (= protocol "file") (.exists sibling))
                         (str (as-url sibling))
                         (let [icon-url (make-url protocol host port (.getPath sibling))
                               icon-data (get-url icon-url)]
                           (if icon-data
                             (str icon-url)))))))]
        (assoc m :icon (or icon "file:///tmp/unknown.icon"))))))

(defn validate-meta
  "A defpclass helper: validate meta data."
  {:added "0.2.0"}
  [meta]
  (if (nil? meta)
    {}
    (do
      (when-not (map? meta)
        (throw (AssertionError. "defpclass :meta must be a map")))
      (reduce-kv validate-meta-kv {} meta))))

(defn validate-args
  "A defpclass/defpmethod helper: validate args is a vector of symbols."
  {:added "0.2.0"}
  [call args]
  (when-not (vector? args)
    (throw (AssertionError. (str call " expects a vector of args."))))
  (clj/when (not (or (empty? args) (reduce and-f (map symbol? args))))
    (throw (AssertionError. (str "All " call " args must be symbols")))))

(defn validate-fields
  "A defpclass helper: validate fields are one of: args, lvars, pclass, pclass-builder."
  {:added "0.2.0"}
  [args fields]
  (doseq [field fields]
    (let [k (first field)
          v (second field)]
      (condp #(= %1 %2) (type v)
        Symbol ;; must be in the args to defpclass
        (clj/when (not-any? #(= v %) args)
          (throw (AssertionError.
                   (str "Symbol " v " not in args " args))))
        PersistentList  ;; must be a lvar, pclass or pclass builder
        (let [f (first v)
              pamela (:pamela (meta (eval (list 'var (symbol (str f))))))]
          (when-not pamela
            (throw (AssertionError.
                     (str "Function " (first v) " does not return an lvar or pclass")))))
        (throw (AssertionError.
                 (str "Field value " v " is not an arg nor returns an lvar or pclass")))))))

(defn validate-arg-types
  "A defpclass helper: validate (at instanciation time) args are lvars."
  {:added "0.2.0"}
  [args]
  (clj/when (not (or (empty? args) (reduce and-f (map lvar-or-pclass? args))))
    (throw (AssertionError. "All pclass args must be LVars or pclasses."))))

;; NOTE: because this function *must* have metadata on the
;; the var pclass *and* on the function it returns Codox
;; cannot document this function correctly.
(defn ^{:pamela :pclass-builder :added "0.2.0"} pclass
  [& args]
  "Build the pclass (first arg) during instanciation with (rest args).

  For example: (pclass bulb :source :drain)"
  ^{:pamela :pclass-builder :added "0.2.0"}
  (fn [p field]
    (let [v (get-field p field)
          builder? (fn? v)]
      (if builder?
        (let [f (first args)
              kwargs (rest args)
              fields (:fields p)
              fargs (map #(if (keyword? %) (get fields %) %) kwargs)
              fieldargs (mapv #(if (keyword? %) % nil) kwargs)
              value (apply f fargs)]
          (-> p
            (assoc-in [:fields field] value)
            (assoc-in [:fieldargs field] fieldargs)))
        p))))

(defn build-fields
  "A defpclass helper: call any pclass builders as needed."
  {:added "0.2.0"}
  [pclass]
  (let [fields (keys (:fields pclass))]
    (if (pos? (count fields))
      ((reduce #(comp %1 %2) identity
         (for [k fields]
           (let [v (get-field pclass k)]
             (cond
               (lvar? v) identity
               (map? v) identity ;; a pclass instance
               (and (fn? v) (:pamela (meta v))) ;; :pamela builder fn?
               (fn [p] (v p k)) ;; call builder
               :else
               (throw (AssertionError. (str "Field value " v " illegal")))))))
       pclass)
      pclass)))

(defn build-mode
  "A defpclass helper: build the mode."
  {:added "0.2.0"}
  [p mode value]
  ^{:added "0.2.0"}
  (assoc-in p [:modes mode] value))

(defn build-modes
  "A defpclass helper: verify modes."
  {:added "0.2.0"}
  [pclass]
  (let [modes (keys (:modes pclass))]
    (if (pos? (count modes))
      ((reduce #(comp %1 %2) identity
         (for [k modes]
           (let [v (get-in pclass [:modes k])]
             (cond
               (true? v) identity
               (list? v) (fn [p] (build-mode p k v))
               :else
               (throw (AssertionError. (str "Mode value " v " illegal")))))))
       pclass)
      pclass)))

(defn build-method
  "A defpclass helper: build the method."
  {:pamela :pclass-builder-fn :added "0.2.0"}
  [pclass method]
  (let [[m f] (first (seq method))
        metaf (meta f)
        pre (:pre metaf)
        post (:post metaf)]
    (clj/when (and pre (or (not (keyword? pre))
                     (nil? (get-in pclass [:modes pre]))))
      (throw (AssertionError. (str "pclass: " (:pclass pclass) " method: " m " has :pre condition \"" pre "\" which is not one of the modes: " (vec (keys (:modes pclass)))))))
    (clj/when (and post (or (not (keyword? post))
                      (nil? (get-in pclass [:modes post]))))
      (throw (AssertionError. (str "pclass: " (:pclass pclass) " method: " m " has :post condition \"" post "\" which is not one of the modes: " (vec (keys (:modes pclass)))))))
    [m f]))

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
  [pclass]
  (let [methods (:methods pclass)]
    (if (pos? (count methods))
      (let [new-methods
            (reduce fuse-methods {}
              (map #(build-method pclass %) methods))]
        (assoc pclass :methods new-methods))
       pclass)))

(defn build-condition
  "A defpclass helper: build the tranistion condition."
  {:pamela :pclass-builder-fn :added "0.2.0"}
  [transition k v]
  (let [value (if (or (= k :pre) (= k :post))
                (if (keyword? v)
                  (list 'mode= v)
                  v)
                v)]
    (assoc transition k value)))

(defn build-transition
  "A defpclass helper: build the transition."
  {:pamela :pclass-builder-fn :added "0.2.0"}
  [transitions fromto transition]
  (assoc transitions fromto
    (reduce-kv build-condition {} transition)))

(defn build-transitions
  "A defpclass helper: build the transitions."
  {:pamela :pclass-builder-fn :added "0.2.0"}
  [pclass]
  (let [transitions (:transitions pclass)]
    (if (pos? (count transitions))
      (let [fromtos (keys transitions)]
        (doseq [fromto fromtos]
          (let [[from to] (map keyword (string/split (name fromto) #":"))]
            (clj/when (and from
                    (not= from :*)
                    (nil? (get-in pclass [:modes from])))
              (throw (AssertionError. (str "pclass: " (:pclass pclass) " has transition :FROM:TO \"" fromto "\" where the :FROM is not one of: " (vec (keys (:modes pclass)))))))
            (clj/when (and to
                    ;; (not= to :*) doesn't make sense!
                    (nil? (get-in pclass [:modes to])))
              (throw (AssertionError. (str "pclass: " (:pclass pclass) " has transition :FROM:TO \"" fromto "\" where the :TO is not one of: " (vec (keys (:modes pclass)))))))
            ))
        (assoc pclass :transitions
          (reduce-kv build-transition {} transitions))
        )
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
  (let [{:keys [meta fields modes methods transitions observable access id]} opts
        {:keys [version icon depends doc]} meta
        pclass-type (if (or (empty? modes) (map? modes)) :pclass :pclass-enumeration)
        modesmap (if-not (nil? modes)
                   (if (map? modes) modes
                      (zipmap modes (repeat true))))
        ;; arglists (list 'list (list 'quote args))
        _ (validate-args "defpclass" args)
        argstrs (mapv str args)
        url *url*] ;; bound in models.clj
    (validate-fields args fields)
    (add-to-pclasses name)
    `(def ~(with-meta name
             (merge {;; :arglists arglists
                     :args (list 'mapv 'symbol argstrs)
                     :pamela pclass-type
                     :url url}
               (validate-meta meta)))
       ^{:pamela ~pclass-type :name '~name}
       (fn [~@args & ~(symbol 'options)]
         (validate-arg-types ~args)
         (let [{:keys [~(symbol 'initial) ~(symbol 'observable)
                       ~(symbol 'access) ~(symbol 'id)]} ~(symbol 'options)
               pclass# (-> {:pclass '~name}
                         (assoc-if :doc ~doc)
                         (assoc-if :args (quote ~args))
                         (assoc-if :fields ~fields)
                         (assoc-if :modes ~modesmap)
                         (assoc-if :methods ~methods)
                         (assoc-if :transitions ~transitions)
                         (assoc-if :observable ~(symbol 'observable))
                         (assoc-if :access ~(symbol 'access))
                         (assoc-if :id ~(symbol 'id))
                         (build-fields)
                         (build-modes)
                         (build-methods)
                         (build-transitions))
               initial# (if ~(symbol 'initial)
                          (do
                            (when-not (get ~modesmap ~(symbol 'initial))
                              (throw (AssertionError.
                                       (str "pclass :initial mode " ~(symbol 'initial)
                                         " is not one of the defined modes: " (vec (keys ~modesmap))))))
                            ~(symbol 'initial))
                          (determine-mode pclass#))]
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

(defn pamela-method
  "Return a function for the pamela method name"
  {:added "0.2.0"}
  [name]
  (fn [& args]
    ;; (cons :pamela-method (cons name args))))
    (cons name args)))

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

;; takes name pre-post-delay-conditions args body
;; NOTE: these :pre and :post conditions are NOT clojure fn conditions
(defmacro defpmethod
  "Define a pclass method (within a defpclass body).

**(defpmethod name conds [args*] body)**

*where*

 **conds** is a map of conditions:
  {:pre pre-conditions
   :post post-conditions
   :delay [from to]
   :doc docstring}

**args** is a list of method arguments (currently no arguments are supported)

  **body** is the method body which may either be :primitive
  for primitive methods or comprised of PAMELA functions."
  {:added "0.2.0" :doc/format :markdown}
  [name conds args & [body]]
  (let [safe-body (restore-pamela-args (replace-pamela-calls body))
        argstrs (mapv str args)]
    `(apply
       (fn [~@args]
        {'~name
         (assoc ~conds
           :args (quote ~args)
           :body ~safe-body)})
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
                {:keys [pre post delay doc body]} method
                primitive? (= body :primitive)]
            (println m (str " (" (if primitive? "primitive" "pamela") ")"))
            (if doc (println " :doc\t" doc))
            (if pre (println " :pre\t" pre))
            (if post (println " :post\t" post))
            (if delay (println " :delay\t" delay))
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
          (let [v (get fields field)]
            ;; (println "checking field:" field)
            (if (lvar? v)
              (let [x {(gensym)
                       {(:lvar v)
                        (conj context :field field)}}]
                ;; (println "found field:" x)
                x)
              (if (map? v) (get-lvars v (conj context :field field))))))
        (for [transition (keys transitions)]
          (let [tv (get transitions transition)]
            ;; (println "checking transition:" transition)
            (apply merge
              (concat
                (for [predicate (keys tv)]
                  (let [v (get tv predicate)]
                    ;; (println "checking predicate:" predicate)
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

(defn pclass-instance?
  "Returns true if pclass is an instance pclass."
  {:pamela :models-helper :added "0.2.0"}
  [pclass]
  (cond
    (or (nil? pclass) (lvar? pclass))
    false
    (map? pclass) ;; this is an instance
    (let [p (:pclass pclass)
          pvar (get-model-var p)
          model-meta (meta pvar)]
      (= (:pamela model-meta) :pclass))
    :else
    false))

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
      (= (:pamela model-meta) :pclass))
    (fn? pclass) ;; a pclass function
    (let [p (:name (meta pclass))
          pvar (get-model-var p)
          model-meta (meta pvar)]
      (= (:pamela model-meta) :pclass))
    (or (symbol? pclass) (string? pclass))
    (let [p (symbol pclass)
          pvar (get-model-var p)
          model-meta (meta pvar)]
      (= (:pamela model-meta) :pclass))
    :else
    (println "ERROR: argument to pclass? should be a PAMELA class instance, pclass (function), pclass name, or pclass symbol")))

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
    (println "ERROR: argument to enumeration-pclass? should be a PAMELA class instance, pclass (function), pclass name, or pclass symbol")))

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
                   {:keys [pre post delay doc body]} method
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
          {:keys [fields modes methods transitions observable access id]} pclass
          prdepends-options (if (or observable access id)
                              (str prdepends
                                (if observable
                                  (str "\n  observable: " observable))
                                (if access
                                  (str "\n  access: " access))
                                (if id
                                  (str "\n  id: " id)))
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
