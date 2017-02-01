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

(ns pamela.parser
  "The parser for the PAMELA language"
  (:require [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :as pp :refer [pprint]]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [me.raynes.fs :as fs]
            [clojure.java.io :refer [resource]]
            [camel-snake-kebab.core :as translate]
            [pamela.utils :refer [output-file display-name-string]]
            [avenir.utils :refer [and-fn assoc-if vec-index-of concatv]]
            [instaparse.core :as insta])
  (:import [java.io
            File]
           [java.lang
            Long Double]))


;; This is a complement to me.raynes.fs and will return the name
;; of each file (or string).
(defn fs-file-name [path]
  (cond
    (= (type path) File) ;; (fs/file? path)
    (.getName path)
    (string? path)
    path
    :else
    (str path)))

;; When a magic file is read in the lvar "name" is assigned a value
;;   in pamela-lvars
;; Then when the PAMELA is parsed each time an lvar is encountered
;;   If it is NOT already in pamela-lvars (or is :unset) then it is added
;;   with the default value (if specified) or :unset
;; Upon emitting the IR all the pamela-lvars are recorded
;;   under the pamela/lvars symbol
(def pamela-lvars (atom {}))

(def #^{:added "0.3.0"}
  default-bounds
  "Default bounds"
  [0 :infinity])

(def zero-bounds [0 0])

(def zero-bounds-type {:type :bounds, :value zero-bounds})

(def default-bounds-type {:type :bounds, :value default-bounds})

(def zero-delay {:type :delay
                 :temporal-constraints [zero-bounds-type]
                 :body nil})

(def default-delay {:type :delay
                    :temporal-constraints [default-bounds-type]
                    :body nil})

(defn pamela-filename? [filename]
  (string/ends-with? filename ".pamela"))

(defn build-parser [& [ebnf-filename]]
  (let [ebnf-filename (or ebnf-filename "pamela.ebnf")
        ebnf (slurp (resource (str "public/" ebnf-filename)))
        whitespace (insta/parser "whitespace = #'([,\\s]+|;.*\\n)+'")
        parser (insta/parser ebnf
                 :input-format :ebnf
                 :auto-whitespace whitespace)]
    parser))

;; IR helper functions

(defn ir-boolean [v]
  (and (vector? v) (= (first v) :TRUE)))

(defn ir-integer [v]
  (Long/parseLong v))

(defn ir-float [v]
  (Double/parseDouble v))

(defn ir-field-type [v]
  (if (map? v)
    v
    (if (symbol? v)
      {:type :arg-reference
       :name v}
      {:type :literal
       :value v})))

(defn ir-lvar-ctor [& args]
  (let [[name lvar-init] args
        magic-init (if name (get @pamela-lvars name))]
    (when (and name (nil? magic-init)) ;; not in pamela-lvars
      (swap! pamela-lvars assoc name (or lvar-init :unset)))
    (assoc-if
      {:type :lvar
       :name (or name :gensym)}
      :default lvar-init)))

(defn ir-id [id]
  {:id id})

(defn ir-plant-part [plant-part]
  {:plant-part plant-part})

(defn ir-interface [interface]
  {:interface interface})

(defn ir-pclass-ctor [name & args-opts]
  (loop [args [] options {} a (first args-opts) more (rest args-opts)]
    (if-not a
      (merge
        {:type :pclass-ctor
         :pclass name
         :args args}
        options)
      (let [args (if (map? a) args (conj args a))
            options (if (map? a) (merge options a) options)]
        (recur args options (first more) (rest more))))))

(defn ir-mode-expr [pclass mode]
  {:type :mode-reference
   :pclass pclass
   :mode mode})

(defn ir-field-expr [field pclass]
  {:type :field-reference
   :pclass pclass
   :field field})

(defn ir-field [field & field-inits]
  (loop [field-map {:access :private :observable false}
         field-init (first field-inits) more (rest field-inits)]
    (if-not field-init
      {field field-map}
      (let [fi (if (and (vector? field-init)
                     (= :field-init (first field-init)))
                 (second field-init)
                 field-init)
            initial (if (not (vector? fi))
                      fi
                      (if (and (vector? fi)
                            (= :initial (first fi)))
                        (second fi)))
            access (if (and (vector? fi)
                            (= :access (first fi)))
                         (second fi))
            observable (if (and (vector? fi)
                            (= :observable (first fi)))
                         (second fi))
            field-map (assoc-if
                        (if (or initial (false? initial))
                          (assoc field-map :initial initial)
                          field-map)
                        :access access
                        :observable observable)]
        (recur field-map (first more) (rest more))))))

(defn ir-mode-enum [& modes]
  (zipmap modes (repeat {:type :literal :value true})))

(defn ir-mode-init [mode v]
  {mode (if (= v [:TRUE]) {:type :literal :value true} v)})

(defn ir-merge [& ms]
  (if (empty? ms)
    {}
    (apply merge ms)))

(defn ir-k-merge [k & ms]
  {k (apply merge ms)})

(defn ir-methods [& methods]
  {:methods (apply merge methods)})

(defn ir-cond-expr [op & operands]
  {:type op
   :args (vec operands)})

(defn ir-map-kv [k v]
  {k v})

(defn ir-vec [& vs]
  (if (empty? vs)
    []
    (vec vs)))

(defn ir-defpclass [pclass args & options]
  {pclass
   (apply merge {:type :pclass} {:args args} options)})

(defn ir-defpmethod [method & args]
  (loop [m {:pre {:type :literal :value true}
            :post {:type :literal :value true}
            :cost 0
            :reward 0
            :controllable false
            :temporal-constraints [default-bounds-type]
            :betweens []
            :primitive false
            :display-name (display-name-string method)
            :body nil}
         args-seen? false
         a (first args)
         more (rest args)]
    ;; (println "method" method)
    ;; (println "args" args)
    (if-not a
      {method (assoc m :primitive (or (nil? (:body m)) (:primitive m)))}
      (let [[args-seen? m] (if (not args-seen?)
                             (if (map? a)
                               [false (merge m a)] ;; merge in cond-map
                               [0 (assoc m :args a)]) ;; merge in :args
                             (if (zero? args-seen?) ;; fn
                               [1 (assoc m :body
                                    (if (vector? a) a [a]))]
                               [(inc args-seen?) (update-in m [:betweens]
                                                   conj a)]))]
        (recur m args-seen? (first more) (rest more))))))

(defn ir-bounds-literal [lb ub]
  [lb (if (= ub [:INFINITY]) :infinity ub)])

(defn ir-opt-bounds [bounds]
  {:temporal-constraints
   [{:type :bounds
     :value bounds}]})

(defn ir-plant-fn [& args]
  (let [[pclass method used] (if (symbol? (first args))
                               (if (symbol? (second args))
                                 [(first args) (second args) 2] ;; remote method
                                 ['this (first args) 1]) ;; local method
                               (let [[p m]
                                     (string/split (name (first args)) #"\." 2)]
                                 [(keyword p) (symbol m) 1]))
        [type pclass-type] (if (symbol? pclass)
                             [:plant-fn-symbol :name]
                             [:plant-fn-field :field])
        args (nthrest args used)]
    (loop [plant-opts {} argvals [] a (first args) more (rest args)]
      (if-not a
        (merge
        {:type type
         pclass-type pclass
         :method method
         :args argvals}
        plant-opts)
        (let [[opt-or-arg v] a]
          (if (= opt-or-arg :plant-opt)
            (recur (merge plant-opts v) argvals (first more) (rest more))
            (recur plant-opts (conj argvals v) (first more) (rest more))))))))

(defn ir-fn [f & args]
  (loop [fn-opts {} body [] a (first args) more (rest args)]
    (if-not a
      (merge {:type f :body (if (empty? body) nil body)} fn-opts)
      (if (and (vector? a) (#{:fn-opt :delay-opt} (first a)))
        (recur (merge fn-opts (second a)) body (first more) (rest more))
        (recur fn-opts (conj body a) (first more) (rest more))))))

(defn ir-fn-cond [f cond-expr & args]
  (loop [fn-opts {:condition cond-expr} body [] a (first args) more (rest args)]
    (if-not a
      (merge {:type f :body (if (empty? body) nil body)} fn-opts)
      (if (and (vector? a) (#{:fn-opt :delay-opt} (first a)))
        (recur (merge fn-opts (second a)) body (first more) (rest more))
        (recur fn-opts (conj body a) (first more) (rest more))))))

(defn ir-choice [& args]
  (loop [choice-opts {} body [] a (first args) more (rest args)]
    ;; (log/warn "IR-CHOICE" a)
    (if-not a
      (merge {:type :choice :body (if (empty? body) nil body)} choice-opts)
      (if (and (vector? a) (= :choice-opt (first a)))
        (let [choice-opt (second a)]
          ;; (log/warn "IR-CHOICE OPT" choice-opt);;
          (if (map? choice-opt)
            (recur (merge choice-opts choice-opt)
              body (first more) (rest more))
            (let [[opt val] choice-opt]
              (if (= opt :guard)
                (recur (assoc choice-opts :condition val)
                  body (first more) (rest more))
                (recur (assoc choice-opts opt val)
                  body (first more) (rest more))))))
        (recur choice-opts (conj body a) (first more) (rest more))))))

(defn ir-choose [f & args]
  ;; (log/warn "IR-CHOOSE" f (pr-str args))
  (loop [choose-opts {} body [] a (first args) more (rest args)]
    (if-not a
      (merge {:type f :body (if (empty? body) nil body)} choose-opts)
      (if (and (vector? a) (#{:choose-opt :delay-opt} (first a)))
        (if (vector? (second a))
          (if (= (first (second a)) :fn-opt)
            (let [fn-opt (second (second a))]
              (recur (merge choose-opts fn-opt) body (first more) (rest more)))
            (let [a1 (second a)
                  opt (first a1)
                  opt (if (= opt [:MIN])
                        :min
                        (if (= opt [:MAX])
                          :max
                          (if (= opt [:EXACTLY])
                            :exactly
                            :unknown-choose-opt ;; FIXME
                            )))
                  val (second a1)]
              (recur (assoc choose-opts opt val) body (first more) (rest more))))
          (recur (merge choose-opts (second a)) body (first more) (rest more)))
        (recur choose-opts (conj body a) (first more) (rest more))))))

(defn make-slack-sequence [body]
  {:type :sequence
   :body (concatv
           [default-delay]
           (interpose default-delay body)
           [default-delay])})

(defn make-slack-parallel [body]
  {:type :parallel
   :body (mapv (comp make-slack-sequence vector) body)})

(defn make-optional [body]
  {:type :choose
   :body [{:type :choice
           :body [zero-delay]}
          {:type :choice
           :body body}]})

(defn make-soft-sequence [body]
  {:type :sequence
   :body (mapv (comp make-optional vector) body)})

(defn make-soft-parallel [body]
  {:type :parallel
   :body (mapv (comp make-optional vector) body)})

(defn ir-slack-fn [f & args]
  (let [slack-fn (apply ir-fn f args)
        {:keys [type body]} slack-fn
        fn-opts (dissoc slack-fn :type :body)
        slack-fn (case type
                   :slack-sequence (make-slack-sequence body)
                   :slack-parallel (make-slack-parallel body)
                   :optional (make-optional body)
                   :soft-sequence (make-soft-sequence body)
                   :soft-parallel (make-soft-parallel body)
                   ;; default
                   {:error (log/error "invalid slack type:" type)})]
    (merge slack-fn fn-opts)))

(defn ir-between [f from to & args]
  (loop [between-opts {:type f
                       :from from
                       :to to}
         a (first args) more (rest args)]
    (if-not a
      between-opts
      (recur (merge between-opts a) (first more) (rest more)))))

(defn ir-try [& args]
  (loop [fn-opts {} body [] catch false a (first args) more (rest args)]
    (if-not a
      (merge {:type :try
              :body (if (empty? body) nil body)
              :catch (if (false? catch) nil catch)}
        fn-opts)
      (if catch
        (recur fn-opts body (conj catch a) (first more) (rest more))
        (if (and (map? a) (:temporal-constraints a))
          (recur (merge fn-opts a) body catch (first more) (rest more))
          (if (= a [:CATCH])
            (recur fn-opts body [] (first more) (rest more))
            (recur fn-opts (conj body a) catch (first more) (rest more))))))))

(defn ir-tell [cond-expr]
  {:type :tell
   :condition cond-expr})

(defn ir-inherit [& args]
  {:inherit (vec args)})

(defn ir-dotimes [times fn]
  {:type :sequence
   :body (vec (repeat times fn))})


;; If you're doing some REPL-based development, and change any of the above helper functions:
;;    !!! Don't forget to re-eval pamela-ir !!!
(def pamela-ir {
                ;; :access handled in ir-field
                :and-expr (partial ir-cond-expr :and)
                :args ir-vec
                ;; :argval handled in ir-plant-fn
                :ask (partial ir-fn-cond :ask)
                :assert (partial ir-fn-cond :assert)
                :between (partial ir-between :between)
                :between-ends (partial ir-between :between-ends)
                :between-opt identity
                :between-starts (partial ir-between :between-starts)
                :between-stmt identity
                :boolean ir-boolean
                :bounds identity
                :bounds-literal ir-bounds-literal
                :choice ir-choice
                ;; :choice-opt handled by ir-choice
                :choose (partial ir-choose :choose)
                ;; :choose-opt handled by ir-choose
                :choose-whenever (partial ir-choose :choose-whenever)
                :cond identity
                :cond-expr identity
                :cond-map ir-merge
                :cond-operand identity
                :controllable (partial ir-map-kv :controllable)
                :cost (partial ir-map-kv :cost)
                :cost-le (partial ir-map-kv :cost<=)
                :defpclass ir-defpclass
                :defpmethod ir-defpmethod
                :delay (partial ir-fn :delay)
                ;; :delay-opt handled in ir-fn and ir-fn-cond
                :dep ir-map-kv
                :depends (partial ir-k-merge :depends)
                :display-name (partial ir-map-kv :display-name)
                :doc (partial ir-map-kv :doc)
                :dotimes ir-dotimes
                ;; :enter handled by ir-choice
                :equal-expr (partial ir-cond-expr :equal)
                :exactly ir-vec
                :field ir-field
                :field-expr ir-field-expr
                ;; :field-init handled in ir-field
                :field-type ir-field-type
                :fields (partial ir-k-merge :fields)
                :float ir-float
                :fn identity
                ;; :fn-opt handled in ir-fn and ir-fn-cond
                ;; :guard handled in ir-choice
                :icon (partial ir-map-kv :icon)
                :id ir-id
                :implies-expr (partial ir-cond-expr :implies)
                :inherit ir-inherit
                :initial identity
                :integer ir-integer
                :interface ir-interface
                :keyword #(keyword (subs % 1))
                :label (partial ir-map-kv :label)
                ;; :leave handled by ir-choice
                :literal identity
                :lvar-ctor ir-lvar-ctor
                :lvar-init identity
                :maintain (partial ir-fn-cond :maintain)
                :max ir-vec
                :meta (partial ir-k-merge :meta)
                :meta-entry identity
                :methods ir-methods
                :min ir-vec
                :mode-enum ir-mode-enum
                :mode-expr ir-mode-expr
                :mode-init ir-mode-init
                :mode-map ir-merge
                :modes (partial ir-map-kv :modes)
                :natural ir-integer
                :not-expr (partial ir-cond-expr :not)
                :number identity
                :number-ref identity
                ;; :observable handled in ir-field
                :opt-bounds ir-opt-bounds
                :option identity
                :optional (partial ir-slack-fn :optional)
                :or-expr (partial ir-cond-expr :or)
                :pamela ir-merge
                :parallel (partial ir-fn :parallel)
                :pclass-arg-keyword identity
                :pclass-ctor ir-pclass-ctor
                :pclass-ctor-arg identity
                :pclass-ctor-option identity
                :pclass-name identity
                :plant-fn ir-plant-fn
                :plant-fn-symbol identity
                ;; :plant-opt handled in ir-plant-fn
                :plant-part ir-plant-part
                :post (partial ir-map-kv :post)
                :pre (partial ir-map-kv :pre)
                :probability (partial ir-map-kv :probability)
                ;; reserved-keyword only for grammer disambiguation
                ;; reserved-pclass-ctor-keyword only for grammer disambiguation
                :reward (partial ir-map-kv :reward)
                :reward-ge (partial ir-map-kv :reward>=)
                :safe-keyword identity
                :sequence (partial ir-fn :sequence)
                :slack-parallel (partial ir-slack-fn :slack-parallel)
                :slack-sequence (partial ir-slack-fn :slack-sequence)
                :soft-parallel (partial ir-slack-fn :soft-parallel)
                :soft-sequence (partial ir-slack-fn :soft-sequence)
                :string identity
                :symbol symbol
                :tell ir-tell
                :trans identity
                :trans-map ir-merge
                :transition ir-map-kv
                :transitions (partial ir-k-merge :transitions)
                :try ir-try
                :unless (partial ir-fn-cond :unless)
                :version (partial ir-map-kv :version)
                :when (partial ir-fn-cond :when)
                :whenever (partial ir-fn-cond :whenever)
                })

(def reference-types #{:mode-reference :field-reference :field-reference-mode
                       :field-reference-field})

(def literal-ref-types (conj reference-types :literal))



;; ;; :box-f , (:box-f this)
;; {:type :field-reference
;;  :pclass this ;; or other class, or field reference
;;  :field :box-f}

;; ;; :close , (mode-of this :close)
;; {:type :mode-reference
;;  :pclass this ;; or other class
;;  :mode :close}

;; ;; (whenever (= :cannon-f.:ready true)
;; {:type :field-reference-mode
;;  :pclass this ;; or other class
;;  :field :cannon-f
;;  :value :ready} ;; is a mode

;; ;; (unless (= :cannon-f.:ammunitions 0)
;; {:type :field-reference-field
;;  :pclass this ;; or other class
;;  :field :cannon-f
;;  :value :ammunitions} ;; is a field

;; unknown (e.g. compared against the mode or field of a pclass arg)
;; NOTE: a warning will be logged
;;  {:type :literal
;;   :value :high}

;; pclass arguments
;; {:type :arg-reference
;;  :name power}

;; state variables
;; {:type :state-variable
;;  :name global-state}

(defn validate-keyword  [ir state-vars pclass fields modes context kw]
  (let [[m-or-f ref] (map keyword (string/split (name kw) #"\.:" 2))
        field-ref (get fields m-or-f)
        field-pclass (if field-ref (get-in field-ref [:initial :pclass]))
        mode-ref (get modes m-or-f)]
    ;; (log/info "VALIDATE-KEYWORD context" context "m-or-f" m-or-f
    ;;   "ref" ref "field-ref" field-ref "mode-ref" mode-ref)
    (if field-ref
      (if ref
        (if (get-in ir [field-pclass :fields ref])
          ;; ref is field?
          {:type :field-reference-field
           :pclass 'this
           :field m-or-f
           :value ref} ;; is a field
          (if (get-in ir [field-pclass :modes ref])
            ;; ref is mode?
            {:type :field-reference-mode
             :pclass 'this
             :field m-or-f
             :value ref} ;; is a mode
            {:type :error
             :msg (str "field reference invalid: " kw)}))
        {:type :field-reference
         :pclass 'this
         :field m-or-f})
      (if mode-ref
        (if ref
          {:type :error
           :msg (str "cannot reference the field of a mode: " kw)}
          {:type :mode-reference
           :pclass 'this
           :mode m-or-f})
        (do
          ;; could look in context to see if we can get type hints
          ;; from other arguments
          (log/warn "unable to determine if this keyword"
            "value is valid in pclass" pclass kw)
          {:type :literal
           :value kw})))))

;; if one arg is
;;   {:type :field-reference, :pclass this, :field :pwr}
;; and we know
;;   :initial {:type :mode-reference, :pclass pwrvals, :mode :none}
;; then we can determine
;;   (keys (get-in ir [pwrvals :modes])) ==> #{:high :none}
;; such that when we see another arg
;;   {:type :literal, :value :high}
;; THEN we can convert it
;;   :high ==> {:type :mode-reference, :pclass pwrvals, :mode :high}
;; ALSO handle :field-reference-field
(defn mode-qualification [ir dpclass fields args]
  (loop [vargs [] a (first args) more (rest args)]
    (if-not a
      vargs
      (let [{:keys [type value]} a
            a (if (= type :literal)
                (loop [va a b (first args) moar (rest args)]
                  (if-not b
                    va
                    (if (= b a)
                      (recur va (first moar) (rest moar))
                      (let [a-value value
                            {:keys [type pclass field value]} b
                            pclass (if (= pclass 'this) dpclass pclass)
                            fpclass (if (#{:field-reference
                                           :field-reference-field} type)
                                      (get-in ir [pclass :fields
                                                  field :initial :pclass]))
                            fpclass (if (and fpclass
                                          (= type :field-reference-field))
                                      (get-in ir [fpclass :fields
                                                  value :initial :pclass])
                                      fpclass)
                            values (if fpclass
                                     (set (keys (get-in ir [fpclass :modes]))))
                            va (if (and (set? values) (values a-value))
                                 {:type :mode-reference,
                                  :pclass fpclass,
                                  :mode a-value}
                                 va)]
                        (recur va (first moar) (rest moar))))))
                a)]
        (recur (conj vargs a) (first more) (rest more))))))

;; return Validated condition or {:error "message"}
(defn validate-condition [ir state-vars pclass fields modes context condition]
  ;; (log/info "VALIDATE-CONDITION for" pclass
  ;;   "\nfields:" (keys fields)
  ;;   "\nmodes:" (keys modes)
  ;;   "\ncontext:" context
  ;;   "\ncondition:" condition)
  (let [{:keys [type args]} condition
        pclass-args (get-in ir [pclass :args])
        [c0 c1 c2] context
        method (if (= c0 :method) c1)
        method-args (if method (get-in ir [pclass :methods method :args]))]
    (cond
      (and (nil? type) (keyword? condition)) ;; bare keyword
      (validate-keyword ir state-vars pclass fields modes context condition)
      (literal-ref-types type)
      condition
      (= type :equal)
      (loop [vcond {:type type} vargs [] a (first args) more (rest args)]
        (if-not a
          (assoc vcond :args (mode-qualification ir pclass fields vargs))
          (cond
            (map? a) ;; already specified by instaparse
            (recur vcond (conj vargs a) (first more) (rest more))
            (keyword? a) ;; must disambiguate here
            (recur vcond
              (conj vargs (validate-keyword ir state-vars pclass fields modes
                            context a))
              (first more) (rest more))
            (symbol? a) ;; must resolve in scope here
            (recur vcond (conj vargs
                           (if (and method-args
                                 (some #(= % a) method-args))
                             {:type :method-arg-reference
                              :name a}
                             (if (some #(= % a) pclass-args)
                               {:type :arg-reference
                                :name a}
                               (do
                                 (swap! state-vars assoc a
                                   {:type :state-variable})
                                 {:type :state-variable
                                  :name a}))))
              (first more) (rest more))
            :else ;; must be a literal
            (recur vcond (conj vargs {:type :literal :value a})
              (first more) (rest more))
            )))
      :else ;; :and :or :not :implies => recurse on args
      (do
        {:type type
         :args (mapv
                 (partial validate-condition ir state-vars
                   pclass fields modes
                   (conj context type))
                 args)}))))

;; return Validated body or {:error "message"}
(defn validate-body [ir state-vars pclass fields modes methods in-method mbody]
  (loop [vmbody []
         b (if (vector? mbody) (first mbody) mbody)
         more (if (vector? mbody) (rest mbody))]
    (if (or (:error vmbody) (not b))
      vmbody
      (let [{:keys [type name method condition body]} b
            [b error] (cond
                        (and (= type :plant-fn-symbol) (= name 'this))
                        (if (nil? (get methods method))
                          [nil (str "method " method " used in method " in-method
                                 " is not defined in the pclass " pclass)]
                          [b nil])
                        (and (= type :plant-fn-symbol)
                          (or
                            (some #(= name %)
                              (get-in methods [in-method :args]))
                            (some #(= name %)
                              (get-in ir [pclass :args]))))
                        [b nil]
                        (and (= type :plant-fn-symbol)
                          (some #(= (keyword name) %)
                            (keys fields)))
                        ;; interpret name as a field reference
                        [(assoc (dissoc b :name)
                           :type :plant-fn-field
                           :field (keyword name))
                         nil]
                        (= type :plant-fn-symbol)
                        [nil (str "plant " name " used in method " in-method
                               " is not defined in the pclass " pclass)]
                        :else
                        [b nil])
            condition (if (and (not error) condition)
                        (validate-condition ir state-vars pclass fields modes
                          [:method method :body] condition))
            body (if (and (not error) (not (:error condition)) body)
                   (validate-body ir state-vars pclass fields modes methods
                     in-method body))
            vb (assoc-if b
                 :condition condition
                 :body body)
            vmbody (cond
                     error
                     {:error error}
                     (:error condition)
                     condition
                     (:error body)
                     body
                     :else
                     (conj vmbody vb))]
        (recur vmbody (first more) (rest more))))))

;; return Validated args or {:error "message"}
(defn validate-pclass-ctor-args [ir scope-pclass fields pclass args]
  (loop [vargs [] a (first args) more (rest args)]
    (if (or (not a) (:error vargs))
      vargs
      (let [a (if (keyword? a)
                (if (or (#{:id :interface :plant-part} a)
                      (get fields a)) ;; this is a field
                  a
                  {:error (str "Keyword argument to pclass constructor "
                            a " is not a field in the pclass " scope-pclass)})
                (if (symbol? a)
                  ;; is it a formal arg to scope-pclass?
                  (if (or (some #(= a %) (get-in ir [scope-pclass :args]))
                        (get fields (keyword a))) ;; this is a field
                    a
                    {:error (str "Symbol argument to pclass constructor "
                              a " is neither a formal argument to, "
                              "nor a field of the pclass " scope-pclass)})
                  a))
            vargs (if (and (map? a) (:error a))
                    a
                    (conj vargs a))]
        (recur vargs (first more) (rest more))))))

  ;; return Validated fields or {:error "message"}
(defn validate-fields [ir state-vars pclass fields]
  (let [field-vals (seq fields)]
    (loop [vfields {} field-val (first field-vals) more (rest field-vals)]
      (if (or (:error vfields) (not field-val))
        vfields
        (let [[field val] field-val
              {:keys [access observable initial]} val
              scope-pclass pclass
              {:keys [type pclass args]} initial
              args (if (and (= type :pclass-ctor) args)
                     (validate-pclass-ctor-args ir scope-pclass fields
                       pclass args)
                     args)
              val (if (and (map? args) (:error args))
                    args
                    val)
              vfields (if (and (map? val) (:error val))
                       val
                       (assoc vfields field val))]
          (recur vfields (first more) (rest more)))))))

;; return Validated modes or {:error "message"}
(defn validate-modes [ir state-vars pclass fields modes]
  (let [mode-conds (seq modes)]
    (loop [vmodes {} mode-cond (first mode-conds) more (rest mode-conds)]
      (if (or (:error vmodes) (not mode-cond))
        vmodes
        (let [[mode cond] mode-cond
              cond (validate-condition ir state-vars pclass fields modes
                     [:mode mode] cond)
              vmodes (if (:error cond)
                       cond
                       (assoc vmodes mode cond))]
          (recur vmodes (first more) (rest more)))))))

;; return Validated transitions or {:error "message"}
(defn validate-transitions [ir state-vars pclass fields modes transitions]
  (let [from-to-transitions (seq transitions)]
    (loop [vtransitions {}
           from-to-transition (first from-to-transitions)
           more (rest from-to-transitions)]
      (if (or (:error vtransitions) (not from-to-transition))
        vtransitions
        (let [[from-to transition] from-to-transition
              [from to] (map keyword (string/split (name from-to) #":"))
              ;; TODO check that from and to are valid modes
              {:keys [pre post]} transition
              pre (if pre
                    (validate-condition ir state-vars pclass fields modes
                      [:transition from-to :pre] pre))
              post (if post
                     (validate-condition ir state-vars pclass fields modes
                       [:transition from-to :post] post))
              transition (assoc-if transition
                           :pre pre
                           :post post)
              vtransitions (cond
                             (:error pre)
                             pre
                             (:error post)
                             post
                             :else
                             (assoc vtransitions from-to transition))]
          (recur vtransitions (first more) (rest more)))))))

;; return Validated methods or {:error "message"}
(defn validate-methods [ir state-vars pclass fields modes methods]
  (let [method-mdefs (seq methods)]
    (loop [vmethods {}
           method-mdef (first method-mdefs)
           more (rest method-mdefs)]
      (if (or (:error vmethods) (not method-mdef))
        vmethods
        (let [[method mdef] method-mdef
              {:keys [pre post args body]} mdef
              pre (if pre
                    (validate-condition ir state-vars pclass fields modes
                      [:method method :pre] pre))
              post (if post
                     (validate-condition ir state-vars pclass fields modes
                       [:method method :post] post))
              body (if-not (empty? body)
                     (validate-body ir state-vars pclass fields modes methods
                       method body))
              mdef (assoc-if mdef
                     :pre pre
                     :post post
                     :body body)
              vmethods (cond
                         (:error pre)
                         pre
                         (:error post)
                         post
                         (:error body)
                         body
                         :else
                         (assoc vmethods method mdef))]
          (recur vmethods (first more) (rest more)))))))

;; PAMELA semantic checks
;; Hoist state variables, disambiguate conditional expression operands
;; return Validated PAMELA IR or {:error "message"}
(defn validate-pamela [ir]
  (let [sym-vals (seq ir)
        state-vars (atom {})]
    (loop [vir {} sym-val (first sym-vals) more (rest sym-vals)]
      (if (or (:error vir) (not sym-val))
        (if (:error vir)
          vir
          (merge vir @state-vars))
        (let [[sym val] sym-val
              {:keys [type args fields modes transitions methods]} val
              pclass? (= type :pclass)
              fields (if (and pclass? fields)
                      (validate-fields ir state-vars sym fields))
              modes (if (and pclass? (not (:error fields)) modes)
                      (validate-modes ir state-vars sym fields modes))
              transitions (if (and pclass? transitions
                                (not (:error fields)) (not (:error modes)))
                            (validate-transitions ir state-vars
                              sym fields modes transitions))
              methods (if (and pclass? methods
                            (not (:error fields))
                            (not (:error modes))
                            (not (:error transitions)))
                            (validate-methods ir state-vars
                              sym fields modes methods))
              val (assoc-if val
                    :modes modes
                    :transitions transitions
                    :methods methods)
              vir (cond
                    (:error fields)
                    fields
                    (:error modes)
                    modes
                    (:error transitions)
                    transitions
                    (:error methods)
                    methods
                    :else
                    (assoc vir sym val))]
          (recur vir (first more) (rest more)))))))

(defn ir-magic [& args]
  args)

(def magic-ir {:boolean ir-boolean
               :bounds-literal ir-bounds-literal
               :float ir-float
               :integer ir-integer
               :keyword #(keyword (subs % 1))
               :literal identity
               :lvar-ctor ir-lvar-ctor
               :lvar-init identity
               :natural ir-integer
               :number identity
               :magic ir-magic
               :string identity
               })


(defn parse-magic [magic]
  (let [magic-parser (build-parser "magic.ebnf")
        magic-tree (insta/parses magic-parser (slurp magic))]
    (cond
      (insta/failure? magic-tree)
      (do
        (log/errorf "parse: invalid magic file: %s" magic)
        (log/errorf (with-out-str (pprint (insta/get-failure magic-tree))))
        false)
      (not= 1 (count magic-tree))
      (do
        (log/errorf "parse: grammar is ambiguous for magic file: %s" magic)
        (log/errorf (with-out-str (pprint magic-tree)))
        false)
      :else
      (let [lvars (insta/transform magic-ir (first magic-tree))]
        (loop [mir {} lvar (first lvars) more (rest lvars)]
          (if-not lvar
            mir
            (let [{:keys [name default]} lvar]
              (recur (assoc mir name (or default :unset))
                (first more) (rest more)))))))))

;; Will parse the pamela input file(s)
;; and return {:error "message"} on errors
;; else the PAMELA IR
;;   unless check-only? in which case it will return the parse
;;   tree as txt
(defn parse [options]
  (let [{:keys [input magic output-magic check-only?]} options
        parser (build-parser)
        mir (if magic (parse-magic magic) {})]
    (when magic
      ;; (println "Magic" magic "MIR" mir)  ;; DEBUG
      (log/debug "MAGIC input\n" (with-out-str (pprint mir))))
    (reset! pamela-lvars mir)
    (loop [ir {} input-filename (first input) more (rest input)]
      (if (or (:error ir) (not input-filename))
        (let [lvars (if check-only? [] @pamela-lvars)
              input-names (mapv fs-file-name input)
              out-magic (if (pos? (count lvars))
                          (apply str
                            ";; -*- Mode: clojure; coding: utf-8  -*-\n"
                            ";; magic file corresponding to:\n"
                            ";; " input-names "\n"
                            (for [k (keys lvars)]
                              (str "(lvar \"" k "\" " (get lvars k) ")\n"))))]
          (if out-magic
            (do
              (if output-magic
                (output-file output-magic "string" out-magic))
              (assoc ir
                'pamela/lvars
                {:type :lvars
                 :lvars lvars}))
            ir))
        (let [tree (if (fs/exists? input-filename)
                     (insta/parses parser (slurp input-filename)))
              ir (cond
                   (not (fs/exists? input-filename))
                   (let [msg (str "parse: input file not found: "
                               input-filename)]
                     (log/error msg)
                     {:error msg})
                   (insta/failure? tree)
                   (let [msg (str "parse: invalid input file: "
                               input-filename)]
                     (log/error msg)
                     (log/error (with-out-str (pprint (insta/get-failure tree))))
                     {:error msg})
                   (not= 1 (count tree))
                   (let [msg (str "parse: grammar is ambiguous for input file: "
                               input-filename)]
                     (log/error msg)
                     (log/error (with-out-str (pprint tree)))
                     {:error msg})
                   :else
                   (let [tree0 (first tree)
                         pir (if check-only?
                               {:tree tree0}
                               (validate-pamela
                                 (insta/transform pamela-ir tree0)))]
                     (if (:error pir)
                       pir
                       (merge ir pir))))]
          (recur ir (first more) (rest more)))))))
