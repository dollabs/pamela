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
            [pamela.utils :refer [make-url get-url output-file]]
            [avenir.utils :refer [and-fn assoc-if vec-index-of concatv]]
            [instaparse.core :as insta])
  (:import [java.lang
            Long Double]))

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
        ebnf (slurp (resource (str "data/" ebnf-filename)))
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

(defn ir-number [v]
  (if (integer? v)
    v
    (let [[float-kw whole fraction] v]
      (Double/parseDouble (str whole "." fraction)))))

(defn ir-field-type [v]
  (if (map? v)
    v
    (if (symbol? v)
      {:type :arg-reference
       :name v}
      {:type :literal
       :value v})))

(defn ir-lvar-ctor [& args]
  (let [[name lvar-init] args]
    (assoc-if
      {:type :lvar
       :name (or name :gensym)}
      :default lvar-init)))

(defn ir-id [id]
  {:id id})

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
            :body nil}
         args-seen? false
         a (first args)
         more (rest args)]
    (if-not a
      {method m}
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
                :doc (partial ir-map-kv :doc)
                ;; :enter handled by ir-choice
                :equal-expr (partial ir-cond-expr :equal)
                :exactly ir-vec
                :field ir-field
                :field-expr ir-field-expr
                ;; :field-init handled in ir-field
                :field-type ir-field-type
                :fields (partial ir-k-merge :fields)
                ;; :float handled in :ir-number
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
                :keyword keyword
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
                :number ir-number
                :number-ref identity
                ;; :observable handled in ir-field
                :opt-bounds ir-opt-bounds
                :option identity
                :optional (partial ir-slack-fn :optional)
                :or-expr (partial ir-cond-expr :or)
                :pamela ir-merge
                :parallel (partial ir-fn :parallel)
                ;; pclass-arg-keyword
                :pclass-ctor ir-pclass-ctor
                :pclass-ctor-arg identity
                :pclass-ctor-option identity
                :pclass-name identity
                :plant-fn ir-plant-fn
                :plant-fn-symbol identity
                ;; :plant-opt handled in ir-plant-fn
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

(defn validate-body [ir state-vars pclass fields modes method mbody]
  (loop [vmbody []
         b (if (vector? mbody) (first mbody) mbody)
         more (if (vector? mbody) (rest mbody))]
    (if-not b
      vmbody
      (let [{:keys [condition body]} b
            condition (if condition
                        (validate-condition ir state-vars pclass fields modes
                          [:method method :body] condition))
            body (if body
                   (validate-body ir state-vars pclass fields modes method body))
            vb (assoc-if b
                 :condition condition
                 :body body)]
        (recur (conj vmbody vb) (first more) (rest more))))))

(defn validate-modes-new [ir state-vars pclass fields modes]
  (let [mode-conds (seq modes)]
    (loop [vmodes {} mode-cond (first mode-conds) more (rest mode-conds)]
      (if-not mode-cond
        vmodes
        (let [[mode cond] mode-cond
              cond (validate-condition ir state-vars pclass fields modes
                     [:mode mode] cond)
              vmodes (assoc vmodes mode cond)]
          (recur vmodes (first more) (rest more)))))))

(defn validate-transitions-new [ir state-vars pclass fields modes transitions]
  (let [from-to-transitions (seq transitions)]
    (loop [vtransitions {}
           from-to-transition (first from-to-transitions)
           more (rest from-to-transitions)]
      (if-not from-to-transition
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
              vtransitions (assoc vtransitions from-to transition)]
          (recur vtransitions (first more) (rest more)))))))

(defn validate-methods-new [ir state-vars pclass fields modes methods]
  (let [method-mdefs (seq methods)]
    (loop [vmethods {}
           method-mdef (first method-mdefs)
           more (rest method-mdefs)]
      (if-not method-mdef
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
                     (validate-body ir state-vars pclass fields modes
                       method body))
              mdef (assoc-if mdef
                     :pre pre
                     :post post
                     :body body)
              vmethods (assoc vmethods method mdef)]
          (recur vmethods (first more) (rest more)))))))

;; Hoist state variables, disambiguate conditional expression operands
(defn validate-pamela [ir]
  (let [sym-vals (seq ir)
        state-vars (atom {})]
    (loop [vir {} sym-val (first sym-vals) more (rest sym-vals)]
      (if-not sym-val
        (merge vir @state-vars)
        (let [[sym val] sym-val
              {:keys [type args fields modes transitions methods]} val
              pclass? (= type :pclass)
              modes (if (and pclass? modes)
                      (validate-modes-new ir state-vars sym fields modes))
              transitions (if (and pclass? transitions)
                            (validate-transitions-new ir state-vars
                              sym fields modes transitions))
              methods (if (and pclass? methods)
                            (validate-methods-new ir state-vars
                              sym fields modes methods))
              val (assoc-if val
                    :modes modes
                    :transitions transitions
                    :methods methods)
              vir (assoc vir sym val)]
          (recur vir (first more) (rest more)))))))

(defn ir-magic [& args]
  args)

(def magic-ir {:boolean ir-boolean
               :bounds-literal ir-bounds-literal
               ;; :float handled in :ir-number
               :integer ir-integer
               :keyword keyword
               :literal identity
               :lvar-ctor ir-lvar-ctor
               :lvar-init identity
               :natural ir-integer
               :number ir-number
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
              (recur (assoc mir name default)
                (first more) (rest more)))))))))

;; return PAMELA IR
(defn parse [options]
  (let [{:keys [cwd input magic]} options
        parser (build-parser)]
    (loop [ir {} input-filename (first input) more (rest input)]
      (if (or (false? ir) (not input-filename))
        ir
        (let [input-file (fs/file (if (fs/exists? input-filename)
                                    input-filename
                                    (str cwd "/" input-filename)))
              tree (if (fs/exists? input-file)
                     (insta/parses parser (slurp input-file)))
              ir (cond
                   (not (fs/exists? input-file))
                   (do
                     (log/errorf "parse: input file not found: %s" input-filename)
                     false)
                   (insta/failure? tree)
                   (do
                     (log/errorf "parse: invalid input file: %s" input-filename)
                     (log/errorf (with-out-str (pprint (insta/get-failure tree))))
                     false)
                   (not= 1 (count tree))
                   (do
                     (log/errorf "parse: grammar is ambiguous for input file: %s"
                       input-filename)
                     (log/errorf (with-out-str (pprint tree)))
                     false)
                   :else
                   (let [pir (validate-pamela
                               (insta/transform pamela-ir (first tree)))
                         mir (if (and ir magic) (parse-magic magic))]
                     (when mir
                       (log/warn "MAGIC")
                       (log/warn (with-out-str (pprint mir))))
                     (merge ir pir)))]
          (recur ir (first more) (rest more)))))))
