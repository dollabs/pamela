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
            [pamela.utils :refer [output-file display-name-string dbg-println
                                  clj19-boolean?]]
            [pamela.inheritance]
            [avenir.utils :refer [and-fn assoc-if vec-index-of concatv]]
            [instaparse.core :as insta]
            [plan-schema.utils :refer [fs-basename]]
            [plan-schema.sorting :refer [sort-mixed-map]])
  (:import [java.lang
            Long Double]))

(defn merge-keys-one
  "converts each map value v into a vector [v]"
  {:added "0.6.1"}
  ([m]
   (cond
     (map? m) (reduce-kv merge-keys-one {} m)
     :else m))
  ([m k v]
   (assoc m k (if (vector? v) v [v]))))

(defn merge-keys
  "converts all key values v into vectors [v] and coalesces values for equal keys into the respective key value"
  {:added "0.6.1"}
  ([m]
   (merge-keys-one m))
  ([m0 m1]
   (let [kvs (seq m1)]
     (loop [mk (merge-keys-one m0) kv (first kvs) more (rest kvs)]
       (if-not kv
         mk
         (let [[k v] kv
               v0 (get mk k [])
               v (conj v0 v)
               mk (assoc mk k v)]
           (recur mk (first more) (rest more)))))))
  ([m0 m1 & more]
   (apply merge-keys (merge-keys m0 m1) more)))

(defn literal? [v]
  (or (number? v) ;; literal
    (true? v) ;; (boolean? v) ;; literal
    (false? v) ;; (boolean? v) ;; literal
    (string? v) ;; literal
    (keyword? v) ;; literal
    ))

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

;; (def true-type {:type :literal, :value true})
(def true-type true)

;; (def false-type {:type :literal, :value false})
(def false-type false)

(def between-stmt-types #{:between :between-ends :between-starts})

(defn pamela-filename? [filename]
  (string/ends-with? filename ".pamela"))

(defn build-parser [& [ebnf-filename]]
  (let [ebnf-filename (or ebnf-filename "pamela.ebnf")
        ebnf (slurp (resource (str "public/" ebnf-filename)))
        whitespace (insta/parser "whitespace = #'([,\\s]+|;.*\\s)+'")
        parser (insta/parser ebnf
                 :input-format :ebnf
                 :auto-whitespace whitespace)]
    parser))

;; IR helper functions

(defn mode-ref? [m]
  (and (map? m) (= :mode-ref (:type m))))

(defn symbol-ref? [m]
  (and (map? m) (= :symbol-ref (:type m))))

(defn ir-boolean [v]
  (if (and (vector? v) (= (first v) :TRUE))
    true
    false))

(defn ir-integer [v]
  (Long/parseLong v))

(defn ir-float [v]
  (Double/parseDouble v))

;; field-type = ( literal | lvar-ctor | !lvar-ctor pclass-ctor |
;;                mode-ref | symbol-ref )
(defn ir-field-type [v]
  v)
;; (if (map? v) ;; lvar-ctor, pclass-ctor, mode-ref, or symbol-ref
;;   v
;;   {:type :literal
;;    :value v}))

;; The only rationale for not simply using identity is that we
;; may want to, at some point, change the IR to express every
;; literal as {:type :literal ...} and perhaps even encode the type
;; {:type :literal-string ...}
(defn ir-argval [v]
  v
  ;; (if (map? v) ;; already handled
  ;;   v
  ;;   {:type :literal
  ;;    :value v})
  )

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
  {:plant-id id})

(defn ir-plant-part [plant-part]
  {:plant-part plant-part})

(defn ir-interface [interface]
  {:plant-interface interface})

(defn ir-pclass-ctor [name & args-opts]
  (dbg-println :trace "ir-pclass-ctor" name "ARGS-OPTS" args-opts)
  (loop [args [] options {} a (first args-opts) more (rest args-opts)]
    (if (nil? a)
      (merge
        {:type :pclass-ctor
         :pclass name
         :args args}
        options)
      (let [arg? (or (not (map? a)) (symbol-ref? a))
            args (if arg? (conj args a) args)
            options (if arg? options (merge options a))]
        (dbg-println :trace "  arg?" arg? "A" a)
        (recur args options (first more) (rest more))))))

(defn ir-mode-ref [symbol-ref mode]
  {:type :mode-ref
   :mode-ref symbol-ref
   :mode mode})

(defn ir-symbol-ref [& symbols]
  {:type :symbol-ref
   :names (vec symbols)})

;; field = symbol ( <LM> field-init+ <RM> | field-type )
;; field-init = ( initial | access | observable | consumer | producer )
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
            consumer (if (and (vector? fi)
                             (= :consumer (first fi)))
                         (second fi))
            producer (if (and (vector? fi)
                             (= :producer (first fi)))
                         (second fi))
            field-map (assoc-if
                        (if (or initial (false? initial))
                          (assoc field-map :initial initial)
                          field-map)
                        :access access
                        :observable observable
                        :consumer consumer
                        :producer producer)]
        (recur field-map (first more) (rest more))))))

(defn ir-mode-enum [& modes]
  (zipmap modes (repeat true-type)))

(defn ir-mode-init [mode v]
  {mode (if (= v [:TRUE]) true-type v)})

(defn ir-merge [& ms]
  (if (empty? ms)
    {}
    (apply merge ms)))

(defn ir-k-merge [k & ms]
  {k (if (empty? ms)
       {}
       (apply merge ms))})

(defn ir-methods [& methods]
  {:methods (if (empty? methods)
              []
              (apply merge-keys methods))})

(defn ir-cond-expr [op & operands]
  (if (= op :COND-EXPR)
    (let [expr (first operands)]
      (cond
        (and (vector? expr) (= (first expr) :TRUE))
        true-type
        (and (vector? expr) (= (first expr) :FALSE))
        false-type
        :else
        expr))
    {:type op
     :args (vec operands)}))

(defn ir-arith-expr [op & operands]
  {:type :expr
   :operation op
   :args (vec operands)})

(defn arith-expr?
  [ast]
  (some #{:add :subtract :multiply :divide} #{(first ast)}))

(defn ir-vec [& vs]
  (if (empty? vs)
    []
    (vec vs)))

(defn ir-defpclass [pclass args & options]
  {pclass
   (apply merge {:type :pclass} {:args args} options)})

(defn default-mdef [method]
  (let [cond-map {:pre true-type
                  :post true-type
                  :cost 0
                  :reward 0
                  :controllable false
                  :temporal-constraints [default-bounds-type]
                  :betweens []
                  :primitive false
                  :display-name nil
                  :display-args nil
                  :probability 1.0
                  :body nil}
        display-name (if method (display-name-string method))]
    (assoc-if cond-map :display-name display-name)))

(defn ir-defpmethod [method & args]
  (dbg-println :trace "ir-defpmethod" method "ARGS" args)
  (loop [m (default-mdef method)]
    (loop [m m args-seen? false a (first args) more (rest args)]
      (if (nil? a)
        {method
         (sort-mixed-map
           (assoc m :primitive (or (nil? (:body m)) (:primitive m))))}
        (let [[args-seen? m] (if (not args-seen?)
                               (if (map? a)
                                 [false (merge m a)] ;; merge in cond-map
                                 [0 (assoc m :args a)]) ;; merge in :args
                               (if (and (zero? args-seen?)
                                     (not (between-stmt-types (:type a))))
                                 [1 (assoc m :body
                                      (if (vector? a) a [a]))]
                                 [(inc args-seen?) (update-in m [:betweens]
                                                     conj a)]))]
          (dbg-println :trace "  args-seen?" args-seen? "A" a)
          ;; (println "  args-seen?" args-seen? "M"
          ;;   (with-out-str (pprint (dissoc m :body))))
          (recur m args-seen? (first more) (rest more)))))))

(defn ir-bounds-literal [lb ub]
  [lb (if (= ub [:INFINITY]) :infinity ub)])

;(defn de-expr [aval]
;  (if (and (vector? aval) (= (first aval) :expr))
;    (second aval)
;    aval))

(defn ir-opt-bounds [bounds]
  {:temporal-constraints
   [{:type :bounds
     :value bounds}]})

;; method-fn = <LP> symbol-ref method-opt* argval* <RP>
(defn ir-method-fn [symbol-ref & args]
  (dbg-println :trace "ir-method-fn symbol-ref" symbol-ref "ARGS" args)
  (loop [method-opts {} argvals [] a (first args) more (rest args)]
    (if (nil? a)
      (merge
        {:type :method-fn
         :method-ref symbol-ref
         :args argvals}
        method-opts)
      (let [[opt-or-arg v] (if (vector? a) a [nil a])]
        (if (= opt-or-arg :method-opt)
          (recur (merge method-opts v) argvals (first more) (rest more))
          (recur method-opts (conj argvals v) (first more) (rest more)))))))

;; NOTE: due to the refactoring of *-opts in the grammar as
;;     between-opt = ( opt-bounds | cost-le | reward-ge )
;;     fn-opt = ( between-opt | label )
;;     delay-opt = ( fn-opt | controllable )
;; AND the dispatching of those terminals in 'pamela-ir
;;     :between-opt identity
;;     ;; :fn-opt handled in ir-fn
;;     ;; :delay-opt handled in ir-fn
;; We handle the args as shown in comments below...
(defn ir-fn [f & args]
  (dbg-println :trace "IR-FN" f "ARGS" args)
  (loop [fn-opts {} body [] a (first args) more (rest args)]
    (dbg-println :trace "  FN-OPTS" fn-opts "A" a)
    (if (nil? a)
      (merge {:type f :body (if (empty? body) nil body)} fn-opts)
      (cond
        ;; [:fn-opt OPT] where OPT is opt-bounds | cost-le | reward-ge | label
        (and (vector? a) (= :fn-opt (first a)))
        (recur (merge fn-opts (second a)) body
          (first more) (rest more))
        ;; [:delay-opt [:fn-opt OPT]]
        ;; where OPT is opt-bounds | cost-le | reward-ge | label
        (and (vector? a) (= :delay-opt (first a))
          (vector? (second a)) (= :fn-opt (first (second a))))
        (recur (merge fn-opts (-> a second second)) body
          (first more) (rest more))
        ;; [:delay-opt {:controllable true-or-false}]
        (and (vector? a) (= :delay-opt (first a)) (map? (second a)))
        (recur (merge fn-opts (second a)) body
          (first more) (rest more))
        :else
        (recur fn-opts (conj body a)
          (first more) (rest more))))))

;; by definition (at the call sites)
;; (#{:ask :assert :maintain :unless :when :whenever} f)
(defn ir-fn-cond [f cond-expr & args]
  (dbg-println :trace "IR-FN-COND" f "COND-EXPR" cond-expr "ARGS" args)
  (let [fn {:type f
            :condition cond-expr
            :body nil}
        [arg0 arg1] args
        [fn arg1] (if (and (map? arg0)
                        (= '(:temporal-constraints) (keys arg0)))
                    [(merge fn arg0) arg1] ;; opt-bounds? present
                    [fn arg0]) ;; opt-bounds? NOT present
        fn (if arg1
             (assoc fn :body [arg1])
             fn)]
    (dbg-println :trace "IR-FN-COND" f cond-expr "ARGS" args "\nFN" fn)
    fn))

(defn ir-choice [& args]
  (dbg-println :trace "IR-CHOICE ARGS" args)
  (loop [choice-opts {} body [] a (first args) more (rest args)]
    (dbg-println :trace "IR-CHOICE-OPTS" choice-opts "A" a)
    (if (nil? a)
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
    (if (nil? a)
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
    (if (nil? a)
      between-opts
      (recur (merge between-opts a) (first more) (rest more)))))

(defn ir-try [& args]
  (loop [fn-opts {} body [] catch false a (first args) more (rest args)]
    (if (nil? a)
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

(declare pamela-ir)

(defn ir-expr [arg]
  ;; translate ARG to IR here +++
  (cond (vector? arg)
        (case (first arg)
          :number (second arg)

          else {:error (str "ir-expr don't know how to handle:" arg)})

        (number? arg)
        arg

        (map? arg)
        arg

        :otherwise
        {:error (str "ir-expr don't know how to handle:" arg)}))

(defn propositions-ref?
  [operand]
  (= (first operand) :propositions-expr))

(defn search-constraints?
  [operand]
  (and (vector? operand) (= (first operand) :search-constraints)))

;; If you're doing some REPL-based development, and change any of the above
;; helper functions: Don't forget to re-eval pamela-ir !!!
(def pamela-ir {
                ;; :access handled in ir-field
                :add-expr (partial ir-arith-expr :add)
                :and-expr (partial ir-cond-expr :and)
                :args ir-vec
                :argval ir-argval
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
                :call-expr (partial ir-cond-expr :function-call)
                :choice ir-choice
                ;; :choice-opt handled by ir-choice
                :choose (partial ir-choose :choose)
                ;; :choose-opt handled by ir-choose
                :choose-whenever (partial ir-choose :choose-whenever)
                :cond identity
                :cond-expr (partial ir-cond-expr :COND-EXPR)
                :cond-map ir-merge
                :cond-operand identity
                :controllable (partial hash-map :controllable)
                :cost (partial hash-map :cost)
                :cost-le (partial hash-map :cost<=)
                :defpclass ir-defpclass
                :defpmethod ir-defpmethod
                :delay (partial ir-fn :delay)
                ;; :delay-opt handled in ir-fn
                :dep hash-map
                :depends (partial ir-k-merge :depends)
                :display-name (partial hash-map :display-name)
                :divide-expr (partial ir-arith-expr :divide)
                :doc (partial hash-map :doc)
                :dotimes ir-dotimes
                ;; :enter handled by ir-choice
                :equal-expr (partial ir-cond-expr :equal)
                :expr ir-expr
                :gt-expr (partial ir-cond-expr :gt)
                :ge-expr (partial ir-cond-expr :ge)
                :lt-expr (partial ir-cond-expr :lt)
                :le-expr (partial ir-cond-expr :le)
                :exactly ir-vec
                :field ir-field
                ;; :field-init handled in ir-field
                :field-type ir-field-type
                :fields (partial ir-k-merge :fields)
                :float ir-float
                :fn identity
                ;; :fn-opt handled in ir-fn
                ;; :guard handled in ir-choice
                :icon (partial hash-map :icon)
                :id ir-id
                :implies-expr (partial ir-cond-expr :implies)
                :inherit ir-inherit
                :initial identity
                :integer ir-integer
                :interface ir-interface
                :keyword #(keyword (subs % 1))
                :label (partial hash-map :label)
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
                :mode-ref ir-mode-ref
                :mode-init ir-mode-init
                :mode-map ir-merge
                :modes (partial hash-map :modes)
                :multiply-expr (partial ir-arith-expr :multiply)
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
                :method-fn ir-method-fn
                ;; :method-opt handled in ir-method-fn
                :plant-part ir-plant-part
                :post (partial hash-map :post)
                :pre (partial hash-map :pre)
                :primitive (partial hash-map :primitive)
                :probability (partial hash-map :probability)
                :propositions-expr (partial ir-cond-expr :propositions-expr)
                ;; reserved-keyword  only for grammer disambiguation
                ;; reserved-pclass-ctor-keyword only for grammer disambiguation
                ;; reserved-string only for grammer disambiguation
                ;; reserved-symbol only for grammer disambiguation
                :reward (partial hash-map :reward)
                :reward-ge (partial hash-map :reward>=)
                :safe-keyword identity
                :same-expr (partial ir-cond-expr :same)
                :sequence (partial ir-fn :sequence)
                :slack-parallel (partial ir-slack-fn :slack-parallel)
                :slack-sequence (partial ir-slack-fn :slack-sequence)
                :soft-parallel (partial ir-slack-fn :soft-parallel)
                :soft-sequence (partial ir-slack-fn :soft-sequence)
                ;; stop-token only for grammer disambiguation
                :string identity
                :subtract-expr (partial ir-arith-expr :subtract)
                :symbol symbol
                :symbol-ref ir-symbol-ref
                :tell ir-tell
                :trans identity
                :transition hash-map
                :transitions (partial ir-k-merge :transitions)
                :trans-map ir-merge
                :try ir-try
                :unless (partial ir-fn-cond :unless)
                :version (partial hash-map :version)
                :when (partial ir-fn-cond :when)
                :whenever (partial ir-fn-cond :whenever)
                ;; whitespace only for grammer disambiguation
                })

;; operand may be a keyword (mode), mode-ref or symbol-ref
;; NOTE must do mode-qualification too
;; return Validated condition or {:error "message"}
(defn validate-cond-operand [ir state-vars pclass fields modes context
                             pclass-args method-args operand]
  (dbg-println :debug  "VALIDATE-COND-OPERAND context" context
    "pclass-args" pclass-args "method-args" method-args
    "operand" operand)
  (cond
    ;; Do NOT error here: we may want to have literal keywords which
    ;; are not modes.
    ;; (and (keyword? operand) (not (get modes operand))) ;; local mode
    ;; {:error (str "mode " operand " is not defined in pclass " pclass)}
    (keyword? operand)
    (if (not (get modes operand))
      (do
        (log/warn (str "mode " operand " is not defined in pclass " pclass))
        ;; {:type :literal, :value operand}
        operand)
      {:type :mode-ref
       :mode-ref {:type :symbol-ref :names ['this]}
       :mode operand})

    (and (mode-ref? operand)
      (= (get-in operand [:mode-ref :names]) '[this])
      (not (get modes (:mode operand))))
    {:error (str "mode " (:mode operand) " is not defined in pclass " pclass)}

    (mode-ref? operand)
    (let [{:keys [mode-ref]} operand
          {:keys [names]} mode-ref
          [n0 & more] names
          [n0 more vnames] (if (= 'this n0)
                             [(first more) (rest more) (vec (rest names))]
                             [n0 more names])
          vmode-ref (cond
                      (and (nil? n0) (not (empty? more)))
                      {:error (str "cannot use 'this except in the first position: " names)}
                      (nil? n0)
                      {:type :symbol-ref :names ['this]}
                      (or (= n0 'this) (some #(= 'this %) more))
                      {:error (str "cannot use 'this except in the first position: " names)}
                      (get fields n0)
                      {:type :field-ref :names vnames}
                      (and method-args (some #(= % n0) method-args))
                      {:type :method-arg-ref :names names}
                      (some #(= % n0) pclass-args)
                      {:type :pclass-arg-ref :names names}
                      ;; NOTE a mode-ref could refer to pclass
                      (= :pclass (get-in ir [n0 :type]))
                      {:type :symbol-ref :names [n0]}
                      :else
                      {:error
                       (str "a state variable (undefined symbol) cannot be a mode: "
                         operand)})]
      (if (:error vmode-ref)
        vmode-ref
        (assoc operand :mode-ref vmode-ref)))

    (symbol-ref? operand)
    (let [{:keys [names]} operand
          [n0 & more] names
          [n0 more vnames] (if (= 'this n0)
                             [(first more) (rest more) (vec (rest names))]
                             [n0 more names])]
      (cond
        (or (= n0 'this) (some #(= 'this %) more))
        {:error (str "cannot use 'this except in the first position: " names)}
        (get fields n0)
        {:type :field-ref :names vnames}
        (and method-args (some #(= % n0) method-args))
        {:type :method-arg-ref :names names}
        (some #(= % n0) pclass-args)
        {:type :pclass-arg-ref :names names}
        (not (empty? more))
        {:error
         (str "cannot dereference a state variable (undefined symbol): "
           operand)}
        :else
        (do
          (swap! state-vars assoc n0 {:type :state-variable})
          {:type :state-variable :name n0})))

    (string? operand)
    {:type :literal, :value operand}

    :else
    {:error (str "unknown conditional operand: " operand)}))

;; returns a validated method-ref map
;; or an {:error msg}
(defn validate-method-fn-method-ref [ir state-vars in-pclass
                                     fields modes methods
                                     context method-ref args]
  (dbg-println :trace  "VALIDATE-METHOD-FN-METHOD-REF" in-pclass
    "CONTEXT" context "\n METHOD-REF" method-ref
    "\n  CALLER ARITY" (count args) "ARGS" args)
  (let [pclass-args (get-in ir [in-pclass :args])
        [c-pclass c-methods c-method c-mi] context
        method (if (= c-methods :methods) c-method)
        method-args (if method
                      (get-in ir [in-pclass :methods method c-mi :args]))
        {:keys [names]} method-ref
        [n0 & more] names
        [n0 more vnames] (if (= 'this n0)
                           [(first more) (rest more) (vec (rest names))]
                           [n0 more names])]
    (cond
      (nil? n0)
      {:error "this : method not defined"}
      (or (= n0 'this) (some #(= 'this %) more))
      {:error (str "cannot use 'this except in the first position: " names)}
      (empty? more) ;; local method
      (let [caller-arity (count args)
            caller-arg-str (if (= 1 caller-arity) " arg" " args")
            candidate-mdefs (get methods n0)
            mdefs (filter #(= (count (:args %)) caller-arity) candidate-mdefs)]
        (if (= 1 (count mdefs))
          ;; found match to method with the correct arity
          (assoc method-ref :names ['this n0]) ;; match is (first mdefs)
          (let [msg (str "Call from " in-pclass "." c-method
                      " to " n0)]
            ;; consider adding the args signature to the msg to
            ;; take advantage of c-mi and clarify which method signature
            (if (empty? candidate-mdefs)
              {:error (str msg " : method not defined")}
              (if (= 1 (count candidate-mdefs))
                (let [arity (-> candidate-mdefs first :args count)]
                  {:error
                   (str msg " has " caller-arity caller-arg-str
                     ", but expects " arity " arg"
                     (if (= 1 arity) "" "s"))})
                {:error
                 (apply str msg " has " caller-arity caller-arg-str
                   ", which does not match any of the available arities: "
                   (interpose ", "
                     (map #(count (:args %)) candidate-mdefs)))})))))
      (get fields n0)
      {:type :field-ref :names vnames}
      (and method-args (some #(= % n0) method-args))
      {:type :method-arg-ref :names names}
      (some #(= % n0) pclass-args)
      {:type :pclass-arg-ref :names names}
      :else
      {:error
       (str "a state variable (undefined symbol) cannot be a method-fn: "
         names)})))

;; returns a vector of args
;; or an {:error msg}
(defn validate-method-fn-args [ir state-vars in-pclass
                               fields modes methods
                               context names args]
  (dbg-println :trace  "VALIDATE-METHOD-FN-ARGS" in-pclass
    "CONTEXT" context "CALLER ARITY" (count args) "ARGS" args)
  (let [pclass-args (get-in ir [in-pclass :args])
        [c-pclass c-methods c-method c-mi] context
        method (if (= c-methods :methods) c-method)
        method-args (if method
                      (get-in ir [in-pclass :methods method c-mi :args]))]
    (loop [vargs [] a (first args) more (rest args)]
      (if (or (nil? a) (:error vargs))
        vargs
        (let [va (cond
                   (literal? a)
                   ;; {:type :literal, :value a}
                   a

                   (mode-ref? a)
                   (validate-cond-operand ir state-vars in-pclass
                     fields modes context
                     pclass-args method-args a)

                   (symbol-ref? a)
                   (let [a-names (:names a)
                         [n0 & more] a-names
                         [n0 more vnames] (if (= 'this n0)
                                            [(first more) (rest more)
                                             (vec (rest a-names))]
                                            [n0 more a-names])]
                     (cond
                       (or (= n0 'this) (some #(= 'this %) more))
                       {:error (str "cannot use 'this except in the first position: " names)}
                       (get fields n0)
                       {:type :field-ref :names vnames}
                       (and method-args (some #(= % n0) method-args))
                       {:type :method-arg-ref :names a-names}
                       (some #(= % n0) pclass-args)
                       {:type :pclass-arg-ref :names a-names}
                       (not (empty? more))
                       {:error (str "cannot derefence a state variable (undefined symbol): " a)}
                       :else
                       (do
                         (swap! state-vars assoc n0 {:type :state-variable})
                         {:type :state-variable :name n0})))

                   (map? a)
                   a

                   :else
                   {:error (str "unknown argument type: " a)})
              vargs (if (:error va) va (conj vargs va))]
          (recur vargs (first more) (rest more)))))))

;; return Validated condition or {:error "message"}
(defn validate-condition [ir state-vars pclass fields modes context condition]
  (dbg-println :debug  "VALIDATE-CONDITION for" pclass
    "\nfields:" (keys fields)
    "\nmodes:" (keys modes)
    "\ncontext:" context
    "\ncondition:" condition)
  (let [{:keys [type args]} condition
        pclass-args (get-in ir [pclass :args])
        [c-pclass c-methods c-method c-mi] context
        method (if (= c-methods :methods) c-method)
        method-args (if method
                      (get-in ir [pclass :methods method c-mi :args]))
        rv
        (cond
          ;; (= type :literal)
          (clj19-boolean? condition)
          condition ;; literal boolean condition

          (or (nil? type) (symbol-ref? condition) (mode-ref? condition))
          (validate-cond-operand ir state-vars pclass fields modes context
                                 pclass-args method-args condition)

          (#{:equal :gt :ge :lt :le :same :function-call} type)
          (loop [vcond {:type type} vargs [] a (first args) more (rest args)]
            (if (nil? a)
              (assoc-if vcond :args vargs)
              (cond
                (or (keyword? a) (symbol-ref? a) (mode-ref? a))
                (let [va (validate-cond-operand ir state-vars pclass fields modes
                           context pclass-args method-args a)
                      vcond (if (:error va) va vcond)
                      vargs (if (:error va) nil (conj vargs va))]
                  (recur vcond vargs (first more) (rest more)))

                (map? a) ;; already specified by instaparse
                (recur vcond (conj vargs a) (first more) (rest more))

                :else ;; must be a literal
                ;; (recur vcond (conj vargs {:type :literal :value a})
                (recur vcond (conj vargs a)
                       (first more) (rest more)))))

          (#{:propositions-expr} type)
          (do
            (let [propositions (if (vector? (last args)) args (butlast args))
                  where (if (vector? (last args)) nil (last args))]
              (let [result
                    {:type :lookup-propositions
                     :where (if where (validate-condition ir state-vars pclass fields modes context where))
                     :propositions (into []
                                         (map (fn [pp]
                                                (let [search (if (search-constraints? (second pp)) (rest (second pp)) [])
                                                      proppart (if (search-constraints? (second pp)) (rest (rest pp)) (rest pp))]
                                                  {:type :proposition-pattern
                                                   :look-where (into [] (map (fn [sc]
                                                                               (case (first sc)
                                                                                 :wm {:type :wm}
                                                                                 :ltm {:type :ltm}
                                                                                 :recency {:type :recency :value (second sc)}

                                                                                 (log/error "Invalid search constraint: %s" (first sc))))
                                                                             search))
                                                   :prop-name (first proppart)
                                                   :args (into [] (map (fn [anarg]
                                                                         (validate-cond-operand ir state-vars pclass fields modes
                                                                                                context pclass-args method-args anarg))
                                                                       (rest proppart)))}))
                                              propositions))}]
                result)))

          (#{:and :or :not :implies} type)  ;; => recurse on args
          (let [vargs (mapv
                        (partial validate-condition ir state-vars
                          pclass fields modes (conj context type))
                        args)
                error (first (filter #(:error %) vargs))]
            (if error
              error
              {:type type
               :args vargs}))

          :else
          {:error (str "Unknown type (" type ") in condition")})]
    (dbg-println :debug  "  VALIDATE-CONDITION =>" (pr-str rv))
    rv))

;; return Validated body or {:error "message"}
(defn validate-body [ir state-vars in-pclass
                     fields modes methods in-method in-mi mbody & [context]]
  (loop [vmbody []
         bi 0
         b (if (vector? mbody) (first mbody) mbody)
         more (if (vector? mbody) (rest mbody))]
    (if (or (:error vmbody) (not b))
      vmbody
      (let [{:keys [type field name method-ref method args condition body]} b
            context (or context [in-pclass :methods in-method in-mi :body])
            context (conj context bi)
            _ (dbg-println :trace "VALIDATE-BODY CONTEXT" context)
            [b error] (cond
                        (= type :method-fn)
                        (let [vmethod-ref (validate-method-fn-method-ref
                                            ir state-vars in-pclass
                                            fields modes methods
                                            context method-ref args)
                              vargs (validate-method-fn-args
                                      ir state-vars in-pclass
                                      fields modes methods
                                      context vmethod-ref args)]
                          (if (:error vmethod-ref)
                            [nil vmethod-ref]
                            (if (:error vargs)
                              [nil vargs]
                              [(assoc b :method-ref vmethod-ref :args vargs)
                               nil])))
                        :else
                        [b nil])
            condition (if (and (not error) condition)
                        (validate-condition ir state-vars in-pclass fields
                          modes context condition))
            body (if (and (not error) (not (:error condition)) body)
                   (validate-body ir state-vars in-pclass fields
                     modes methods in-method in-mi body (conj context :body)))
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
        (recur vmbody (inc bi) (first more) (rest more))))))

;; validate just one argval in the context of a scope-pclass
;; while evaluating the initializer of field
(defn validate-argval [ir scope-pclass field fields argval]
  (dbg-println :trace "    VALIDATE-ARGVAL" argval scope-pclass field fields)
  (let [{:keys [type names]} (if (map? argval) argval)
        n0 (first names)
        rv (cond
             (and (= :symbol-ref type)
                  (some #(= n0 %) (get-in ir [scope-pclass :args])))
             (do
               (dbg-println :trace "    promote to :pclass-arg-ref")
               (assoc argval :type :pclass-arg-ref))

             (and (= :symbol-ref type)
                  (not= n0 field) (get fields n0))
             (do
               (dbg-println :trace "    promote to :field-ref")
               (assoc argval :type :field-ref))

             (= :symbol-ref type)
             {:error
              (str "Symbol argument to " field
                " pamela class constructor arg " n0
                " is neither a formal argument to, "
                "nor another field of the pclass " scope-pclass)}

             ;;+++ validate-arith-expr here +++

             :else
             argval)]
    (dbg-println :trace "    =>" rv)
    rv))

;; return Validated args or {:error "message"}
(defn validate-pclass-ctor-args [ir scope-pclass field fields pclass args]
  (dbg-println :trace "    VALIDATE-PCLASS-CTOR-ARGS for pclass:" pclass
               "args:" (pr-str args))
  (let [pclass-args (get-in ir [pclass :args])]

    (cond
      ;;Is the pclass defined??
      (not (= (get-in ir [pclass :type]) :pclass))
      {:error (str "Pclass constructor call to undefined undefined pclass: " pclass)}

      ;;Correct number of args?
      (not (= (count pclass-args) (count args)))
      {:error (str "Incorrect number of arguments passed to pclass constructor '" pclass "': " (count args)
                   " argument(s) when expecting " (count pclass-args)
                   " (i.e., " (pr-str args) " instead of " (pr-str pclass-args)")")}

      :else
      (loop [vargs [] a (first args) more (rest args)]
        (if (or (not a) (:error vargs))
          vargs
          (let [{:keys [type names]} (if (map? a) a)
                n0 (first names)
                ;; _ (dbg-println :trace "    pclass-ctor-arg for" field "=" a)
                a (validate-argval ir scope-pclass field fields a)
                vargs (if (and (map? a) (:error a))
                        a
                        (conj vargs a))]
            (recur vargs (first more) (rest more))))))))

;; return Validated fields or {:error "message"}
(defn validate-fields [ir state-vars pclass fields]
  (dbg-println :trace "VALIDATE-FIELDS" pclass)
  (let [field-vals (seq fields)]
    (loop [vfields {} field-val (first field-vals) more (rest field-vals)]
      (if (or (:error vfields) (not field-val))
        vfields
        (let [[field val] field-val
              {:keys [access observable initial]} val
              scope-pclass pclass
              {:keys [type pclass args names value
                      plant-id plant-interface plant-part]} initial
              n0 (first names)
              _ (dbg-println :trace "  FIELD" field "type" type "n0" n0)
              [type args] (cond
                            (= :pclass-ctor type)
                            [type (validate-pclass-ctor-args ir scope-pclass field
                                    fields pclass args)]
                            (and (= :symbol-ref type)
                              (some #(= n0 %) (get-in ir [scope-pclass :args])))
                            (do
                              (dbg-println :trace "  promote to :pclass-arg-ref")
                              [:pclass-arg-ref args])
                            (and (= :symbol-ref type)
                              (not= n0 field) (get fields n0))
                            (do
                              (dbg-println :trace "  promote to :field-ref")
                              [:field-ref args])
                            (= :symbol-ref type)
                            [type
                             {:error
                              (str "Symbol argument to " field
                                " field initializer " n0
                                " is neither a formal argument to, "
                                "nor another field of the pclass " scope-pclass)}]
                            :else
                            [type args])
              plant-id (if plant-id
                         (validate-argval ir scope-pclass field fields
                           plant-id))
              plant-interface (if plant-interface
                                (validate-argval ir scope-pclass field fields
                                  plant-interface))
              plant-part (if plant-part
                           (validate-argval ir scope-pclass field fields
                             plant-part))
              vfields (if (and (map? args) (:error args))
                        args
                        (assoc vfields field
                          (assoc val :initial
                            (assoc-if initial
                              :type type
                              :args args
                              :plant-id plant-id
                              :plant-interface plant-interface
                              :plant-part plant-part))))]
          (recur vfields (first more) (rest more)))))))

;; return Validated modes or {:error "message"}
(defn validate-modes [ir state-vars pclass fields modes]
  (let [mode-conds (seq modes)]
    (loop [vmodes {} mode-cond (first mode-conds) more (rest mode-conds)]
      (if (or (:error vmodes) (not mode-cond))
        vmodes
        (let [[mode cond] mode-cond
              cond (validate-condition ir state-vars pclass fields modes
                     [pclass :mode mode] cond)
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
                      [pclass :transition from-to :pre] pre))
              post (if post
                     (validate-condition ir state-vars pclass fields modes
                       [pclass :transition from-to :post] post))
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
  (dbg-println :trace "VALIDATE-METHODS" pclass)
  (let [method-mdefss (seq methods)]
    (loop [vmethods {}
           method-mdefs (first method-mdefss)
           more (rest method-mdefss)]
      (if (or (:error vmethods) (not method-mdefs))
        vmethods
        (let [[method mdefs] method-mdefs
              vmdefs
              (loop [vmdefs []
                     mi 0
                     mdef (first mdefs)
                     moar (rest mdefs)]
                (dbg-println :trace "  VALIDATE-METHOD" method
                  "MDEF" (dissoc mdef :body))
                (if (or (and (map? vmdefs) (:error vmdefs)) (not mdef))
                  vmdefs
                  (let [{:keys [pre post args body]} mdef
                        pre (if pre
                              (validate-condition ir state-vars pclass fields
                                modes [pclass :methods method mi :pre] pre))
                        post (if post
                               (validate-condition ir state-vars pclass fields
                                 modes [pclass :methods method mi :post] post))
                        body (if-not (empty? body)
                               (validate-body ir state-vars pclass
                                 fields modes methods method mi body))
                        ;; Yes, this is a hack to avoid a circular
                        ;; namespace dependency
                        unparser (the-ns 'pamela.unparser)
                        unparse-cond-expr (ns-resolve unparser 'unparse-cond-expr)
                        mdef (assoc-if mdef
                               :pre pre
                               :post post
                               :body body
                               :display-args
                               (mapv unparse-cond-expr args))
                        _ (dbg-println :trace "  VM ARGS" args
                            "DISPLAY-ARGS" (:display-args mdef))
                        vmdefs (cond
                                 (:error pre)
                                 pre
                                 (:error post)
                                 post
                                 (:error body)
                                 body
                                 :else
                                 (conj vmdefs mdef))]
                    (recur vmdefs (inc mi) (first moar) (rest moar)))))
              vmethods (if (and (map? vmdefs) (:error vmdefs))
                         vmdefs
                         (assoc vmethods method vmdefs))]
          (recur vmethods (first more) (rest more)))))))

;; PAMELA semantic checks
;; Hoist state variables, disambiguate conditional expression operands
;; return Validated PAMELA IR or {:error "message"}
(defn validate-pamela [ir]
  ;; (log/warn "VALIDATE-PAMELA")
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
                    :fields fields
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

(defn parse-pamela-file
  "Parses input-filename using instaparse
  Returns :error or the instaparse tree"
  [input-filename & [insta-parser-obj]]
  (if (fs/exists? input-filename)
    (let [tree (insta/parses (or insta-parser-obj (build-parser)) (slurp input-filename))]
      (cond (insta/failure? tree)
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
            :else                                           ; All is well
            (first tree)))
    (let [msg (str "parse: input file not found: "
                   input-filename)]
      (log/error msg)
      {:error msg})))

(defn get-dependent-inputs
  "Based on the :depends declarations, determines the list of input files that will provide
  those dependencies"
  [irs-for-direct-inputs]
  ;;TODO: Deal with version dependencies
  ;;TODO: Support a PATH for looking for dependency files.  Currently, just look in the same
  ;;  directory as the dependent
  ;;TODO: Support recursive dependencies
  (mapcat (fn [[f ir]]
            (mapcat (fn [[pclass pclass-ir]]
                      (keep (fn [[dep-name dep-version]]
                              (let [dependency-in-ir? (= (get-in ir [dep-name :type]) :pclass)
                                    dep-filename
                                    (fs/file (fs/parent f) (str dep-name ".pamela"))]
                                (if (and (not dependency-in-ir?)
                                         (not (fs/exists? dep-filename)))
                                  (log/warn "File for dependency " dep-name " does not exist"))
                                (if dependency-in-ir?
                                  nil
                                  (list dep-name dep-version pclass dep-filename))))
                            (get-in pclass-ir [:meta :depends])))
                    ir))
          irs-for-direct-inputs))

(defn transform-pamela-files
  "input is a list of pamela files
   parse and transform pamela files
   Return {input-filename transformed-ir}"
  [input parser]
  (let [transform-files
        (fn [input-files transformed]
          (reduce (fn [result input-filename]
                  #_(log/info "parse" input-filename)
                  (let [parsed-tree (parse-pamela-file input-filename parser)
                        parse-error? (contains? parsed-tree :error)]
                    (conj result {input-filename (if parse-error? parsed-tree
                                                     (insta/transform pamela-ir parsed-tree))})
                    ))
                  transformed input-files))
        irs-for-direct-inputs (transform-files input {})
        dependent-inputs (get-dependent-inputs irs-for-direct-inputs)
        irs-for-all-inputs (transform-files (map #(nth % 3) dependent-inputs)
                                                  irs-for-direct-inputs)]
    irs-for-all-inputs))

(defn do-magic-processing
  "Add lvars to transformed IR"
  [ir input output-magic]
  (let [lvars @pamela-lvars
        input-names (mapv fs-basename input)
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
      ir)))

;; Will parse the pamela input file(s)
;; and return {:error "message"} on errors
;; else the PAMELA IR
(defn parse [options]
  ;; (log/warn "PARSE" options)
  (let [{:keys [input magic output-magic]} options
        parser (build-parser)
        mir (if magic (parse-magic magic) {})]
    (when magic
      ;; (println "Magic" magic "MIR" mir)  ;; DEBUG
      (log/debug "MAGIC input\n" (with-out-str (pprint mir))))
    (reset! pamela-lvars mir)
    (let [transformed (transform-pamela-files input parser)
          fnil-conj (fnil conj [])
          ;; Collect errors if any.
          ir (reduce (fn [ir [_ tir]]
                       (if (contains? tir :error)
                         (update ir :error fnil-conj (:error tir))
                         (merge ir tir)))
                     {} transformed)
          final-ir (cond
                     (contains? ir :error)
                     {:error (:error ir)}                   ;if there are any errors, return only errors
                     :else
                     (validate-pamela (pamela.inheritance/flatten-inheritance ir))
                     )]
      (do-magic-processing final-ir input output-magic))))
