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
            [clojure.java.io :refer :all] ;; for as-file
            [pamela.utils :refer [make-url get-url]]
            [avenir.utils :refer [and-fn assoc-if vec-index-of concatv]]
            [instaparse.core :as insta])
  (:import [java.lang
            Long Double]))

(def zero-bounds {:type :bounds, :value [0 0]})

(def default-bounds {:type :bounds, :value [0 :infinity]})

(def zero-delay {:type :delay
                 :temporal-constraints [zero-bounds]
                 :body nil})

(def default-delay {:type :delay
                    :temporal-constraints [default-bounds]
                    :body nil})

(defn pamela-filename? [filename]
  (string/ends-with? filename ".pamela"))

(defn build-parser []
  (let [ebnf (slurp (resource "data/pamela.ebnf"))
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
       :value v}
      {:type :literal
       :value v})))

(defn ir-lvar-ctor [& [name]]
  {:type :lvar
   :name (or name :gensym)})

(defn ir-id [id]
  {:id id})

(defn ir-interface [interface]
  {:interface interface})

(defn ir-pclass-ctor [name & args-opts]
  (loop [args [] options {} a (first args-opts) more (rest args-opts)]
    (if-not a
      (merge
        {:type :pclass-ctor
         :name name
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
  (apply merge ms))

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
            :temporal-constraints [default-bounds]
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
                               [1 (assoc m :body a)]
                               [(inc args-seen?) (update-in m [:betweens]
                                                   conj a)]))]
        (recur m args-seen? (first more) (rest more))))))

(defn ir-bounds [lb ub]
  [lb (if (= ub [:INFINITY]) :infinity ub)])

(defn ir-opt-bounds [bounds]
  {:temporal-constraints
   [{:type :bounds
     :value bounds}]})

(defn ir-plant-fn [& args]
  (let [[pclass method used] (if (symbol? (first args))
                               [(first args) (second args) 2]
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
  (loop [fn-opts {:condition cond-expr } body [] a (first args) more (rest args)]
    (if-not a
      (merge {:type f :body (if (empty? body) nil body)} fn-opts)
      (if (and (vector? a) (#{:fn-opt :delay-opt} (first a)))
        (recur (merge fn-opts (second a)) body (first more) (rest more))
        (recur fn-opts (conj body a) (first more) (rest more))))))

(defn ir-choice [& args]
  (loop [choice-opts {} body [] a (first args) more (rest args)]
    (if-not a
      (merge {:type :choice :body (if (empty? body) nil body)} choice-opts)
      (if (and (vector? a) (= :choice-opt (first a)))
        (if (and (vector? (second a)) (= :guard (first (second a))))
          (recur (assoc choice-opts :condition (second (second a)))
            body (first more) (rest more))
          (recur (merge choice-opts (second a)) body (first more) (rest more)))
        (recur choice-opts (conj body a) (first more) (rest more))))))

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
                :bounds ir-bounds
                :choice ir-choice
                ;; :choice-opt handled by ir-choice
                :choose (partial ir-fn :choose)
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
                :equal-expr (partial ir-cond-expr :equal)
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
                :literal identity
                :lvar-ctor ir-lvar-ctor
                :maintain (partial ir-fn-cond :maintain)
                :meta (partial ir-k-merge :meta)
                :meta-entry identity
                :methods ir-methods
                :mode-enum ir-mode-enum
                :mode-expr ir-mode-expr
                :mode-init ir-mode-init
                :mode-map ir-merge
                :modes (partial ir-map-kv :modes)
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

(defn validate-condition [pclass fields modes condition]
  (log/info "VALIDATE-CONDITION for" pclass
    "\nfields:" (keys fields)
    "\nmodes:" (if modes (keys modes))
    "\ncondition:" condition)
  condition)

(defn validate-modes [pclass fields modes]
  (let [mode-conds (seq modes)]
    (loop [vmodes {} mode-cond (first mode-conds) more (rest mode-conds)]
      (if-not mode-cond
        vmodes
        (let [[mode cond] mode-cond
              cond (validate-condition pclass fields nil cond)
              vmodes (assoc vmodes mode cond)]
          (recur vmodes (first more) (rest more)))))))

;; Hoist state variables, disambiguate conditional expression operands

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
(defn validate-pamela [ir]
  (let [sym-vals (seq ir)]
    (loop [vir {} sym-val (first sym-vals) more (rest sym-vals)]
      (if-not sym-val
        vir
        (let [[sym val] sym-val
              {:keys [type args fields modes transitions methods]} val
              pclass? (= type :pclass)
              modes (if pclass? (validate-modes sym fields modes))
              val (if pclass? (assoc val :modes modes) val)
              vir (assoc vir sym val)]
          (recur vir (first more) (rest more)))))))

;; return PAMELA IR
(defn parse [options]
  (let [{:keys [cwd input]} options
        filename (if (= 1 (count input)) (first input))
        file (if file (as-file filename))
        file (if (and file (.exists file))
               file
               (as-file (str cwd "/" filename)))
        parser (build-parser)
        tree (if (.exists file) (insta/parses parser (slurp file)))]
    (cond
      (not= 1 (count input))
      (do
        (log/errorf "parse: expecting one input file: %s" input)
        false)
      (not (.exists file))
      (do
        (log/errorf "parse: input file not found: %s" filename)
        false)
      (insta/failure? tree)
      (do
        (log/errorf "parse: invalid input file: %s" filename)
        (log/errorf (with-out-str (pprint (insta/get-failure tree))))
        false)
      (not= 1 (count tree))
      (do
        (log/errorf "parse: grammar is ambiguous for input file: %s" filename)
        (log/errorf (with-out-str (pprint tree)))
        false)
      :else
      (validate-pamela (insta/transform pamela-ir (first tree))))))