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

(ns pamela.unparser
  "The unparser for the PAMELA intermediate representation"
  (:require ;; [clojure.set :as set]
   [clojure.string :as string]
   [clojure.pprint :as pp :refer [pprint]]
   [clojure.tools.logging :as log]
   [avenir.utils :refer [assoc-if concatv]]
   [pamela.parser :refer [true-type default-bounds-type
                          default-bounds default-mdef literal?]]
   [pamela.utils :refer [dbg-println]]))

(defn dissoc-default
  "Dissoc k from m if the current value is equal to default"
  [m k default]
  (let [v (get m k)]
    (if (= v default)
      (dissoc m k)
      m)))

(defn pprint-option [option val]
  (let [ppval (with-out-str (pprint val))]
    (str "\n  " option " "
      (string/replace
        (subs ppval 0 (dec (count ppval)))
        "\n"
        (apply str "\n" (repeat (+ (count (str option)) 3) " "))))))

(defn unparse-number-ref [number-ref]
  (let [{:keys [type name default]} number-ref]
    (if (= type :lvar)
      (let [lvar '()
            lvar (if default (cons default lvar) lvar)
            lvar (if name (cons name lvar) lvar)]
        (cons 'lvar lvar))
      number-ref)))

(defn cond-operand? [v]
  (or (literal? v) ;; literal
    ;; (#{:literal :field-ref :mode-ref :state-variable
    (#{:field-ref :mode-ref :state-variable
       :method-arg-ref :pclass-arg-ref}
      (:type v))))

(defn proposition-operand? [v]
  (and (map? v) (= (:type v) :lookup-propositions)))

(defn remove-first-this [names]
  (if (= (first names) 'this)
    (vec (rest names))
    names))

(defn unparse-symbol-ref [symbol-ref]
  (let [{:keys [type names]} symbol-ref
        symbol-ref-src (->> names
                         remove-first-this
                         (map str)
                         (interpose ".")
                         (apply str)
                         symbol)]
    symbol-ref-src))

;; unparse-field-type -------------------------

(defn unparse-field-type-dispatch [field-type]
  (dbg-println :trace "    UFT DISPATCH" field-type)
  (:type field-type))

(defmulti unparse-field-type
  #'unparse-field-type-dispatch
  :default :default)

(defmethod unparse-field-type :default [field-type]
  (unparse-number-ref field-type))

;; (defmethod unparse-field-type :literal [field-type]
;;   (:value field-type))

(defmethod unparse-field-type :mode-ref [field-type]
  (let [{:keys [mode-ref mode]} field-type
        {:keys [names]} mode-ref]
    (if (= names ['this])
      mode
      (list 'mode-of (unparse-symbol-ref mode-ref) mode))))

(defmethod unparse-field-type :symbol-ref [field-type]
  (unparse-symbol-ref field-type))

(defmethod unparse-field-type :pclass-arg-ref [field-type]
  (let [_ (dbg-println :trace "  UFT PAR before" field-type)
        rv (unparse-symbol-ref field-type)]
    (dbg-println :trace "  UFT PAR after" rv)
    rv))

(defmethod unparse-field-type :method-arg-ref [field-type]
  (unparse-symbol-ref field-type))

(declare unparse-argvals)
(declare unparse-cond-expr)

(defmethod unparse-field-type :pclass-ctor [field-type]
  (let [{:keys [pclass args plant-id plant-interface plant-part]} field-type
        pclass-ctor '()
        pclass-ctor (if plant-part
                      (cons :plant-part
                        (cons
                          (unparse-cond-expr plant-part)
                          pclass-ctor))
                      pclass-ctor)
        pclass-ctor (if plant-interface
                      (cons :interface
                        (cons
                          (unparse-cond-expr plant-interface)
                          pclass-ctor))
                      pclass-ctor)
        pclass-ctor (if plant-id
                      (cons :id
                        (cons
                          (unparse-cond-expr plant-id)
                          pclass-ctor))
                      pclass-ctor)
        pclass-ctor (if (empty? args)
                      (cons pclass pclass-ctor)
                      (concat (list pclass)
                        (unparse-argvals args)
                        pclass-ctor))]
    pclass-ctor))

(defmethod unparse-field-type :lvar [field-type]
  (let [{:keys [name default]} field-type
        lvar-ctor (if default (list default) '())
        lvar-ctor (if (and name (not= name :gensym))
                    (cons name lvar-ctor)
                    lvar-ctor)]
    (cons 'lvar lvar-ctor)))

(def arith-op {:add '+,
               :subtract '-,
               :multiply '*,
               :divide '/})

(declare compile-tc-value)

(defn unparse-expr
  [exprn]
  (let [{type :type
         op :operation
         args :args} exprn
        cargs (into () (reverse (map compile-tc-value args)))]
    (cons (get arith-op op) cargs)))

(defn compile-tc-value
  [value]
  (if (map? value)
    (case (:type value)
      :lvar (unparse-field-type value)
      :expr (unparse-expr value)
      value)
    value))

;; unparse-temporal-constraints -------------------------

(defn unparse-temporal-constraints [temporal-constraints]
  (let [[tc0] temporal-constraints ;; consider only first one
        {:keys [type value]} tc0]
    (when (and (vector? temporal-constraints)
            (> (count temporal-constraints) 1))
      (log/error "Unable to unparse more than one temporal constraint"
                 temporal-constraints))
    (if (= type :bounds)
       (cond (vector? value)
             (into [] (map compile-tc-value value))
             (map? value)
             (compile-tc-value value)
             (nil? value)
             value
             :otherwise
             (do (log/warn "Suspicious bounds in unparse-temporal-constraints" temporal-constraints)
                 value)))))

;; unparse-cond-operand -------------------------

(defn unparse-cond-operand-dispatch [cond-operand]
  (if (literal? cond-operand)
    :default
    (:type cond-operand)))

(defmulti unparse-cond-operand
  #'unparse-cond-operand-dispatch
  :default :default)

(defmethod unparse-cond-operand :default [cond-operand]
  cond-operand)

;; (defmethod unparse-cond-operand :literal [cond-operand]
;;   (:value cond-operand))

(defmethod unparse-cond-operand :state-variable [cond-operand]
  (let [{:keys [name]} cond-operand]
    name))

(defmethod unparse-cond-operand :mode-ref [cond-operand]
  (let [{:keys [mode-ref mode]} cond-operand
        {:keys [names]} mode-ref]
    (if (= names ['this])
      mode
      (list 'mode-of (unparse-symbol-ref mode-ref) mode))))

(defmethod unparse-cond-operand :field-ref [cond-operand]
  (unparse-symbol-ref cond-operand))

(defmethod unparse-cond-operand :method-arg-ref [cond-operand]
  (unparse-symbol-ref cond-operand))

(defmethod unparse-cond-operand :pclass-arg-ref [cond-operand]
  (unparse-symbol-ref cond-operand))

;; unparse-propositions-expr --------------------

(defn unparse-look-where [lws]
  (let [result (into [] (map (fn [lw]
                               (case (:type lw)
                                 :wm 'wm
                                 :ltm 'ltm
                                 :recency (list 'recency (:value lw))
                                 (do
                                   (log/error "Unable to unparse search constraint:" (:type lw) " raw IR inserted in place")
                                   lw)))
                             lws))]
    result))

(declare unparse-cond-expr)

(defn unparse-proposition-operand [prop-expr]
  (let [{typ :type
         where :where
         propositions :propositions} prop-expr
        lookwhere (map (fn [p] (if (not (empty? (:look-where p))) (unparse-look-where (:look-where p)))) propositions)
        propname  (map (fn [p] (:prop-name p)) propositions)
        args      (map (fn [p] (into [] (map unparse-cond-operand (:args p)))) propositions)]
    (concat '(propositions)
            (list (into [] (map (fn [lw pn args]
                                  (if lw
                                    (cons lw  (cons pn args))
                                    (cons pn args)))
                                lookwhere propname args)))
            (if where (list 'where (unparse-cond-expr where)) ()))))

;; unparse-cond-expr -------------------------

(defn unparse-cond-expr [cond-expr]
  (cond
    (nil? cond-expr)
    true-type ;; default for unspecified conditions

    (or (symbol? cond-expr) (literal? cond-expr))
    cond-expr

    (proposition-operand? cond-expr)
    (unparse-proposition-operand cond-expr)

    (cond-operand? cond-expr)
    (unparse-cond-operand cond-expr)
    ;; NOTE this would only provide the first field name, not the
    ;; whole dereferencing chain
    ;; (= :pclass-ctor (:type cond-expr)) ;; helper for display-argument
    ;; (-> cond-expr :ancestry first last)

    (map? cond-expr)
    (let [{:keys [type args]} cond-expr]
      (apply list
             (get {:and 'and
                   :equal '=
                   :gt '>
                   :ge '>=
                   :lt '<
                   :le '<=
                   :same 'same
                   :implies 'implies
                   :not 'not
                   :or 'or
                   :function-call 'call}
                  type)
        (map unparse-cond-expr args)))
    :else ;; default
    true-type))

;; -------------------------

(defn unparse-argvals [args]
  (mapv unparse-cond-expr args))
  ;; (loop [argvals nil a (first args) more (rest args)]
  ;;   (if-not a
  ;;     (if-not (empty? argvals)
  ;;       (vec (reverse argvals)))
  ;;     (let [{:keys [type mode-ref mode name param]} (if (map? a) a)
  ;;           {:keys [names]} mode-ref
  ;;           argval (cond
  ;;                    (= type :mode-ref)
  ;;                    (if (= names ['this])
  ;;                      mode
  ;;                      (list 'mode-of (unparse-symbol-ref mode-ref) mode))
  ;;                    (#{:symbol-ref :field-ref :method-arg-ref :pclass-arg-ref}
  ;;                      type)
  ;;                    (unparse-symbol-ref a)
  ;;                    (= type :state-variable)
  ;;                    name
  ;;                    (= type :pclass-ctor)
  ;;                    param
  ;;                    :else
  ;;                    a)
  ;;           argvals (cons argval argvals)]
  ;;       (recur argvals (first more) (rest more))))))

;; unparse-option -------------------------

(defn unparse-option-dispatch [option def]
  (or option :inherit))

(defmulti unparse-option
  #'unparse-option-dispatch)

(defmethod unparse-option :meta [option def]
  (pprint-option option
    (let [meta-kvs (seq def)]
      (loop [meta-source {} meta-kv (first meta-kvs) more (rest meta-kvs)]
          (if-not meta-kv
            meta-source
            (let [[k v] meta-kv
                  v-src (if (= k :depends)
                          (vec (seq v))
                          v)
                  meta-source (assoc meta-source k v-src)]
              (recur meta-source (first more) (rest more))))))))

(defmethod unparse-option :inherit [option def]
  (pprint-option option def))

;; ;; if the value of all the modes is {:type :literal, :value true}
;; if the value of all the modes is true?
;; then convert to a mode-enum
;; else if value is true then use that
;; else unparse-cond-expr
(defmethod unparse-option :modes [option def]
  (pprint-option option
    (let [modes-kvs (seq def)]
      (if (every? true? (map second modes-kvs))
        (mapv first modes-kvs) ;; mode-enum
        (loop [modes-source {} mode-kv (first modes-kvs) more (rest modes-kvs)]
          (if-not mode-kv
            modes-source
            (let [[mode v] mode-kv
                  mode-init-source (unparse-cond-expr v)
                  modes-source (assoc modes-source mode mode-init-source)]
              (recur modes-source (first more) (rest more)))))))))

(defmethod unparse-option :fields [option def]
  (let [field-keys (keys def)]
    (loop [fields def field (first field-keys) more (rest field-keys)]
      (if-not field
        (pprint-option option fields)
        (let [field-init (get def field)
              {:keys [access observable initial]} field-init
              _ (dbg-println :trace "INITIAL before" initial)
              initial-src (unparse-field-type initial)
              _ (dbg-println :trace "INITIAL-SRC" initial-src)
              field-type-src (if (and (= access :private) (false? observable))
                               initial-src
                               (->
                                 {:initial initial-src
                                  :access access
                                  :observable observable}
                                 (dissoc-default :access :private)
                                 (dissoc-default :observable false)))
               fields (assoc fields field field-type-src)]
          (recur fields (first more) (rest more)))))))

(defn unparse-trans-map [trans-map]
    (let [{:keys [doc pre post cost reward probability
                  temporal-constraints]} trans-map
          pre-src (if pre (unparse-cond-expr pre))
          post-src (if post (unparse-cond-expr post))
          probability-src (if probability (unparse-number-ref probability))
          cost-src (if cost (unparse-number-ref cost))
          reward-src (if reward (unparse-number-ref reward))
          bounds (unparse-temporal-constraints temporal-constraints)
          trans-map-source (assoc-if (dissoc trans-map :temporal-constraints)
                             :pre pre-src
                             :post post-src
                             :bounds bounds
                             :cost cost-src
                             :reward reward-src
                             :probability probability-src)]
      trans-map-source))

(defmethod unparse-option :transitions [option def]
  (let [transition-keys (keys def)]
    (loop [transitions def
           from-to (first transition-keys)
           more (rest transition-keys)]
      (if-not from-to
        (pprint-option option transitions)
        (let [trans-map (get def from-to)
              trans-map-source (unparse-trans-map trans-map)
              transitions (assoc transitions from-to trans-map-source)]
          (recur transitions (first more) (rest more)))))))

(defn unparse-fn [fn]
  (let [{:keys [type body probability name method-ref args
                condition field temporal-constraints
                cost reward cost<= reward>=
                exactly min max controllable
                catch label enter leave]} fn
        bounds (unparse-temporal-constraints temporal-constraints)]
    (cond
      (#{:choose :choose-whenever :delay :parallel :sequence :tell :catch} type)
      (let [body-src (if-not (empty? body) (map unparse-fn body))
            body-src (if (nil? controllable)
                       body-src
                       (cons :controllable (cons controllable body-src)))
            body-src (if max
                          (cons :max (cons max body-src))
                          body-src)
            body-src (if min
                          (cons :min (cons min body-src))
                          body-src)
            body-src (if exactly
                          (cons :exactly (cons exactly body-src))
                          body-src)
            body-src (if reward>=
                       (cons :reward>=
                         (cons (unparse-number-ref reward>=) body-src))
                       body-src)
            body-src (if cost<=
                       (cons :cost<=
                         (cons (unparse-number-ref cost<=) body-src))
                       body-src)
            body-src (if condition
                       (cons (unparse-cond-expr condition) body-src)
                       body-src)
            body-src (if bounds
                         (cons :bounds (cons bounds body-src))
                         body-src)
            body-src (if label
                         (cons :label (cons label body-src))
                         body-src)]
        (cons (symbol (clojure.core/name type)) body-src))
      (#{:ask :assert :maintain :unless :when :whenever} type)
      (let [body-src (if-not (empty? body) (map unparse-fn body))
            body-src (if bounds
                         (cons :bounds (cons bounds body-src))
                         body-src)
            body-src (if condition
                       (cons (unparse-cond-expr condition) body-src)
                       body-src)]
        (cons (symbol (clojure.core/name type)) body-src))
      (= type :try)
      (let [catch-src (if-not (empty? catch)
                        (list (cons 'catch (map unparse-fn catch))))
            body-src (if (empty? body)
                       catch-src
                       (concat (map unparse-fn body) catch-src))
            body-src (if bounds
                         (cons :bounds (cons bounds body-src))
                         body-src)]
        (cons (symbol (clojure.core/name type)) body-src))
      (= type :choice)
      (let [choice-src (list (unparse-fn (first body)))
            choice-src (if leave
                         (cons :leave
                           (cons (unparse-fn leave) choice-src))
                          choice-src)
            choice-src (if enter
                         (cons :enter
                           (cons (unparse-fn enter) choice-src))
                          choice-src)
            choice-src (if reward
                         (cons :reward
                           (cons (unparse-number-ref reward) choice-src))
                          choice-src)
            choice-src (if cost
                         (cons :cost
                           (cons (unparse-number-ref cost)choice-src))
                          choice-src)
            choice-src (if bounds
                         (cons :bounds (cons bounds choice-src))
                         choice-src)
            choice-src (if probability
                         (cons :probability
                           (cons (unparse-number-ref probability) choice-src))
                         choice-src)
            choice-src (if condition
                         (cons :guard
                           (cons (unparse-cond-expr condition) choice-src))
                         choice-src)
            choice-src (if label
                         (cons :label (cons label choice-src))
                         choice-src)]
        (cons 'choice choice-src))
      (= type :method-fn)
      (let [args-src (unparse-argvals args)
            args-src (if (nil? controllable)
                       args-src
                       (cons :controllable (cons controllable args-src)))
            args-src (if reward
                        (cons :reward
                          (cons (unparse-number-ref reward) args-src))
                        args-src)
            args-src (if cost
                        (cons :cost
                          (cons (unparse-number-ref cost) args-src))
                        args-src)
            args-src (if bounds
                         (cons :bounds (cons bounds args-src))
                         args-src)
            args-src (if label
                         (cons :label (cons label args-src))
                         args-src)
            method-ref-src (unparse-symbol-ref method-ref)]
        (cons method-ref-src args-src))
      :else
      (do
        (log/error "Unable to unparse fn type:" type " raw IR inserted in place")
        fn)))) ;; default case for new fn that are not yet handled

(defn unparse-between-stmt [between-stmt]
  (let [{:keys [type from to temporal-constraints
                cost<= reward>=]} between-stmt
        bounds (unparse-temporal-constraints temporal-constraints)
        between-src (if reward>=
                      (list :reward>= (unparse-number-ref reward>=)))
        between-src (if cost<=
                      (cons :cost<=
                        (cons (unparse-number-ref cost<=) between-src))
                      between-src)
        between-src (if bounds
                         (cons :bounds (cons bounds between-src))
                         between-src)]
    (cons (symbol (clojure.core/name type))
      (cons from
        (cons to between-src)))))

(defn unparse-defpmethod [method mdef]
  (let [{:keys [doc pre post cost reward controllable primitive display-name
                temporal-constraints ;; bounds in cond-map
                args betweens body]} mdef
        cond-map (dissoc mdef :args :betweens :body :display-args)
        primitive-default (nil? body)
        cond-map-default (assoc (dissoc (default-mdef method) :betweens :body)
                           :primitive primitive-default)
        display-name-default (:display-name cond-map-default)
        probability-default 1.0
        cond-map-src (->
                       (if (= cond-map cond-map-default)
                         {}
                         (assoc
                           (dissoc cond-map :temporal-constraints)
                           :bounds (unparse-temporal-constraints
                                     temporal-constraints)
                           :pre (unparse-cond-expr pre)
                           :post (unparse-cond-expr post)))
                       (dissoc-default :bounds default-bounds)
                       (dissoc-default :pre true)
                       (dissoc-default :post true)
                       (dissoc-default :cost 0)
                       (dissoc-default :reward 0)
                       (dissoc-default :controllable false)
                       (dissoc-default :primitive primitive-default)
                       (dissoc-default :bounds default-bounds)
                       (dissoc-default :display-name display-name-default)
                       (dissoc-default :probability probability-default))
        src (if-not (empty? betweens) (map unparse-between-stmt betweens))
        src (if (empty? body) src (cons (unparse-fn (first body)) src))
        src (cons args src)
        src (if-not (empty? cond-map-src) (cons cond-map-src src) src)
        src (cons method src)
        src (cons 'defpmethod src)]
    src))

(defmethod unparse-option :methods [option def]
  (pprint-option option
    (let [method-mdefss (seq def)]
      (loop [methods-source []
             method-mdefs (first method-mdefss)
             more (rest method-mdefss)]
        (if-not method-mdefs
          methods-source
          (let [[method mdefs] method-mdefs
                mdefs-source (mapv (partial unparse-defpmethod method) mdefs)
                methods-source (concatv methods-source mdefs-source)]
            (recur methods-source (first more) (rest more))))))))

;; unparse-pclass -------------------------

(defn unparse-pclass [pclass pdef]
  (let [{:keys [meta inherit fields modes transitions methods]} pdef]
    (str
      (if meta (unparse-option :meta meta))
      (if inherit (unparse-option :inherit inherit))
      (if fields (unparse-option :fields fields))
      (if modes (unparse-option :modes modes))
      (if transitions (unparse-option :transitions transitions))
      (if methods (unparse-option :methods methods)))))

;; unparse -------------------------

;; converts from ir back to pamela source
(defn unparse [ir]
  (apply str
    (interpose "\n\n"
      (cons
        ";; PAMELA source generated from IR"
        (for [pclass (keys ir)
              :let [pdef (get ir pclass)
                    {:keys [type args]} pdef
                    pclass-str (if (= type :pclass)
                                 (str "(defpclass " pclass " " args
                                   (unparse-pclass pclass pdef)
                                   ")"))]
              :when pclass-str]
          pclass-str)))))
