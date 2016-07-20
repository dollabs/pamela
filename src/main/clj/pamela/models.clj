;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;; This is the namespace used to load PAMELA models

;;; Acknowledgement and Disclaimer:
;;; This material is based upon work supported by the Army Contracting
;;; and DARPA under contract No. W911NF-15-C-0005.
;;; Any opinions, findings and conclusions or recommendations expressed
;;; in this material are those of the author(s) and do necessarily reflect the
;;; views of the Army Contracting Command and DARPA.

(ns pamela.models
  "The pamela.models namespace is used to import all PAMELA models.
  The symbols defined in this namespace with {:pamela :models-helper}
  metadata are helper functions. All other symbols have been imported."
  (:refer-clojure :exclude [assert when sequence delay try catch]) ;; try catch
  (:require [clojure.java.io :refer :all] ;; for as-file
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [environ.core :refer [env]]
            [pamela.pclass :refer :all]
            [avenir.utils :refer [assoc-if concatv]]))

;; pamela functions

(defn new-pclass
  "Instanciate a predefined pclass."
  {:pamela :models-helper :added "0.2.0"}
  [pclass & args]
  (let [pclass (get-model pclass)]
    (apply pclass args)))

(defn get-opts-body
  "Return the optional bounds as a map and the body"
  {:pamela :models-helper :added "0.2.4"}
  [body]
  (loop [bounds nil body body]
    (cond
      (= :bounds (first body))
      (if (not (legal-bounds? (second body)))
        (throw (AssertionError. (str "bounds: not a vector\n")))
        (if (not (nil? bounds))
          (throw (AssertionError.
                   (str (first body) " may only be specified once\n")))
          (recur (second body) (nthrest body 2))))
      :else
      (let [opts (assoc-if {}
                   :bounds bounds)]
        [opts body]))))

(defn get-fn-opts-body
  "Return the fn-opt(s) as a map and the body (label allowed if not between?)"
  {:pamela :models-helper :added "0.2.4"}
  [label? delay? body]
  (loop [bounds nil label nil cost<= nil reward>= nil controllable delay?
         body body]
    (cond
      (= :label (first body))
      (if label?
        (if (not (keyword? (second body)))
          (throw (AssertionError. (str "label: not a keyword\n")))
          (if (not (nil? label))
            (throw (AssertionError.
                     (str (first body) " may only be specified once\n")))
            (recur bounds (second body) cost<= reward>= controllable (nthrest body 2))))
        (throw (AssertionError.
                 (str ":label not allowed here\n"))))
      (= :controllable (first body))
      (if delay?
        (if (not (boolean? (second body)))
          (throw (AssertionError. (str "controllable: value must be true or false\n")))
          (recur bounds label cost<= reward>= (second body) (nthrest body 2)))
        (throw (AssertionError.
                 (str ":controllable not allowed here\n"))))
      (= :bounds (first body))
      (if (not (legal-bounds? (second body)))
        (throw (AssertionError. (str "bounds: not a vector\n")))
        (if (not (nil? bounds))
          (throw (AssertionError.
                   (str (first body) " may only be specified once\n")))
          (recur (second body) label cost<= reward>= controllable (nthrest body 2))))
      (= :cost<= (first body))
      (if (not (number? (second body)))
        (throw (AssertionError. (str "cost<=: not a number\n")))
        (if (not (nil? cost<=))
          (throw (AssertionError.
                   (str (first body) " may only be specified once\n")))
          (recur bounds label (second body) reward>= controllable (nthrest body 2))))
      (= :reward>= (first body))
      (if (not (number? (second body)))
        (throw (AssertionError. (str "reward>=: not a number\n")))
        (if (not (nil? reward>=))
          (throw (AssertionError.
                   (str (first body) " may only be specified once\n")))
          (recur bounds label cost<= (second body) controllable (nthrest body 2))))
      :else
      ;; (let [fn-opts (-> {:bounds (or bounds default-bounds)}
      (let [fn-opts (assoc-if
                      (if delay?
                        {:controllable (or controllable false)}
                        {})
                      :bounds bounds
                      :label label
                      :cost<= cost<=
                      :reward>= reward>=)]
        [fn-opts body]))))

(defn get-choice-opts-body
  "Return the choice-opt(s) as a map and the body"
  {:pamela :models-helper :added "0.2.0"}
  [body]
  (loop [bounds nil label nil probability nil cost nil reward nil guard nil
         body body]
    (cond
      (= :bounds (first body))
      (if (not (legal-bounds? (second body)))
        (throw (AssertionError. (str "bounds: not a vector\n")))
        (if (not (nil? bounds))
          (throw (AssertionError.
                   (str (first body) " may only be specified once\n")))
          (recur (second body) label probability cost reward guard
            (nthrest body 2))))
      (= :label (first body))
      (if (not (keyword? (second body)))
        (throw (AssertionError. (str "label: not a keyword\n")))
        (if (not (nil? label))
          (throw (AssertionError.
                   (str (first body) " may only be specified once\n")))
          (recur bounds (second body) probability cost reward guard
            (nthrest body 2))))
      (= :probability (first body))
      (if (not (number? (second body))) ;; FIXME support number-ref
        (throw (AssertionError. (str "probability: not a number\n")))
        (if (not (nil? probability))
          (throw (AssertionError.
                   (str (first body) " may only be specified once\n")))
          (recur bounds label (second body) cost reward guard
            (nthrest body 2))))
      (= :cost (first body))
      (if (not (number? (second body)))
        (throw (AssertionError. (str "cost: not a number\n")))
        (if (not (nil? cost))
          (throw (AssertionError.
                   (str (first body) " may only be specified once\n")))
          (recur bounds label probability (second body) reward guard
            (nthrest body 2))))
      (= :reward (first body))
      (if (not (number? (second body)))
        (throw (AssertionError. (str "reward: not a number\n")))
        (if (not (nil? reward))
          (throw (AssertionError.
                   (str (first body) " may only be specified once\n")))
          (recur bounds label probability cost (second body) guard
            (nthrest body 2))))
      (= :guard (first body))
      (if (not (nil? guard))
        (throw (AssertionError.
                 (str (first body) " may only be specified once\n")))
        (recur bounds label probability cost reward (second body)
          (nthrest body 2)))
      :else
      (let [choice-opts (assoc-if {}
                          :bounds bounds ;; (or bounds default-bounds)
                          :label label
                          :probability probability
                          :cost cost
                          :reward reward
                          :guard guard)]
        [choice-opts body]))))

(defmacro delay
  "Delay operation function (planner may elect proper time delay)"
  {:pamela :models-helper :added "0.2.0"}
  [& body]
  (let [[fn-opts _] (get-fn-opts-body true true body)]
    (cons 'list
      (list (quote (symbol "delay")) fn-opts))))

(defmacro maintain
  "Try to achieve condition throughout the specified time bounds
   and maintain it for the duration of body."
  {:pamela :models-helper :added "0.2.0"}
  [condition & body]
  (let [[opts body] (get-opts-body body)
        safe-body (replace-pamela-calls body)]
    (cons 'list
      (cons (quote (symbol "maintain"))
        (cons condition
          (cons opts safe-body))))))

;; NOTE overrides clojure.core/assert in this namespace
(defmacro assert
  "Try to achieve condition throughout the specified time bounds."
  {:pamela :models-helper :added "0.2.0"}
  [condition & time]
  (let [time (if (nil? time) [] (first time))]
    (list 'list (quote (symbol "assert")) condition time)))

;; NOTE overrides clojure.core/when in this namespace
(defmacro when
  "If condition is true body will be performed once
  (i.e. if condition is true in this time window)."
  {:pamela :models-helper :added "0.2.0"}
  [condition & body]
  (let [[opts body] (get-opts-body body)
        safe-body (replace-pamela-calls body)]
    (cons 'list
      (cons (quote (symbol "when"))
        (cons condition
          (cons opts safe-body))))))

(defmacro unless
  "If condition is not true body will be performed."
  {:pamela :models-helper :added "0.2.0"}
  [condition & body]
  (let [[opts body] (get-opts-body body)
        safe-body (replace-pamela-calls body)]
    (cons 'list
      (cons (quote (symbol "unless"))
        (cons condition
          (cons opts safe-body))))))

(defmacro whenever
  "Every time that condition is true body will be performed.
  Limited to the time bounds (if provided)."
  {:pamela :models-helper :added "0.2.0"}
  [condition & body]
  (let [[opts body] (get-opts-body body)
        safe-body (replace-pamela-calls body)]
    (cons 'list
      (cons (quote (symbol "whenever"))
        (cons condition
          (cons opts safe-body))))))

;; NOTE overrides clojure.core/sequence in this namespace
(defmacro sequence
  "The forms that constitute body are performed in sequence
  within the specified time bounds. The form will exit with failure
  if the last item has not completed within the upper time bound."
  {:pamela :models-helper :added "0.2.0"}
  [& body]
  (let [[fn-opts body] (get-fn-opts-body true false  body)
        safe-body (replace-pamela-calls body)]
    (cons 'list
      (cons (quote (symbol "sequence"))
        (cons fn-opts safe-body)))))

(defmacro slack-sequence
  "The forms that constitute body are performed in sequence
  with flexible spacing in between each function.
  (slack-sequence fn-opt* fn0 fn1 ...)
  is equivalent to
  (sequence fn-opt* (delay) fn0  (delay) fn1 ...  (delay))"
  {:pamela :models-helper :added "0.2.6"}
  [& body]
  (let [[fn-opts body] (get-fn-opts-body true false  body)
        safe-body (replace-pamela-calls body)
        {:keys [label bounds cost<= reward>=]} fn-opts
        opts (if label [:label label] [])
        opts (if bounds (concatv opts [:bounds bounds]) opts)
        opts (if cost<= (concatv opts [:cost<= cost<=]) opts)
        opts (if reward>= (concatv opts [:reward>= reward>=]) opts)
        slack-body (cons '(delay) (conj (vec (interpose '(delay) safe-body)) '(delay)))]
    `(sequence ~@opts ~@slack-body)))

(defmacro soft-sequence
  "The forms that constitute body are optionally performed
  (soft-sequence fn-opt* fn0 fn1 ...)
  is equivalent to
  (sequence fn-opt* (optional fn0) (optional fn1) ...)"
  {:pamela :models-helper :added "0.2.6"}
  [& body]
  (let [[fn-opts body] (get-fn-opts-body true false  body)
        safe-body (replace-pamela-calls body)
        {:keys [label bounds cost<= reward>=]} fn-opts
        opts (if label [:label label] [])
        opts (if bounds (concatv opts [:bounds bounds]) opts)
        opts (if cost<= (concatv opts [:cost<= cost<=]) opts)
        opts (if reward>= (concatv opts [:reward>= reward>=]) opts)
        soft-body (map #(list 'optional %) safe-body)]
    `(sequence ~@opts ~@soft-body)))

(defmacro parallel
  "The forms that constitute body are performed in parallel
  within the specified time bounds. The form will exit with failure
  if the last item has not completed within the upper time bound."
  {:pamela :models-helper :added "0.2.0"}
  [& body]
  (let [[fn-opts body] (get-fn-opts-body true false body)
        safe-body (replace-pamela-calls body)]
    (cons 'list
      (cons (quote (symbol "parallel"))
        (cons fn-opts safe-body)))))

(defmacro slack-parallel
  "The forms that constitute body are performed in parallel
  with flexible spacing in between each function.
  (slack-parallel fn-opt* fn0 fn1 ...)
  is equivalent to
  (parallel fn-opt* (slack-sequence fn0) (slack-sequence fn1) ...)"
  {:pamela :models-helper :added "0.2.0"}
  [& body]
  (let [[fn-opts body] (get-fn-opts-body true false  body)
        safe-body (replace-pamela-calls body)
        {:keys [label bounds cost<= reward>=]} fn-opts
        opts (if label [:label label] [])
        opts (if bounds (concatv opts [:bounds bounds]) opts)
        opts (if cost<= (concatv opts [:cost<= cost<=]) opts)
        opts (if reward>= (concatv opts [:reward>= reward>=]) opts)
        slack-body (map #(list 'slack-sequence %) safe-body)]
    `(parallel ~@opts ~@slack-body)))

(defmacro soft-parallel
  "The forms that constitute body are optionally performed in parallel
  (soft-parallel fn-opt* fn0 fn1 ...)
  is equivalent to
  (parallel fn-opt* (optional fn0) (optional fn1) ...)"
  {:pamela :models-helper :added "0.2.0"}
  [& body]
  (let [[fn-opts body] (get-fn-opts-body true false  body)
        safe-body (replace-pamela-calls body)
        {:keys [label bounds cost<= reward>=]} fn-opts
        opts (if label [:label label] [])
        opts (if bounds (concatv opts [:bounds bounds]) opts)
        opts (if cost<= (concatv opts [:cost<= cost<=]) opts)
        opts (if reward>= (concatv opts [:reward>= reward>=]) opts)
        soft-body (map #(list 'optional %) safe-body)]
    `(parallel ~@opts ~@soft-body)))

(defmacro choose
  "One of the choices will be taken either to maximize reward, minimise
  cost, or randomly weighted by relative probability, or just randomly
  (equal chance for each choice) if no probability is provided.

  (choose
    (choice <body> {[<time bounds>]|} {:probability <n>|} {:cost <n>|} {:reward <n>|} {:guard (<condition>)|})
  …)

  Note that <n> can be a numeric constant or a logic variable."
  {:pamela :models-helper :added "0.2.0"}
  [& choices]
  (let [[fn-opts choices] (get-fn-opts-body true false choices)
        safe-choices (replace-pamela-calls choices)]
    (cons 'list
      (cons (quote (symbol "choose"))
        (cons fn-opts safe-choices)))))

(defmacro optional
  "Optionally execute the fn.
  (optional fn-opt* fn)
  is equivalent to
  (choose fn-opt* (choice (delay :bounds [0 0])) (choice fn))"
  {:pamela :models-helper :added "0.2.6"}
  [& fn]
  (let [[fn-opts fn] (get-fn-opts-body true false fn)
        safe-fn (replace-pamela-calls fn)
        {:keys [label bounds cost<= reward>=]} fn-opts
        opts (if label [:label label] [])
        opts (if bounds (concatv opts [:bounds bounds]) opts)
        opts (if cost<= (concatv opts [:cost<= cost<=]) opts)
        opts (if reward>= (concatv opts [:reward>= reward>=]) opts)]
    `(choose ~@opts (choice (delay :bounds [0 0])) (choice ~@safe-fn))))

(defn expect-number-or-lvar
  "Verify the value for param is a number or lvar. (helper function)"
  {:pamela :models-helper :added "0.2.0"}
  [param value]
  (if-not (or (number? value)
            (lvar? value)
            (and (list? value) (= (first value) 'lvar)))
    (throw (AssertionError.
             (str "choice parameter " param " expects a number or lvar: " value "\n")))))

(defn check-choice-params
  "Check validity of choice parameters (helper function)."
  {:pamela :models-helper :added "0.2.0"}
  [params]
  (if (pos? (count params))
    (if (odd? (count params))
      (throw (AssertionError.
               (str "expected an even number of choice parameters\n")))
      (doseq [i (range 0 (count params) 2)]
        (let [k (nth params i)
              v (nth params (inc i))]
          (case k
            :probability (expect-number-or-lvar k v)
            :cost (expect-number-or-lvar k v)
            :reward (expect-number-or-lvar k v)
            :guard true ;; assume v is a condition
            ;; default
            (throw (AssertionError.
                     (str "invalid choice parameter " k "\n")))))))))

(defmacro choice
  "Define a choice (see also choose)."
  {:pamela :models-helper :added "0.2.0"}
  [& body]
  (let [[choice-opts body] (get-choice-opts-body body)
        safe-body (replace-pamela-calls body)
        ;; {:keys [bounds label probability cost reward]} choice-opts
        ;; body (cons :reward (cons reward safe-body))
        ;; body (cons :cost (cons cost body))
        ;; body (cons :probability (cons probability body))
        ;; body (cons :label (cons label body))
        ;; body (cons :bounds (cons bounds body))
        body (cons choice-opts safe-body)]
    (cons 'list (cons (quote (symbol "choice")) body))))

(defmacro try-form
  "The form is performed, but if a problem is encountered,
   body is run. A problem here is if a constraint cannot be achieved.

  (try form [<time bounds>] (catch body))"
  {:pamela :models-helper :added "0.2.0"}
  ;; [& form-time-body]
  ;; (let [form (first form-time-body)
  ;;       tb (second form-time-body)
  ;;       time-bounds (if (vector? tb) tb [])
  ;;       body (if (vector? tb)
  ;;              (first (rest (rest form-time-body)))
  ;;              (first (rest form-time-body)))]
  ;;   (list 'try form time-bounds body)))
  [& opt-bounds-body]
  (let [bounds-kw (first opt-bounds-body)
        bounds (second opt-bounds-body)
        opt-bounds? (and (= bounds-kw :bounds) (legal-bounds? bounds))
        bounds (if opt-bounds?  bounds default-bounds)
        body (if opt-bounds? (nthrest opt-bounds-body 2) opt-bounds-body)
        safe-body (replace-pamela-calls (replace-try-catch body))]
    ;; (apply vector :try :bounds bounds safe-body)
    (cons 'try-form
      (cons {:bounds bounds} safe-body))
    ))

(defmacro catch-form
  "Failure handling block for try."
  {:pamela :models-helper :added "0.2.0"}
  [& body]
  ;; (apply vector :catch body))
  (let [safe-body (replace-pamela-calls (replace-try-catch body))]
    ;; (apply vector :try :bounds bounds safe-body)
    (cons 'catch-form safe-body)
    ))

(defmacro special [& args]
  (println "SPECIAL" args)
  :fred)

(defmacro between
  "This constrains that the time between the form named by
  label1 and label2 is <time bounds>. ie: sequential actions
  with gap between."
  {:pamela :models-helper :added "0.2.0"}
  [label1 label2 & opts]
  (when-not (keyword? label1)
    (throw (AssertionError.
             (str "label must be a keyword: " label1 "\n"))))
  (when-not (keyword? label2)
    (throw (AssertionError.
             (str "label must be a keyword: " label2 "\n"))))
  (let [[fn-opts _] (get-fn-opts-body false false opts)]
    (list 'list
      (quote (symbol "between"))
      label1 label2 fn-opts)))

(defmacro between-starts
  "Form named by <label2> begins <time bounds> after form named by <label1>."
  {:pamela :models-helper :added "0.2.0"}
  [label1 label2 & opts]
  (when-not (keyword? label1)
    (throw (AssertionError.
             (str "label must be a keyword: " label1 "\n"))))
  (when-not (keyword? label2)
    (throw (AssertionError.
             (str "label must be a keyword: " label2 "\n"))))
  (let [[fn-opts _] (get-fn-opts-body false false opts)]
    (list 'list
      (quote (symbol "between-starts"))
      label1 label2 fn-opts)))

(defmacro between-ends
  "Form named by <label2> ends <time bounds> after form named by <label1>."
  {:pamela :models-helper :added "0.2.0"}
  [label1 label2 & opts]
  (when-not (keyword? label1)
    (throw (AssertionError.
             (str "label must be a keyword: " label1 "\n"))))
  (when-not (keyword? label2)
    (throw (AssertionError.
             (str "label must be a keyword: " label2 "\n"))))
  (let [[fn-opts _] (get-fn-opts-body false false opts)]
    (list 'list
      (quote (symbol "between-ends"))
      label1 label2 fn-opts)))

(defmacro ask
  "Wait for condition during time bounds, else fail
  (for thread synchronization)."
  {:pamela :models-helper :added "0.2.0"}
  [condition & [time-bounds]]
  (let [time-bounds (or time-bounds default-bounds)]
    (list 'list (quote (symbol "ask")) condition time-bounds)))

(defmacro tell
  "Assert condition (for thread synchronization)."
  {:pamela :models-helper :added "0.2.0"}
  [condition]
  (list 'list (quote (symbol "tell")) condition))

;; model helpers -----------------------------------------------------

(defn load-pamela-string
  "Load a pamela model from a string(in pamela.models ns)"
  {:pamela :models-helper :added "0.2.0"}
  [source & [url]]
  (binding [*ns* (the-ns 'pamela.models)
            *url* (str (or url "file:///tmp/string.pamela"))]
    (with-out-str
      (load-string source))))

(defn load-pamela
  "Load a pamela file from a pathname (in pamela.models ns)"
  {:pamela :models-helper :added "0.2.0"}
  [path]
  (let [file (as-file path)]
    (if (.exists file)
      (binding [*ns* (the-ns 'pamela.models)
                *url* (str (as-url file))]
        (load-file (.getCanonicalPath file)))
      (throw (AssertionError.
               (str "load-pamela: file does not exist: " path "\n"))))))

(defn load-pamela-project
  "Load a pamela file from a pathname relative to the project"
  {:pamela :models-helper :added "0.2.0"}
  [project-path]
  (let [relative-path (if (.startsWith project-path "/")
                        project-path
                        (str "/" project-path))
        path (str (:user-dir env) relative-path)]
    (load-pamela path)))
