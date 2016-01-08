;; Copyright © 2016 Dynamic Object Language Labs Inc.tr
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

;; This is the namespace used to load PAMELA models

(ns pamela.models
  "The pamela.models namespace is used to import all PAMELA models.
  The symbols defined in this namespace with {:pamela :models-helper}
  metadata are helper functions. All other symbols have been imported."
  (:refer-clojure :exclude [assert when sequence delay]) ;; try catch
  (:require [clojure.java.io :refer :all] ;; for as-file
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [environ.core :refer [env]]
            [pamela.pclass :refer :all]))

(defn new-pclass
  "Instanciate a predefined pclass."
  {:pamela :models-helper :added "0.2.0"}
  [pclass & args]
  (let [pclass (get-model pclass)]
    (apply pclass args)))

(defn helper?
  "Is x a helper function?"
  {:pamela :models-helper :added "0.2.0"}
  [x]
  (if-let [h (get-helper-var x)]
    (:pamela (meta h))))

(defn get-bounds-body
  "Return the bounds and the body (where bounds are optional)"
  {:pamela :models-helper :added "0.2.0"}
  [body]
  (let [a (first body)
        b (second body)
        bounds (if (= a :bounds) b (if (vector? a) a []))
        body (if (= a :bounds) (nthrest body 2) (if (vector? a) (rest body) body))]
    [bounds body]))

(defmacro condition
  "Define a condition.

  Conditions must
  - if keyword then converted to (mode= keyword)
  - return boolean type
  - comprised of and, or, not, implies
  - either (field= field-kw constant) or (mode= mode-kw)
  - must not have cycles

  Conditions may appear in
  - mode definitions
  - transition :pre :post conditions
  - method :pre :post conditions"
  {:pamela :models-helper :added "0.2.0" :doc/format :markdown}
  [& body]
  (if (empty? body)
    (throw (AssertionError.
             (str "condition: empty condition\n")))
    (if (not= 1 (count body))
      (throw (AssertionError.
               (str "condition: expecting only one condition\n")))
      `(quote ~@body))))

(defmacro noop
  "No operation function (used for testing)"
  {:pamela :models-helper :added "0.2.0"}
  []
  (list 'list (quote (symbol "noop"))))

(defmacro maintain
  "Try to achieve condition throughout the specified time bounds
   and maintain it for the duration of body."
  {:pamela :models-helper :added "0.2.0"}
  [condition & body]
  (let [[bounds body] (get-bounds-body body)
        safe-body (replace-pamela-args (replace-pamela-calls body))]
    (cons 'list (cons (quote (symbol "maintain")) (cons condition (cons :bounds (cons bounds safe-body)))))))

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
  (let [[bounds body] (get-bounds-body body)
        safe-body (replace-pamela-args (replace-pamela-calls body))]
    (cons 'list (cons (quote (symbol "when")) (cons condition (cons :bounds (cons bounds safe-body)))))))

(defmacro unless
  "If condition is not true body will be performed."
  {:pamela :models-helper :added "0.2.0"}
  [condition & body]
  (let [[bounds body] (get-bounds-body body)
        safe-body (replace-pamela-args (replace-pamela-calls body))]
    (cons 'list (cons (quote (symbol "unless")) (cons condition (cons :bounds (cons bounds safe-body)))))))

(defmacro whenever
  "Every time that condition is true body will be performed.
  Limited to the time bounds (if provided)."
  {:pamela :models-helper :added "0.2.0"}
  [condition & body]
  (let [[bounds body] (get-bounds-body body)
        safe-body (replace-pamela-args (replace-pamela-calls body))]
    (cons 'list (cons (quote (symbol "whenever")) (cons condition (cons :bounds (cons bounds safe-body)))))))

;; NOTE overrides clojure.core/sequence in this namespace
(defmacro sequence
  "The forms that constitute body are performed in sequence
  within the specified time bounds. The form will exit with failure
  if the last item has not completed within the upper time bound."
  {:pamela :models-helper :added "0.2.0"}
  [& body]
  (if (empty? body)
    (cons 'list (list (quote (symbol "sequence")) :bounds []))
    (let [[bounds body] (get-bounds-body body)
          safe-body (replace-pamela-args (replace-pamela-calls body))]
      (cons 'list (cons (quote (symbol "sequence")) (cons :bounds (cons bounds safe-body)))))))

(defmacro parallel
  "The forms that constitute body are performed in parallel
  within the specified time bounds. The form will exit with failure
  if the last item has not completed within the upper time bound."
  {:pamela :models-helper :added "0.2.0"}
  [& body]
  (if (empty? body)
    (cons 'list (list (quote (symbol "parallel")) :bounds []))
    (let [[bounds body] (get-bounds-body body)
          safe-body (replace-pamela-args (replace-pamela-calls body))]
      (cons 'list (cons (quote (symbol "parallel")) (cons :bounds (cons bounds safe-body)))))))

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
  (cons 'list (cons (quote (symbol "choose")) choices)))

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
  (let [[bounds body] (get-bounds-body body)
        safe-body (replace-pamela-args (replace-pamela-calls body))]
    (cons 'list (cons (quote (symbol "choice")) (cons :bounds (cons bounds safe-body))))))
  ;; (let [safe-body (walk-exprs method-call? pamela-call body)]
  ;;   (if (empty? params)
  ;;     `(list (symbol "choice") ~safe-body)
  ;;     (let [time-bounds (first params)]
  ;;       (if (vector? time-bounds)
  ;;         (check-choice-params (rest params))
  ;;         (check-choice-params params))
  ;;       `(list (symbol "choice") ~safe-body ~@params)))))

;; NOTE due to a clojure limitation we CANNOT redfine try
(defn try-form
  "The form is performed, but if a problem is encountered,
   body is run. A problem here is if a constraint cannot be achieved.

  (try form [<time bounds>] (catch body))"
  {:pamela :models-helper :added "0.2.0"}
  [& form-time-body]
  (let [form (first form-time-body)
        tb (second form-time-body)
        time-bounds (if (vector? tb) tb [])
        body (if (vector? tb)
               (first (rest (rest form-time-body)))
               (first (rest form-time-body)))]
    (list 'try-form form time-bounds body)))

;; NOTE due to a clojure limitation we CANNOT redfine catch as macro,
;; but we can as a function
(defn catch
  "Failure handling block for try-form."
  {:pamela :models-helper :added "0.2.0"}
  [& body]
  (cons 'catch body))

(defn delay
  "Delay for n time ticks."
  {:pamela :models-helper :added "0.2.0"}
  [n]
  (list 'delay n))

(defmacro between
  "This constrains that the time between the form named by
  label1 and label2 is <time bounds>. ie: sequential actions
  with gap between."
  {:pamela :models-helper :added "0.2.0"}
  [label1 label2 & [time-bounds]]
  (when-not (keyword? label1)
    (throw (AssertionError.
             (str "label must be a keyword: " label1 "\n"))))
  (when-not (keyword? label2)
    (throw (AssertionError.
             (str "label must be a keyword: " label2 "\n"))))
  (let [time-bounds (or time-bounds [])]
    (list 'list (quote (symbol "between")) label1 label2 time-bounds)))

(defmacro between-starts
  "Form named by <label2> begins <time bounds> after form named by <label1>."
  {:pamela :models-helper :added "0.2.0"}
  [label1 label2 & [time-bounds]]
  (when-not (keyword? label1)
    (throw (AssertionError.
             (str "label must be a keyword: " label1 "\n"))))
  (when-not (keyword? label2)
    (throw (AssertionError.
             (str "label must be a keyword: " label2 "\n"))))
  (let [time-bounds (or time-bounds [])]
    (list 'list (quote (symbol "between-starts")) label1 label2 time-bounds)))

(defmacro between-ends
  "Form named by <label2> ends <time bounds> after form named by <label1>."
  {:pamela :models-helper :added "0.2.0"}
  [label1 label2 & [time-bounds]]
  (when-not (keyword? label1)
    (throw (AssertionError.
             (str "label must be a keyword: " label1 "\n"))))
  (when-not (keyword? label2)
    (throw (AssertionError.
             (str "label must be a keyword: " label2 "\n"))))
  (let [time-bounds (or time-bounds [])]
    (list 'list (quote (symbol "between-ends")) label1 label2 time-bounds)))

(defmacro ask
  "Wait for condition during time bounds, else fail
  (for thread synchronization)."
  {:pamela :models-helper :added "0.2.0"}
  [condition & [time-bounds]]
  (let [time-bounds (or time-bounds [])]
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
