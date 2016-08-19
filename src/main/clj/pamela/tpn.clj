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

(ns pamela.tpn
  "TPN functions."
  (:require [clojure.java.io :refer :all] ;; for as-file
            [clojure.data :refer [diff]]
            [clojure.java.shell :refer [sh]]
            [clojure.set :as set]
            [clojure.string :as string]
            [clojure.pprint :as pp]
            [clojure.walk :refer [prewalk postwalk]]
            [pamela.daemon :as daemon]
            [pamela.tpnrecords :as tpns]
            ;; [pamela.pclass :refer :all]
            ;; [pamela.models :refer [load-pamela-project load-pamela-string]]
            [avenir.utils :refer [concatv assoc-if keywordize]]
            [pamela.web :as web]
            [clojure.tools.logging :as log]
            [environ.core :refer [env]]
            [clojure.data.json :as json]
            [manifold.deferred :as d]
            [manifold.stream :as s]
            [aleph.tcp :as tcp]))

;; -----------------------------------------------------------------------

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

;; DEBUG
(defn map-has-nil-key? [m]
  (pos?
    (count
      (remove nil?
        (for [[k v] (seq m)]
          (do
            (println "MHNK" k "v" v)
            (if (nil? k) true)))))))

(defn argmethod?
  "Returns true if symbol is a method on an argument (i.e. contains .)"
  {:added "0.2.0"}
  [symbol]
  (and (symbol? symbol)
    (pos? (.indexOf (name symbol) "."))))

(defn dotmethod?
  "Returns true if symbol is a method on an argument (i.e. contains .)"
  {:added "0.2.0"}
  [symbol]
  (and (symbol? symbol)
    (pos? (.indexOf (name symbol) "."))))

(defn plant-function?
  "Returns true if the form is a plant function"
  {:added "0.2.0"}
  [form]
  (and (list? form) (argmethod? (first form))))

(defn safe-nth
  "Verion of nth which will simply return nil if n exceeds collection"
  {:added "0.2.0"}
  [form n]
  (if (> (count form) n)
    (nth form n)
    nil))

(defn default-bounds?
  "Return true of bounds are [0 :infinity]"
  {:added "0.2.0"}
  [bounds]
  (and (vector? bounds)
    (or (zero? (count bounds))
      (and
        (= 2 (count bounds))
        (= 0 (first bounds))
        (= :infinity (second bounds))))))

(defn replace-plant-functions
  "Replace plant functions"
  {:added "0.2.0"}
  [tpn tpn-args]
  (fn [form]
    ;; (println "RPF form" (type form) "=" (pr-str form)) ;; DEBUG
    (if-not (list-or-cons? form)
      form
      (let [f (first form)]
        (if-not (argmethod? f)
          form
          (let [arg-method (first form)
                delay? (= 'delay arg-method)
                [arg method-name] (string/split (name arg-method) #"\.")
                opts (second form)
                argvals (nthrest form 2)
                label (:label opts)
                f-bounds (:bounds opts)
                f-cost (:cost opts)
                f-reward (:reward opts)
                f-controllable (:controllable opts)
                f' (if delay?
                     'delay
                     (symbol (str arg "." method-name)))
                method-sym (if-not delay? (symbol method-name))
                ;; _ (println "GET PLANT " (type arg) " = "
                ;;     arg "." method-name)  ;; DEBUG
                plant (if (or (= arg "this") delay?)
                        tpn
                        (get tpn-args (keyword arg)))
                _ (if (nil? plant)
                    (throw (AssertionError.
                             (str "cannot find plant for arg: " arg))))
                {:keys [pclass id]} plant
                method (if delay?
                         {:doc "built in delay function"
                          :bounds [0 :infinity]
                          :controllable true}
                         (get-in plant [:methods method-sym]))
                {:keys [doc pre post bounds cost reward controllable
                        args body betweens]} method
                doc (or doc "") ;; doc (or doc method-name)
                [flb fub] f-bounds
                [lb ub] bounds
                lb (or lb 0)
                ub (or ub :infinity)
                flb (if flb
                      (if (>= flb lb) flb
                          lb) ;; (str "invalid lower bound")
                      lb)
                fub (if fub
                      (if (= ub :infinity)
                        fub
                        (if (= fub :infinity) ;; constrain by ub
                          ub
                          (if (<= fub ub) fub
                              ub))) ;; (str "invalid upper bound")
                      ub)
                bounds [flb fub]
                bounds (if (default-bounds? bounds) nil bounds)
                cost (or f-cost cost)
                reward (or f-reward reward)
                controllable (if (boolean? f-controllable)
                               f-controllable
                               (or controllable false))
                av (mapv hash-map (map keyword args) argvals)
                body (if body ;; plant-fn body is NOT primitive
                       (do
                         ;; (println "SUB prewalk")  ;; DEBUG
                         (prewalk (replace-plant-functions plant tpn-args) body)
                         ))
                ;; _ (println "PREP plant-fn")  ;; DEBUG
                plant-fn (list f'
                           (assoc-if {:pclass pclass
                                      :method method-name
                                      :args av
                                      :controllable controllable}
                             :id id
                             :doc doc
                             :bounds bounds
                             :label label
                             :cost cost
                             :reward reward
                             :non-primitive body))]
            ;; (println "DEBUG plant-fn:")
            ;; (pp/pprint plant-fn)
            plant-fn))))))

(defn constraint-from-bounds
  "Returns a new constraint if the bounds are non-default bounds"
  {:added "0.2.0"}
  [bounds & [end-node]]
  (let [constraint (if (and bounds (not (default-bounds? bounds)))
                     (tpns/make-temporal-constraint {}
                       :value bounds :end-node end-node))
        constraint (if end-node constraint
                       (dissoc constraint :end-node))]
    constraint))

(defn union-items
  "Unions all items into a set (ignores nil)"
  {:added "0.2.0"}
  ([]
   #{})
  ([item]
   (if item
     (if (set? item) item #{item})
     #{}))
  ([item & more]
   (set/union
     (union-items item)
     (apply union-items more))))

(defn build-sequence
  "sequence helper for build-graph"
  {:added "0.2.0"}
  [form]
  (let [{:keys [bounds label cost<= reward>=]} (second form)
        activities (nthrest form 2)]
    (loop [first-act nil prev nil activity (first activities)
           more (rest activities) objects {}]
      (if (empty? more)
        (if (nil? activity)
          form
          ;; this is the last activty
          (let [first-act (or first-act activity)
                prev-act (get-in prev [:graph :act])
                prev-se (get-in prev [:graph :se])
                sb (get-in activity [:graph :sb])
                act (get-in activity [:graph :act])
                se (get-in activity [:graph :se])
                objs (get-in activity [:graph :objects])
                ;; _ (println "OBJECTS sequence 111") ;; DEBUG
                ;; _ (pp/pprint objs)
                prev-se-obj (if prev-se (get objects prev-se))
                sb-obj (get objs sb)
                na (if (and prev-se (not (and prev-act act))) ;; not simple
                     (tpns/make-null-activity {} :end-node sb)) ;; make na!
                objs (if na
                       (assoc objs
                         (:uid na) na ;; add na
                         sb (assoc sb-obj :incidence-set;; update sb
                              (union-items (:incidence-set sb-obj) (:uid na))))
                       (if prev-se
                         (dissoc objs sb) ;; remove sb
                         objs))
                ;; _ (println "OBJECTS sequence 222") ;; DEBUG
                ;; _ (pp/pprint objs)
                new-activities (if na
                                 (union-items
                                   (:activities prev-se-obj)
                                   (:uid na))
                                 (union-items
                                   (:activities prev-se-obj)
                                   (:activities sb-obj)))
                new-constraints (if na (:constraints prev-se-obj)
                                    (union-items
                                      (:constraints prev-se-obj)
                                      (:constraints sb-obj)))
                prev-se-obj (when prev-se
                              (-> prev-se-obj
                                (assoc :activities new-activities)
                                (assoc-if :constraints new-constraints)))
                objs (assoc-if objs prev-se prev-se-obj)
                ;; _ (println "OBJECTS sequence 333") ;; DEBUG
                ;; _ (pp/pprint objs)
                objects (merge objects objs)
                first-act-sb (get-in first-act [:graph :sb])
                first-act-sb-obj (get objects first-act-sb)
                constraint (constraint-from-bounds bounds se)
                new-constraints (if constraint
                                  (union-items
                                    (:constraints first-act-sb-obj)
                                    (:uid constraint))
                                  (:constraints first-act-sb-obj))
                first-act-sb-obj (assoc-if first-act-sb-obj
                                   :constraints new-constraints
                                   :cost<= cost<=
                                   :reward>= reward>=
                                   :sequence-label label
                                   :sequence-end se)
                objects (-> objects
                          (assoc first-act-sb first-act-sb-obj)
                          (assoc-if (:uid constraint) constraint))
                ;; _ (println "OBJECTS sequence") ;; DEBUG
                ;; _ (pp/pprint objects)
                ;; _ (if (map-has-nil-key? objects)
                ;;     (println "MHNK sequence (1)" objects))
                graph (assoc-if {:sb first-act-sb :se se :objects objects}
                        :label label)
                new-form {:graph graph :function (first form)
                          ;; :activities activities
                          }]
            ;; (println "BUILD-SEQUENCE new-form" new-form)
            new-form))
        ;; there are more activities
        (let [first-act (or first-act activity)
              prev-act (get-in prev [:graph :act])
              prev-se (get-in prev [:graph :se])
              sb (get-in activity [:graph :sb])
              act (get-in activity [:graph :act])
              se (get-in activity [:graph :se])
              objs (get-in activity [:graph :objects])
              prev-se-obj (if prev-se (get objects prev-se))
              sb-obj (get objs sb)
              na (if (and prev-se (not (and prev-act act))) ;; not simple
                   (tpns/make-null-activity {} :end-node sb)) ;; make na!
              ;; _ (println "ACTIVITY" activity)
              ;; _ (println "MORE" more)
              ;; _ (println "NA" na "SB" sb "PREV-SE" prev-se)
              objs (if na
                     (assoc objs
                       (:uid na) na ;; add na
                       sb (assoc sb-obj :incidence-set;; update sb
                            (union-items (:incidence-set sb-obj) (:uid na))))
                     (if prev-se
                       (dissoc objs sb) ;; remove sb
                       objs))
              ;; _ (if (map-has-nil-key? objs)
              ;;     (println "MHNK sequence (1.6)" objs))
              new-activities (if na
                               (union-items
                                 (:activities prev-se-obj)
                                 (:uid na))
                               (union-items
                                 (:activities prev-se-obj)
                                 (:activities sb-obj)))
              new-constraints (if na (:constraints prev-se-obj)
                                  (union-items
                                    (:constraints prev-se-obj)
                                    (:constraints sb-obj)))
              prev-se-obj (when prev-se
                            (-> prev-se-obj
                              (assoc :activities new-activities)
                              (assoc-if :constraints new-constraints)))
              objs (if prev-se ;; if we need to update prev-se
                     (assoc objs prev-se prev-se-obj)
                     objs)
              ;; _ (if (map-has-nil-key? objs)
              ;;     (println "MHNK sequence (1.8)" objs))
              objects (merge objects objs)
              ;; _ (println "OBJECTS sequence MORE") ;; DEBUG
              ;; _ (pp/pprint objects)
              ;; _ (if (map-has-nil-key? objects)
              ;;     (println "MHNK sequence (2)" objects))
              ]
          (recur first-act activity (first more) (rest more) objects))))))

(defn build-choice
  "choice helper for build-graph"
  {:added "0.2.0"}
  [form]
  (let [{:keys [bounds label cost reward probability guard]} (second form)
        first-act (first (nthrest form 2))
        {:keys [sb act se objects]} (:graph first-act)
        ;; _ (if (map-has-nil-key? objects)
        ;;     (println "MHNK choice (1)" objects))
        ;; _ (if (nil? sb)
        ;;     (println "MHNK choice (2)" sb))
        ;; NOTE there is no act for a sequence!
        ;; _ (if (nil? act)
        ;;     (println "MHNK choice (3)" form))
        ;; NO, put choice-opts (except bounds) on null-activity
        ;; act-obj (when act
        ;;           (-> (get objects act)
        ;;             (assoc-if :probability probability)
        ;;             (assoc-if :cost cost)
        ;;             (assoc-if :reward reward)))
        act-obj (if act (get objects act))
        ;; _ (println "BUILD-CHOICE ACT-OBJ" act-obj)
        first-act-sb-obj (get objects sb)
        first-act-se-obj (get objects se)
        constraint (constraint-from-bounds bounds se)
        new-constraints (union-items
                          (:constraints first-act-sb-obj)
                          (:uid constraint))
        first-act-sb-obj (assoc first-act-sb-obj
                           :constraints new-constraints)
        objects (-> objects
                  (assoc-if sb first-act-sb-obj)
                  (assoc-if act act-obj)
                  (assoc-if (:uid constraint) constraint))
        ;; _ (if (map-has-nil-key? objects)
        ;;     (println "MHNK choice (0)" objects))
        graph (assoc-if {:sb sb :se se :objects objects}
                :label label
                :cost cost
                :reward reward
                :probability probability
                :guard (if guard (str guard))) ;; TBD
        new-form {:graph graph :function (first form)}]
    ;; (println "CHOICE ---------------------------") ;; DEBUG
    ;; (pp/pprint new-form) ;; DEBUG
    new-form))

(defn make-null-activities
  "Wire up null activities"
  {:added "0.2.0"}
  [objs ob oe options &[choice?]]
  (let [order (atom -1)
        ob-id (:uid ob)
        oe-id (:uid oe)
        begin-ids
        (for [option options
              :let [g (:graph option)
                    {:keys [sb se objects]} g
                    sb-obj (get objects sb)
                    se-obj (get objects se)
                    choice-opts (if choice?
                                  (dissoc g :sb :se :objects)
                                  {})
                    choice-opts (assoc choice-opts :order (swap! order inc))
                    begin-na (tpns/make-null-activity choice-opts :end-node sb)
                    end-na (tpns/make-null-activity {} :end-node oe-id)
                    ob (get @objs ob-id)
                    oe (get @objs oe-id)
                    [ob sb-obj] (tpns/wire-activity ob begin-na sb-obj)
                    [se-obj oe] (tpns/wire-activity se-obj end-na oe)
                    updates {ob-id ob
                             (:uid begin-na) begin-na
                             sb sb-obj
                             se se-obj
                             (:uid end-na) end-na
                             oe-id oe}]]
          (do
            (swap! objs merge objects updates)
            (:uid begin-na)))]
    (set begin-ids)))

(defn build-choose
  "choose helper for build-graph"
  {:added "0.2.0"}
  [form]
  (let [{:keys [bounds label cost<= reward>=]} (second form)
        choices (nthrest form 2)
        ce (tpns/make-c-end {})
        constraint (constraint-from-bounds bounds (:uid ce))
        cb (tpns/make-c-begin
             (assoc-if {}
               :label label
               :cost<= cost<=
               :reward>= reward>=)
             :constraints (union-items (:uid constraint)))
        ;; each se of the choice must now have a null activity pointing to ce
        objects (atom (assoc-if
                        {(:uid cb) cb (:uid ce) ce}
                        (:uid constraint) constraint))
        begin-ids (make-null-activities objects cb ce choices true)
        cb (get @objects (:uid cb))
        ce (get @objects (:uid ce))
        cb (assoc cb
             :end-node (:uid ce)
             :activities begin-ids)
        _ (swap! objects assoc (:uid cb) cb (:uid ce) ce)
        ;; _ (if (map-has-nil-key? @objects)
        ;;     (println "MHNK choose" @objects))
        graph {:sb (:uid cb) :se (:uid ce) :objects @objects}
        new-form {:graph graph :function (first form)}]
    ;; (println "BUILD-CHOOSE ---------------------") ;; debug
    ;; (pp/pprint new-form) ;; DEBUG
    new-form))

(defn build-parallel
  "parallel helper for build-graph"
  {:added "0.2.0"}
  [form]
  (let [{:keys [bounds label cost<= reward>=]} (second form)
        options (nthrest form 2)
        pe (tpns/make-p-end {})
        constraint (constraint-from-bounds bounds (:uid pe))
        pb (tpns/make-p-begin
             (assoc-if {}
               :label label
               :cost<= cost<=
               :reward>= reward>=)
             :constraints (union-items (:uid constraint)))
        ;; each se of the parallel must now have a null activity pointing to pe
        objects (atom (assoc-if
                        {(:uid pb) pb (:uid pe) pe}
                        (:uid constraint) constraint))
        begin-ids (make-null-activities objects pb pe options)
        pb (get @objects (:uid pb))
        pe (get @objects (:uid pe))
        pb (assoc pb
             :end-node (:uid pe)
             :activities begin-ids)
        _ (swap! objects assoc (:uid pb) pb (:uid pe) pe)
        ;; _ (println "OBJECTS for parallel") ;; DEBUG
        ;; _ (pp/pprint @objects)
        ;; _ (if (map-has-nil-key? @objects)
        ;;     (println "MHNK parallel" @objects))
        graph {:sb (:uid pb) :se (:uid pe) :objects @objects}
        new-form {:graph graph :function (first form)
                  ;; :options options
                  }]
    ;; (println "BUILD-PARALLEL new-form" new-form)
    new-form))

;; {:network-id :net-00000
;;  :net-00000 :fake-network}
(defn build-non-primitive
  "Replace non-primitive body with a sub-TPN"
  {:added "0.2.0"}
  [body]
  (if (nil? body)
    false
    (let [ ;; _ (println "BNP body:") ;; DEBUG
          ;; _ (pp/pprint body)
          {:keys [sb objects]} (:graph body)
          network (tpns/make-network {} :begin-node sb)
          non-primitive (assoc objects
                          :network-id (:uid network)
                          (:uid network) network)]
      ;; (if (map-has-nil-key? non-primitive)
      ;;   (println "MHNK build-non-primitive" non-primitive))
      non-primitive)))

(defn build-graph
  "Convert form to a graph"
  {:added "0.2.0"}
  [form]
  (if (list-or-cons? form)
    ;; (if (argmethod? (first form))
    (if (or (dotmethod? (first form)) (= 'delay (first form)))
      (let [delay? (= 'delay (first form))
            {:keys [pclass id method doc
                    bounds label cost reward controllable
                    args non-primitive]} (second form)
            se (tpns/make-state {})
            constraint (constraint-from-bounds bounds (:uid se))
            ;; remove doc -- it's too verbose!
            ;; label (str "(" pclass ":" id ":" method ") " doc)
            ;; label (str "(" pclass ":" id ":" method
            ;;         (if-not (empty? args) " ")
            ;;         (if-not (empty? args) (string/join " " (map str args)))
            ;;         ")")
            edge-label (if delay? "delay" method)
            net-objects (build-non-primitive non-primitive)
            network-id (:network-id net-objects)
            non-primitive (or network-id false)
            act-details (if delay?
                          {:controllable (or controllable false)}
                          {:plant (str pclass)
                           :plantid (or id "")
                           ;; :command method
                           :command (if (pos? (count args))
                                      (apply str method " " args)
                                      method)
                           :controllable (or controllable false)
                           :non-primitive non-primitive})
            act-details (assoc-if act-details
                          :label label
                          :cost cost
                          :reward reward)
            act (tpns/make-activity act-details
                  ;; :uid uid
                  :name edge-label
                  :constraints (union-items (:uid constraint))
                  :end-node (:uid se))
            act (if delay? (assoc act :tpn-type :delay-activity) act)
            ;; _ (println "DEBUG act: " (type act))
            ;; _ (pp/pprint act)
            se (assoc se :incidence-set #{(:uid act)})
            ;; _ (println "se: " (type se) "=" se) ;; DEBUG
            sb (tpns/make-state {} :activities #{(:uid act)})
            ;; _ (println "sb: " (type sb) "=" sb) ;; DEBUG
            objects [sb act se]
            objects (if constraint (conj objects constraint) objects)
            objects (zipmap (map :uid objects) objects) ;; convert to map
            ;; _ (println "OBJECTS for " label)  ;; DEBUG
            ;; _ (pp/pprint objects)
            objects (if network-id
                      (merge (dissoc net-objects :network-id) objects)
                      objects)
            ;; _ (if (map-has-nil-key? objects) (println "MHNK activity" act))
            graph {:sb (:uid sb) :act (:uid act) :se (:uid se)
                   :objects objects}
            new-form {:graph graph :function (first form)}]
        new-form)
      (case (keyword (first form))
        :sequence (build-sequence form)
        :choice (build-choice form)
        :choose (build-choose form)
        :parallel (build-parallel form)
        ;; default - not handled yet
        form)
      )
    ;; not list
    form))

(defn find-begin-for-activity [objects uid]
  (let [object-vals (vals objects)]
    (loop [object (first object-vals) more (rest object-vals)]
      (let [activities (or (:activities object) #{})]
        (if object
          (if (activities uid)
            (:uid object)
            (recur (first more) (rest more)))
          nil))))) ;; NOT FOUND

;; :between ;; from-end -> to-begin
;; :between-starts ;; from-begin -> to-begin
;; :between-ends ;; from-end -> to-end
(defn add-between-constraints [objects betweens]
  (if (empty? betweens)
    objects
    (let [labels (atom {})
          objs (atom objects)]
      (doseq [[uid object] (seq objects)]
        (let [{:keys [label end-node sequence-label sequence-end]} object]
          (when label
            (swap! labels
              assoc label [uid end-node]))
          (when sequence-label
            (swap! labels
              assoc sequence-label [uid sequence-end]))))
      (doseq [between betweens]
        (let [[between-type from to opts] between
              {:keys [bounds cost<= reward>=]} opts
              ;; _ (println "DEBUG ABC" between-type "from" from "to" to
              ;;     "bounds" bounds "cost<=" cost<= "reward>=" reward>=)
              [from-uid from-end] (get @labels from)
              from-obj (get @objs from-uid)
              from-begin (if (= (:tpn-type from-obj) :activity)
                           (find-begin-for-activity @objs from-uid)
                           from-uid)
              [to-uid to-end] (get @labels to)
              to-obj (get @objs to-uid)
              to-begin (if (= (:tpn-type to-obj) :activity)
                         (find-begin-for-activity @objs to-uid)
                         to-uid)
              object (get @objs
                       (if (= between-type :between-starts)
                         from-begin from-end))
              temporal-constraint (constraint-from-bounds bounds
                                    (if (= between-type :between-ends)
                                      to-end to-begin))
              temporal-constraint (if temporal-constraint
                                    (assoc temporal-constraint
                                      between-type [from to]))
              ;; _ (println "DEBUG ABC tc" temporal-constraint)
              cost<=-constraint (if cost<=
                                  (tpns/make-cost<=-constraint
                                    {between-type [from to]}
                                    :value cost<=
                                    :end-node (if (= between-type
                                                    :between-ends)
                                                to-end to-begin)))
              ;; _ (println "DEBUG ABC c-le" cost<=-constraint)
              reward>=-constraint (if reward>=
                                    (tpns/make-reward>=-constraint
                                      {between-type [from to]}
                                      :value reward>=
                                      :end-node (if (= between-type
                                                      :between-ends)
                                                  to-end to-begin)))
              ;; _ (println "DEBUG ABC r-ge" reward>=-constraint)
              object (assoc object
                       :constraints
                       (union-items
                         (:constraints object)
                         (:uid temporal-constraint)
                         (:uid cost<=-constraint)
                         (:uid reward>=-constraint)))]
          ;; (println "DEBUG ABC constraints" object)
          (swap! objs
            (fn [o]
              (assoc-if o
                (:uid object) object
                (:uid temporal-constraint) temporal-constraint
                (:uid cost<=-constraint) cost<=-constraint
                (:uid reward>=-constraint) reward>=-constraint)
              ))))
      @objs)))

(defn create-tpn
  "Create TPN as a Clojure map given a pclass instance,
  the field representing the TPN, and the method in the TPN to use."
  {:added "0.2.0"}
  [pclass field method-sym]
  ;; FIXME
  ;; (if-not (pclass-instance? pclass)
  ;;   (throw (AssertionError. "requires a pclass instance")))
  (tpns/reset-tpnsym!)
  (let [tpn (get-in pclass [:fields field :initial])
        _ (if (nil? tpn)
            (throw (AssertionError. (str "field " field " is not defined in pclass"))))
        method (get-in tpn [:methods method-sym])
        _ (if (nil? method)
            (throw (AssertionError. (str "method " method-sym " is not defined in tpn"))))
        ;; NOTE: tpn-args keys MUST match the formal parameters of the tpn-class
        ;; FIXME field-args (get-in pclass [:fieldargs field])
        ;; field-vals (map #(get-in pclass [:fields % :initial]) field-args)
        field-vals (map #(get-in pclass [:fields % :initial])
                     (keys (:fields pclass)))
        tpn-params (map keyword (:args tpn))
        tpn-args (apply merge (map hash-map tpn-params field-vals))
        {:keys [args bounds pre post doc betweens body]} method
        ;; _ (println "BODY before------------------------------------")
        ;; _ (pp/pprint body)
        ;; _ (do (println "tpn-args:") (pp/pprint tpn-args)) ;; DEBUG
        ;; _ (println "betweens:" betweens)
        body (prewalk (replace-plant-functions tpn tpn-args) body)
        ;; _ (println "BODY AFTER------------------------------------")
        ;; _ (pp/pprint body)
        tpn-map (postwalk build-graph body)
        ;; _ (println "TPN-MAP ------------------------------------")
        ;; _ (pp/pprint tpn-map)
        {:keys [sb se objects]} (:graph tpn-map)
        network (assoc (tpns/make-network {} :begin-node sb)
                  :end-node se)
        objects (add-between-constraints objects betweens)]
    (assoc objects
      :network-id (:uid network)
      (:uid network) network)))

;; nc in clj ---------------------------------------------------

(defn wrap-duplex-stream
  "Connect s to an output stream"
  {:added "0.2.0"}
  [s]
  (let [out (s/stream)]
    (s/connect
      out
      s)
    (s/splice
      out
      s)))

(defn client
  "Network client"
  {:added "0.2.0"}
  [host port]
  (d/chain (tcp/client {:host host, :port port})
    #(wrap-duplex-stream %)))

(defn show-in-cytoscape
  "Transfer JSON to a running Cytoscape on this host"
  {:added "0.2.0"}
  [json]
  (let [cyto-json (str (format "%08x" (count json)) json)
        cytoscape @(client "localhost" 34170)]
    @(s/put! cytoscape cyto-json)))

;; graph helpers -------------------------------------------------------------------

(def object-state "NORMAL")
(def edge-type "edge")
(def node-type "node")

(def edge-default
  {:edgeid nil ;; "arc-1863"
   :fromid nil ;; "node-1857"
   :toid nil   ;; "node-1860"
   :cost "0"
   :tc-lb "0"
   :tc-ub "0"
   :tpn-object-state object-state
   :type edge-type})

(def node-default
  {:nodeid nil ;; "node-1873",
   :tpn-type nil
   :tpn-object-state object-state
   :type node-type})

(def cytoscape-tpn-types
  {:state "TPN-STATE"
   :p-begin "TPN-PARALLEL-BEGIN"
   :p-end "TPN-PARALLEL-END"
   :c-begin "TPN-CHOICE-BEGIN"
   :c-end "TPN-CHOICE-END"
   :null-activity "null-activity"
   :network "network"
   :temporal-constraint "temporal-constraint"
   :cost<=-constraint "cost<=-constraint"
   :reward>=-constraint "reward>=-constraint"
   })

(defn edgeid
  "Convert the object to an edge identify"
  {:added "0.2.0"}
  [o]
  (let [uid (if (map? o) (:uid o) o)
        id (name uid)
        dash (.indexOf id "-")]
    (keyword (str "arc-" (subs id (inc dash) )))))

(defn make-constraint
  "Make constraint edges"
  {:added "0.2.0"}
  [tpn fromid toid constraint-id]
  (let [constraint (get tpn constraint-id)
        toid (or (:end-node constraint) toid)
        [lb ub] (or (:value constraint) [0 0])
        edge (assoc edge-default
               :edgeid (edgeid constraint-id)
               :fromid fromid
               :toid toid
               :name "Constraint\n\n" ;; make easier to read
               :tc-lb (str lb)
               :tc-ub (str ub))]
    edge))

(defn make-edges
  "make edges from activity and any constraints"
  {:added "0.2.0"}
  [tpn fromid activity]
  (let [toid (:end-node activity)
        unnamed-edge (assoc edge-default
                       :edgeid (edgeid activity)
                       :fromid fromid
                       :toid toid)
        edge (assoc-if unnamed-edge :name (:name activity))
        constraints (mapv (partial make-constraint tpn fromid toid) (:constraints activity))
        edges (conj constraints edge)]
    edges))

(declare merge-activities)
(declare graph-objects)

(defn graph-activity
  "Convert the activity to a TPN"
  {:added "0.2.0"}
  [activity tpn]
  (let [nodeid (:uid activity)
        toid (:end-node activity)
        cyto-tpn-type (get cytoscape-tpn-types (:tpn-type activity) "UNKNOWN")
        ;; FIXME: trial
        ;; node (assoc node-default :nodeid nodeid :tpn-type cyto-tpn-type)
        node (-> (merge node-default activity)
               (assoc :nodeid nodeid :tpn-type cyto-tpn-type))
        nodes [node]
        activities (mapv #(get tpn %) (:activities activity))
        edgesets (mapv (partial make-edges tpn nodeid) activities)
        constraints (mapv (partial make-constraint tpn nodeid toid) (:constraints activity))
        edges (apply concatv constraints edgesets)]
    (merge-activities tpn nodeid nodes edges activities)))

;; returns [nodes edges] where nodes and edges are vectors (or nil)
(defmulti graph-object
  "Convert any object to a TPN"
  {:added "0.2.0"}
  (fn [o tpn]
    (type o)))

(defmethod graph-object pamela.tpnrecords.activity [o tpn]
  (graph-objects tpn (:end-node o)))

(defmethod graph-object pamela.tpnrecords.null-activity [o tpn]
  (graph-objects tpn (:end-node o)))

(defmethod graph-object :default [o tpn]
  (graph-activity o tpn))

(defn merge-activities
  "Convert from activities to nodes and edges"
  {:added "0.2.0"}
  [tpn fromid nodes edges activities]
  (if (empty? activities)
    [nodes edges]
    (let [activity (first activities)
          [a-nodes a-edges] (graph-object activity tpn)
          new-nodes (if (empty? a-nodes) nodes (concatv nodes a-nodes))
          new-edges (if (empty? a-edges) edges (concatv edges a-edges))]
      (recur tpn fromid new-nodes new-edges (rest activities)))))

;; convert object graph to nodes and edges
(defn graph-objects
  "Convert a TPN to nodes and edges"
  {:added "0.2.0"}
  ([tpn]
   (graph-objects tpn (:begin-node ((:network-id tpn) tpn))))
  ([tpn begin-node]
   (let [begin (get tpn begin-node)]
     (graph-object begin tpn))))

;; -------------------------------------------------------------------

(def #^{:dynamic true :added "0.2.0" :doc/format :markdown}
  *graphvized*
  "A set of tpn :uid's which have already been graphed.

  NOTE: as this is a dynamic var it can be used with **binding**"
  (atom #{}))

(def graphviz-tpn-properties
  {;; :state ""
   :p-begin "style=diagonals"
   :p-end "style=diagonals"
   :c-begin "shape=doublecircle fillcolor=white"
   :c-end "shape=doublecircle fillcolor=white"
   ;; :activity ""
   ;; :null-activity ""
   ;; :network ""
   :temporal-constraint "color=lightgrey"
   :cost<=-constraint "color=lightgrey"
   :reward>=-constraint "color=lightgrey"
   })

(defn vizid
  "Make safe graphviz id from a keyword nodeid"
  {:added "0.2.0"}
  [nodeid]
  (string/replace (name nodeid) "-" ""))

(defn vizbounds
  "Make bounds for graphviz"
  {:added "0.2.0"}
  [value]
  (let [[lb ub] (or value [0 :infinity])
        default? (default-bounds? [lb ub])
        ub (if (= ub :infinity) "∞" ub)
        bounds
        (if default? ;; do NOT show default bounds!
          "" ;; (str "[" lb " " ub "]")
          (str "[" lb " " ub "]"))]
    bounds))

(defn makeviz-constraint
  "Make constraint edges"
  {:added "0.2.0"}
  [tpn fromid toid constraint-id]
  (if-not (@*graphvized* constraint-id)
    (let [{:keys [end-node value tpn-type]} (get tpn constraint-id)
          toid (or end-node toid)
          property (get graphviz-tpn-properties tpn-type "")
          edge (str (vizid fromid) " -> " (vizid toid)
                 " [label=\"" (vizbounds value) "\" " property "]")]
      (swap! *graphvized* union-items constraint-id)
      edge)))

(defn makeviz-edges
  "make edges from activity and any constraints"
  {:added "0.2.0"}
  [tpn fromid activity]
  (if-not (@*graphvized* (:uid activity))
    (let [{:keys [uid end-node name tpn-type constraints]} activity
          bounds (if (and (= tpn-type :activity) (= (count constraints) 1))
                   (str " " (vizbounds (:value (get tpn (first constraints)))))
                   "")
          edge (str (vizid fromid) " -> " (vizid end-node)
                 ;; " [label=\"" (vizid fromid) ":" name bounds "\"]") ;; w/ID
                 " [label=\"" name bounds "\"]")
          constraints (if (empty? bounds)
                        (mapv (partial makeviz-constraint tpn fromid end-node) constraints)
                        [])
          edges (conj constraints edge)]
      (if-not (empty? bounds)
        (swap! *graphvized* union-items (:uid (first constraints))))
      (swap! *graphvized* union-items uid)
      edges)))

(declare mergeviz-activities)
(declare graphviz-objects)

(defn graphviz-activity
  "Convert the activity to a TPN"
  {:added "0.2.0"}
  [activity tpn]
  (if-not (@*graphvized* (:uid activity))
    (let [{:keys [uid end-node tpn-type constraints activities]} activity
          property (get graphviz-tpn-properties tpn-type "")
          node (str (vizid uid) " [label=\"\" " property "]")
          ;; node (str (vizid uid) " [" property "]") ;; w/ID
          nodes [node]
          activities (mapv #(get tpn %) activities)
          edgesets (mapv (partial makeviz-edges tpn uid) activities)
          constraints (mapv (partial makeviz-constraint tpn uid end-node) constraints)
          edges (apply concatv constraints edgesets)]
      (swap! *graphvized* union-items uid)
      (mergeviz-activities tpn uid nodes edges activities))))

;; returns [nodes edges] where nodes and edges are vectors (or nil)
(defmulti graphviz-object
  "Convert any object to a graphviz element"
  {:added "0.2.0"}
  (fn [o tpn]
    (type o)))

(defmethod graphviz-object pamela.tpnrecords.activity [o tpn]
  (graphviz-object (get tpn (:end-node o)) tpn))

(defmethod graphviz-object pamela.tpnrecords.null-activity [o tpn]
  (graphviz-object (get tpn (:end-node o)) tpn))

(defmethod graphviz-object :default [o tpn]
  (graphviz-activity o tpn))

(defn mergeviz-activities
  "Convert from activities to nodes and edges"
  {:added "0.2.0"}
  [tpn fromid nodes edges activities]
  (if (empty? activities)
    [nodes edges]
    (let [activity (first activities)
          [a-nodes a-edges] (graphviz-object activity tpn)
          new-nodes (if (empty? a-nodes) nodes (concatv nodes a-nodes))
          new-edges (if (empty? a-edges) edges (concatv edges a-edges))]
      (recur tpn fromid new-nodes new-edges (rest activities)))))

(defn graphviz-objects
  "Convert a TPN to nodes and edges"
  {:added "0.2.0"}
  ([tpn]
   (graphviz-objects tpn (:begin-node ((:network-id tpn) tpn))))
  ([tpn begin-node]
   (binding [*graphvized* (atom #{})]
     (graphviz-object (get tpn begin-node) tpn))))

;; -------------------------------------------------------------------

(defn tpn->cytoscape
  "Convert a new style TPN map to an old (Cytoscape) style TPN map"
  {:added "0.2.0"}
  [tpn & {:keys [label] :as opts}]
  (let [label (or label "Pamela TPN")
        [nodes edges] (graph-objects tpn)
        layout "hierarchical"
        rotate "270"
        scaletofitwindow ""
        network {:Label label
                 :layout layout
                 :rotate rotate
                 :scaletofitwindow scaletofitwindow
                 :nodes nodes
                 :edges edges}
        cyto-tpn {:network network}]
    cyto-tpn))

(defn tpn->dot
  "Convert a new style TPN map to a graphviz dot file"
  {:added "0.2.0"}
  [tpn & {:keys [label] :as opts}]
  (let [;; label (or label "Pamela TPN")
        [nodes edges] (graphviz-objects tpn)
        font "Courier New"
        fontsize "12"
        graph (str "graph [rankdir=LR fontsize=" fontsize
                " fontname=\"" font "\" overlap=false penwidth=0.2];")
        node (str "node [fontsize=" fontsize
               " fontname=\"" font "\" shape=circle style=filled fillcolor=\"#00aadd\" color=\"#006699\" penwidth=1.0];")
        edge (str "edge [fontsize=" fontsize
               " fontname=\"" font "\"];")
        header ["digraph tpn {" graph node edge]
        footer ["}"]
        lines (apply concatv header nodes edges footer)
        dot (string/join "\n" lines)]
    dot))


(defn construct-tpn
  "TPN FIXME"
  {:added "0.2.0"}
  [demo-class tpn-field tpn-method & [out-format]]
  (let [demo-class (cond
                     (symbol? demo-class) demo-class
                     (string? demo-class) (symbol demo-class)
                     (var? demo-class) (deref demo-class)
                     :else
                     (throw (AssertionError.
                              (str "construct-tpn called with bad type for demo-class: " (type demo-class)))))
        tpn-field (cond
                    (keyword? tpn-field) tpn-field
                    (string? tpn-field) (keyword tpn-field)
                    :else
                    (throw (AssertionError.
                             (str "construct-tpn called with bad type for tpn-field: " (type tpn-field)))))
        tpn-method (cond
                     (symbol? tpn-method) tpn-method
                     (string? tpn-method) (symbol tpn-method)
                     :else
                     (throw (AssertionError.
                              (str "construct-tpn called with bad type for tpn-method: " (type tpn-method)))))
        demo-class nil ;; FIXME
        ;; (if-let [d (get-model demo-class)] d
        ;;                    (throw (AssertionError.
        ;;                             (str "construct-tpn undefined demo-class: "
        ;;                               demo-class))))
        demo (demo-class)
        ;; _ (println "demo-class" demo-class "tpn-field" tpn-field
        ;;     "tpn-method" tpn-method)
        ;; _ (pp/pprint demo)
        tpn (create-tpn demo tpn-field tpn-method)
        out-format (or out-format "tpn")
        out (case out-format
              "tpn" (with-out-str (pp/pprint tpn))
              "json" (with-out-str (json/pprint tpn))
              "cytoscape" (with-out-str (json/pprint (tpn->cytoscape tpn)))
              "dot" (tpn->dot tpn)
              ;; default
              (str "ERROR: unknown format: " out-format))]
    (if (string? out)
      (str out \newline)
      out)))

(defn construct-tpn-cfm
  "TPN FIXME"
  {:added "0.2.0"}
  [cfm & [out-format]]
  (let [[demo-class tpn-field tpn-method] (string/split cfm #":")]
    (if (and demo-class tpn-field tpn-method)
      (construct-tpn demo-class tpn-field tpn-method out-format)
      (throw
        (AssertionError.
          (str "construct-tpn-cfm requires a C:F:M argument where C is the demo-class, F is the TPN field and M is the tpn-method: " cfm))))))

;; NOTE loaded is now in PAMELA IR
(defn load-tpn
  "Load a tpn, loaded is the list of loaded class symbols.
  Presumably one symbol is the plant (defpclass taking no arguments)
  and the other symbol is the TPN (defpclass taking one argument)."
  {:added "0.2.0"}
  [loaded & [out-format]]
  ;; NO LONGER NECESSARY
  ;; (if-not (= 2 (count loaded))
  ;;   (throw (AssertionError. "load-tpn expects only two classes")))
  ;; ONE of them must be the plant (zero args)
  ;; the OTHER must be the TPN and have exactly one pmethod (one arg)
  (let [
        ;; plant (first (filter (comp zero? count :args meta get-model-var) loaded))
        ;; tpn-sym (first (filter (comp pos? count :args meta get-model-var) loaded))
        ;; tpn-var (if tpn-sym (get-model-var tpn-sym))
        ;; arglist (if tpn-var (:args (meta tpn-var)))
        ;; args (map lvar arglist)
        ;; tpn-fun (if tpn-var (deref tpn-var))
        ;; tpn-instance (apply tpn-fun args) ;; create an instance
        ;; methods (keys (:methods tpn-instance))
        ;; method (first methods)
        a 1]
    ;; (if (nil? plant)
    ;;   (throw (AssertionError. "load-tpn does not define a plant")))
    ;; (if (nil? tpn-sym)
    ;;   (throw (AssertionError. "load-tpn does not define a tpn class")))
    ;; (if-not (= 1 (count methods))
    ;;   (throw (AssertionError. "load-tpn expects tpn class with exactly one method")))
    ;; (println "DEBUG Will construct tpn based on" tpn-sym "method" method "using" plant)
    (let [
          ;; tpn-pclass-str (str "(defpclass my-tpn []\n"
          ;;                  "  :fields {:plant (" (name plant) ")\n"
          ;;                  "  :tpn (" (name tpn-sym) " plant)})")
          ;; _ (println "DEBUG TPN" tpn-pclass-str)
          legal false ;; FIXME
                 ;; (try (load-pamela-string tpn-pclass-str)
                 ;;     true
                 ;;     (catch Exception e (.. e getCause getMessage)))
          ;; _ (if-not (true? legal)
          ;;     (throw (AssertionError. (str "load-tpn could not create a synthetic class: " legal))))
          ;; my-tpn (get-model "my-tpn")
          ;; my-tpn-instance (my-tpn)
          ;; tpn (create-tpn my-tpn-instance :tpn method)
          tpn loaded ;; FIXME
          out-format (or out-format "tpn")
          out (case out-format
                "tpn" tpn
                ;; consider...
                ;; "tpn" (with-out-str (pp/pprint tpn))
                "json" (with-out-str (json/pprint tpn))
                "cytoscape" (with-out-str (json/pprint (tpn->cytoscape tpn)))
                "dot" (tpn->dot tpn)
                ;; default
                (str "ERROR: unknown format: " out-format))]
      (if (string? out)
        (str out \newline)
        out))))

;; demonstrate loading a plant and a tpn from a file
;; and auto-generating the necessary glue pclass
(defn load-tpn-files
  "Load one or more TPN files (typically a plant and a tpn)"
  {:added "0.2.0"}
  [& filenames]
  nil ;; FIXME
  )
  ;; (binding [*pclasses* (atom [])]
  ;;   ;; (println "loading tpn files from:" filenames)
  ;;   ;; FIXME
  ;;   ;; (doseq [filename filenames]
  ;;   ;;   (load-pamela-project filename))
  ;;   (load-tpn @*pclasses*)))

(defn visualize
  "Visualize TPN as SVG

  Assumes: --visualize --format dot tpn

  * If run on the command line
    * If the --output is STDOUT then save the Graphviz to CWD/tpn.dot
    * Save the SVG file to the same filename as the Graphviz file + .svg

  * If run against the pamelad daemon
    * Save the Graphviz file to resources/public/downloads/tpn.dot
    * Save the SVG file to the same filename as the Graphviz file + .svg
    * If running in a browser redirect to (display) SVG file
    * Otherwise the user can download the svg file from downloads
  "
  {:added "0.2.0" :doc/format :markdown}
  [tpn options]
  (let [{:keys [format output verbose cwd web]} options
        tpnfile (as-file (if (daemon/stdout-option? output)
                           (str cwd "/tpn.dot")
                           output))
        tpnname (.getName tpnfile)
        tpnweb (if (daemon/running?)
                 (as-file (str (:user-dir env) "/" web/downloads "/" tpnname)))
        tpnfile (or tpnweb tpnfile)
        svgfile (as-file (str (.getPath tpnfile) ".svg"))
        svgname (.getName svgfile)
        svgweb (if (daemon/running?)
                 (as-file (str (:user-dir env) "/" web/downloads "/" svgname)))
        svgfile (or svgweb svgfile)
        cmd ["dot" "-Tsvg" "-o" (.getPath svgfile) (.getPath tpnfile)]]
    ;; If we are creating files in the downloads dir, make sure it exists
    (when (and (pos? (.indexOf (.getPath svgfile) web/downloads))
            (not (.exists (.getParentFile svgfile))))
      (log/info (str "Making downloads directory: " (.getParentFile svgfile)))
      (.mkdirs (.getParentFile svgfile)))
    ;; even if tpnfile is stdout we need it as a file for dot
    (when (daemon/stdout? output)
      (spit tpnfile tpn))
    (log/info (str "TPN file: " (.getPath tpnfile)))
    (log/info (str "Visualizing TPN to file: " (.getPath svgfile)))
    (log/info (str "cmd: " cmd))
    ;; run dot
    (try
      (let [{:keys [exit out err]} (apply sh cmd)]
        (if (zero? exit)
          (do
            ;; Already used -o ... (spit svgfile out)
            ;; pamelad? redirect to the SVG file
            (when (daemon/running?)
              (if (and web (= web "curl"))
                (log/info (str "curl detected: client will find files in downloads/"))
                (let [{:keys [host port]} @daemon/server
                      host (if (= host "0.0.0.0") "localhost" host)
                      svgurl (str "http://" host ":" port "/downloads/" svgname)]
                  (log/info (str "Redirecting to SVG: " svgurl))
                  (println "// redirect:" svgurl)
                  ))))
          (do
            (println "Error converting to SVG (" exit "):" err)
            (log/error (str "Error converting to SVG (" exit "):" err))
            )))
      (catch Exception e
        (println "Error converting to SVG):"
          (.. e getCause getMessage))
        (log/error (str "Error converting to SVG: " (.. e getCause getMessage)))
        ))))
