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

(ns pamela.tpnrecords
  "TPN helper functions."
  (:require [clojure.pprint :as pp]
            [clojure.data.json :as json]))

(def #^{:added "0.2.4"} next-tpnsym
  "Next number for typsym"
  (atom 0))

(defn reset-tpnsym!
  "Reset the tpnsym number"
  {:added "0.2.4"}
  []
  (reset! next-tpnsym 0))

(defn tpnsym
  "Generates a symbol (based on next-tpnsym).
  Like gensym, but resettable via reset-tpnsym.
  If prefix is not supplied then 'uid-' is used."
  {:added "0.2.4"}
  ([]
   (tpnsym "uid-"))
  ([prefix]
   (str prefix (swap! next-tpnsym inc))))

                                        ; Structures to hold some information
(defrecord network [uid begin-node])                        ; Network contains begin node of TPN

                                        ; Nodes
(defrecord state [uid activities constraints incidence-set])
(defrecord p-begin [uid activities constraints incidence-set end-node])
(defrecord p-end [uid activities incidence-set])
(defrecord c-begin [uid activities constraints incidence-set end-node])
(defrecord c-end [uid activities incidence-set])

                                        ; Arcs
(defrecord null-activity [uid end-node])                    ; constraints will have single object containing temporal constraints [0 0]
(defrecord activity [uid name cost reward constraints end-node])

                                        ; Constraints
(defrecord temporal-constraint [uid value end-node])
(defrecord cost<=-constraint [uid value end-node])
(defrecord reward>=-constraint [uid value end-node])

                                        ; Constructor fns. Maybe a macro to generate them would be nice
                                        ; ------------------------------------------------------------------------------------------------------------------
(defn make-state
  "Node constructor. Create with supplied values or defaults and then merge with given map"
  {:added "0.2.0"}
  [m & {:keys [uid activities constraints incidence-set]
        :or   {uid         (keyword (tpnsym "node-")), activities #{},
               constraints #{}, incidence-set #{}}}]
  (merge (->state uid activities constraints incidence-set) m {:tpn-type :state}))

(defn make-p-begin
  "Make a parallel begin node"
  {:added "0.2.0"}
  [m & {:keys [uid activities constraints incidence-set end-node]
        :or   {uid         (keyword (tpnsym "node-")), activities #{},
               constraints #{}, incidence-set #{}, end-node nil}}]
  (merge (->p-begin uid activities constraints incidence-set end-node) m {:tpn-type :p-begin}))

(defn make-p-end
  "Make a parallel end node"
  {:added "0.2.0"}
  [m & {:keys [uid activities incidence-set]
        :or   {uid (keyword (tpnsym "node-")), activities #{}, incidence-set #{}}}]
  (merge (->p-end uid activities incidence-set) m {:tpn-type :p-end}))

(defn make-c-begin
  "Make a choice begin node"
  {:added "0.2.0"}
  [m & {:keys [uid activities constraints incidence-set end-node]
        :or   {uid         (keyword (tpnsym "node-")), activities #{},
               constraints #{}, incidence-set #{}, end-node nil}}]
  (merge (->c-begin uid activities constraints incidence-set end-node) m {:tpn-type :c-begin}))

(defn make-c-end
  "Make a choice end node"
  {:added "0.2.0"}
  [m & {:keys [uid activities incidence-set]
        :or   {uid (keyword (tpnsym "node-")), activities #{}, incidence-set #{}}}]
  (merge (->c-end uid activities incidence-set) m {:tpn-type :c-end}))

(defn make-activity
  "Make an activity"
  {:added "0.2.0"}
  [m & {:keys [uid name cost reward constraints end-node]
        :or   {uid (keyword (tpnsym "act-")), name "", cost 0, reward 0, constraints #{}, end-node nil}}]
  "Activity constructor. Create with supplied values or defaults and then merge with given map"
  (merge (->activity uid name cost reward constraints end-node) m {:tpn-type :activity}))

(defn make-null-activity
  "Make an activity"
  {:added "0.2.0"}
  [m & {:keys [uid end-node]
        :or   {uid (keyword (tpnsym "act-"))}}]
  "Null activity constructor."
  (merge (->null-activity uid end-node) m {:tpn-type :null-activity}))

                                        ; Since network is not exactly a TPN object but an enclosing container, we do not add :tpn-type to it.
(defn make-network
  "Make a network"
  {:added "0.2.0"}
  [m & {:keys [uid begin-node]
        :or   {uid (keyword (tpnsym "net-"))}}]
  "Network constructor. Create with supplied values or defaults and then merge with given map"
  (if-not uid
    (merge (->network (keyword (tpnsym "net-")) begin-node) m {:tpn-type :network})
    (merge (->network uid begin-node) m {:tpn-type :network})))

(defn make-temporal-constraint
  "Make a temporal constraint"
  {:added "0.2.0"}
  [m & {:keys [uid value end-node]
        :or   {uid (keyword (tpnsym "cnstr-")) value [0 :infinity]}}]
  (merge (->temporal-constraint uid value end-node) m {:tpn-type :temporal-constraint}))

(defn make-cost<=-constraint
  "Make a cost<= constraint"
  {:added "0.2.0"}
  [m & {:keys [uid value end-node]
        :or   {uid (keyword (tpnsym "cnstr-")) value 0}}]
  (merge (->cost<=-constraint uid value end-node) m {:tpn-type :cost<=-constraint}))

(defn make-reward>=-constraint
  "Make a reward>= constraint"
  {:added "0.2.0"}
  [m & {:keys [uid value end-node]
        :or   {uid (keyword (tpnsym "cnstr-")) value 0}}]
  (merge (->reward>=-constraint uid value end-node) m {:tpn-type :reward>=-constraint}))

                                        ; :tpn-type to constructor-fn map
(def tpn-type-2-create {:state    make-state :p-begin make-p-begin :p-end make-p-end
                        :c-begin  make-c-begin :c-end make-c-end :null-activity make-null-activity
                        :activity make-activity :temporal-constraint make-temporal-constraint :network make-network})

                                        ; Rest
(defn wire-activity
  "Wire up an activity"
  {:added "0.2.0"}
  [from act to]
  "Updates from node's activity set and to node's incidence set with the given activity."
  [(update from :activities conj (:uid act))
   (update to :incidence-set conj (:uid act))
   ])


(defmulti walk-tpn
  "Walk the tpn"
  {:added "0.2.0"}
  (fn [obj _ _ _]
    (type obj)))

(defn set-visited!
  "Set the visited status of obj"
  {:added "0.2.0"}
  [obj visited]
  (var-set visited (conj @visited (:uid obj)))
  #_(println "set-visited!" @visited "\n"))

(defn visited?
  "inspect visited status of ojb"
  {:added "0.2.0"}
  [obj visited]
  (contains? @visited (:uid obj)))

(defn print-walk
  "print walk"
  {:added "0.2.0"}
  [obj _]
  (println "Visited" (:uid obj) (type obj)))

(defn begin-walk
  "begin walk"
  {:added "0.2.0"}
  [obj func objects]
  (if-not obj
    (println "begin-walk obj is nil"))
  (if-not objects
    (println "begin-walk objects is nil"))
  (if-not func
    (println "walk fun is nil. Assuming print-walk"))

  (with-local-vars [visited #{}]
    (walk-tpn obj (if func func print-walk) objects visited)))

(defmethod walk-tpn :default [obj func objects _]
  (binding [*out* *err*]
    (println "walk-tpn handle type:" (type obj) func objects)))

(defn walk-tpn-begin
  "walk tpn begin"
  {:added "0.2.0"}
  [obj func objects visited]
  (if-not (visited? obj visited)
    (do (func obj objects)
        (set-visited! obj visited)
        (doseq [act (:activities obj)]
          #_(println "walking" act " of " (:activities obj))
          (walk-tpn (act objects) func objects visited)))
    #_(println "already visited" (:uid obj) (:tpn-type obj)))
  )

(defmethod walk-tpn p-begin [obj func objects visited]
  #_(println "\nwalk p-begin" (:uid obj) (:end-node obj))
  (walk-tpn-begin obj func objects visited))

(defmethod walk-tpn c-begin [obj func objects visited]
  #_(println "\nwalk c-begin" (:uid obj) (:end-node obj))
  (walk-tpn-begin obj func objects visited))

(defn walk-tpn-state
  "walk tpn state"
  {:added "0.2.0"}
  [obj func objects visited]
  (if-not (visited? obj visited)
    (do (func obj objects)
        (set-visited! obj visited)
        (when (first (:activities obj))
          (walk-tpn ((first (:activities obj)) objects) func objects visited)))
    #_(println "already visited" (:uid obj) (:tpn-type obj))))

(defmethod walk-tpn state [obj func objects visited]
  (walk-tpn-state obj func objects visited))

(defmethod walk-tpn p-end [obj func objects visited]
  (if-not (visited? obj visited)
    (walk-tpn-state obj func objects visited)
    #_(println "already visited" (:uid obj) (:tpn-type obj))))

(defmethod walk-tpn c-end [obj func objects visited]
  (if-not (visited? obj visited)
    (walk-tpn-state obj func objects visited)
    #_(println "already visited" (:uid obj) (:tpn-type obj))))


(defn walk-tpn-activity
  "walk tpn activity"
  {:added "0.2.0"}
  [obj func objects visited]
  (if-not (visited? obj visited)
    (do (func obj objects)
        (set-visited! obj visited)
        (walk-tpn ((:end-node obj) objects) func objects visited))
    #_(println "already visited" (:uid obj) (:tpn-type obj))))

(defmethod walk-tpn null-activity [obj func objects visited]
  (walk-tpn-activity obj func objects visited))

(defmethod walk-tpn activity [obj func objects visited]
  (walk-tpn-activity obj func objects visited))

                                        ; Collect tpn objects
                                        ; --------------------------------------------------------------------------

(defn collect-tpn
  "collect tpn"
  {:added "0.2.0"}
  [netid objs]
  "Walk the tpn and creates a map of nodes and edges"
  #_(println "objs")
  #_(clojure.pprint/pprint objs)
  (with-local-vars [m {}]
    (let [network (netid objs)
          bnode (:begin-node network)
          ]
      (var-set m (assoc @m netid network))
      #_(println "collect-tpn" objs)
      #_(clojure.pprint/pprint @m)
      (begin-walk (bnode objs) (fn [obj _]
                                        ;(println "got " (:uid obj) (type obj))
                                 (var-set m (assoc @m (:uid obj) obj #_(merge obj {:checkme (.getSimpleName (type obj))})))
                                 #_(clojure.pprint/pprint @m)
                                 (when-not (empty? (:constraints obj))
                                   #_(println "constraints" (:constraints obj))
                                   (doseq [c (:constraints obj)]
                                     (var-set m (assoc @m c (c objs)))))
                                 ) objs)
      @m
      )))

(defn collect-tpn-with-netid
  "collect tpn with netid"
  {:added "0.2.0"}
  [netid obj]
  (merge (collect-tpn netid obj) {:network-id netid}))

;; tpnjson

(defn convert-tcub
  "convert tcub"
  {:added "0.2.0"}
  [str]
  "Fn to convert temporal upper bound to Int value or symbol"
  (try
    (Integer/parseInt str)
    (catch NumberFormatException _
      (symbol str))))

(defn val-converter
  "val converter"
  {:added "0.2.0"}
  [k v]
  "Fn to convert values to appropriate types"
  (cond (contains? #{:nodeid :edgeid :fromid :toid :tpn-type :network-id :uid :end-node :begin-node} k)
        (if v (keyword (.toLowerCase v))
            ;; (println "val-converter nil value for key" k)
            )

        (contains? #{:activities :incidence-set :constraints} k) (into #{} (map keyword v))

        (= :tc-lb k) (if (= String (type v))
                       (Integer/parseInt v)
                       v)
        (= :tc-ub k) (if (= String (type v))
                       (convert-tcub v)
                       v)
                                        ; Convert other keys as needed. For now they will be string.
        :otherwise v))

(defn map-from-json-str
  "map from json str"
  {:added "0.2.0"}
  [content]
  (json/read-str content
    :key-fn #(keyword %)
    :value-fn val-converter)
  )

(defn from-file
  "from file"
  {:added "0.2.0"}
  [filename]
  "Read json from a file"
  (map-from-json-str (slurp filename)))

(defn to-file
  "to file"
  {:added "0.2.0"}
  [m fname]
  "Write json to the file"
  (spit fname (json/write-str m))
  )
