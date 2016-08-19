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

(ns pamela.db
  "PAMELA database functions.

  During development these functions support running Elasticsearch
  in standalone mode.

  The data storage will be kept in the data/ directory.

  Configuration is in the resources/es-config/ directory."
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.pprint :as pp]
            [environ.core :refer [env]]
            [clojurewerkz.elastisch.native :as es]
            [clojurewerkz.elastisch.native.index :as esi]
            [clojurewerkz.elastisch.native.document :as doc]
            [clojurewerkz.elastisch.query :as q]
            [clojurewerkz.elastisch.native.response :as esrsp]
            [pamela.utils :refer [get-input sleep repl? http-get]]
            [avenir.utils :refer [and-fn assoc-if]]
            [clojure.tools.logging :as log]
            [pamela.daemon :as daemon]
            ;; [pamela.pclass :as pclass]
            ;; [pamela.models :as models]
            )
  (:import [java.net.URL]))

;; (def #^{:added "0.2.0"} db
;; "Database node and connection atom. Holds the state of the standalone Elasticsearch instance."
(defonce db (atom nil))

(def #^{:added "0.2.0"} settings
  "Settings for standalone Elasticsearch DB"
  {:discovery.zen.ping.multicast.enabled false
   :gateway.expected_nodes 0
   :gateway.recover_after_nodes 0
   :http.cors.enabled true
   :http.cors.allow-credentials true
   :index.number_of_replicas 0
   :index.store.type "default"
   :network.host "127.0.0.1"
   :node.data    true
   :node.master  true
   :path.data    "data"
   :path.conf    "resources/es-config"})

(def #^{:added "0.2.0"}
  db-mapping-types
  "PAMELA mapping types for Elasticsearch"
  {"model" {:properties
            {:name {:type "string" :store "yes"}
             :version {:type "string" :store "yes"}
             :doc {:type "string" :store "yes"}
             ;; :icon {:type "string" :store "yes"}
             :depends {:type "string" :store "yes"}
             :source  {:type "string" :store "yes"} ;; source code
             :url {:type "string" :store "yes"}}}}) ;; provenance

(def #^{:added "0.2.0"}
  db-index
  "PAMELA index for Elasticsearch"
  "pamela")

(defn db-node
  "Return the current Elasticsearch DB node used for API calls"
  {:pamela :db :added "0.2.0"}
  []
  (:node @db))

(defn db-conn
  "Return the current Elasticsearch DB connection used for API calls"
  {:pamela :db :added "0.2.0"}
  []
  (:conn @db))

(defn running?
  "Return true if the Elasticsearch is running"
  {:pamela :db :added "0.2.0"}
  []
  (not (nil? (db-node))))

(defn wait-for-green
  "Wait for the DB be in the GREEN status"
  {:pamela :db :added "0.2.0"}
  []
  (let [cac (-> (db-node) .client .admin .cluster)
        chr (-> (.prepareHealth cac (into-array String nil)) .setWaitForGreenStatus .execute .get)]
    (or (= (.value (.getStatus chr)) 0)
      (println "Warning DB status is not GREEN"))))

(declare list-models)

(defn sync-models-to-db
  "Ensure all models in memory are indexed by Elasticsearch DB"
  {:pamela :db :added "0.2.0"}
  []
  (let [max 30] ;; * 2 seconds
    (loop [i 0
           clj-models [] ;; FIXME (mapv str (pclass/list-models))
           db-models [] ;; FIXME (list-models {:simple true})
           ]
      (if (= db-models clj-models)
        (do
          (log/debug (str "sync-models-to-db complete:" db-models))
          (when (pos? i) ;; we have had to sync...
            (sleep 3))) ;; just to be sure
        (do
          (when (zero? i)
            (log/debug (str "sync-models-to-db out of sync!")))
          (log/debug (str "CLJ models: " clj-models))
          (log/debug (str "DB  models: " db-models))
          (sleep 2)
          (if (< i max)
            (recur (inc i) clj-models (list-models {:simple true}))))))))

(defn stop-db
  "Stop the standalone Elasticsearch DB"
  {:pamela :db :added "0.2.0"}
  [& [options]]
  (if (running?)
    (do
      (sync-models-to-db)
      (when-not (true? (db-node)) ;; remote DB
        ;; for scripted operations we must ensure that pending
        ;; writes have completed
        (log/info "stopping standalone DB")
        (wait-for-green)
        (sleep 2)
        (.close (db-node)))
      (reset! db nil))
    (println "db already stopped")))

(defn stop-db-exit
  "Stop the standalone Elasticsearch DB and exit."
  {:pamela :db :added "0.2.0"}
  [& [options]]
  (sync-models-to-db)
  (log/debug (str "daemon/running? " (daemon/running?) " remote " (true? (db-node))))
  (when (and (not (daemon/running?)) (not (repl?)))
    (if (true? (db-node)) ;; remote DB
      (sleep 3)
      (do
        (wait-for-green)
        (log/info "stopping standalone DB, will exit")
        ;; NOTE the following WILL call System/exit
        (let [cac (-> (db-node) .client .admin .cluster)
              pns (.prepareNodesShutdown cac (into-array String nil))]
          (.nodesShutdown cac (.request pns)))))))

(defn start-db
  "Start the standalone Elasticsearch DB"
  {:pamela :db :added "0.2.0"}
  [& [options]]
  (let [verbose (:verbose options)
        verbose? (pos? (or verbose 0))]
    (when-not (running?)
      (log/info "starting standalone DB")
      (let [node (es/build-local-node settings :client false)
            started (es/start-local-node node)
            conn (es/connect-to-local-node node)]
        (reset! db {:node node :conn conn})
        (wait-for-green)))));; returns true on success

(defn start-db-remote
  "Start the a connection to a remote Elasticsearch DB"
  {:pamela :db :added "0.2.0"}
  [& [options]]
  (when-not (running?)
    (let [{:keys [verbose database]} options
          verbose? (pos? (or verbose 0))
          port 9200
          url (str "http://" database ":" port "/_nodes/cluster")
          data (try
                 (http-get url {:as :json})
                 (catch Exception e
                   (println "ERROR: unable to GET " url "\n"
                     (.. e getMessage))))
          body (if (= 200 (:status data)) (:body data))
          cluster (:cluster_name body)]
      (log/info (str "starting remote DB: " database))
      (if (empty? cluster)
        (println "ERROR: unable to connect to DB at:" url)
        (let [conn (es/connect [[database 9300]]
                     {"cluster.name" cluster})
              node (not (nil? conn))]
          (if-not node
            (println "ERROR: unable to establish connection to DB at:" url)
            (do
              (if verbose?
                (println "DB cluster is:" cluster))
              (reset! db {:node node :conn conn}))))))))

;; Warning: this function will crash if there are dependency cycles
(defn- solve-dependency
  "Given a set of solved model indexes and unresolved deps
  return a valid dependency loading order"
  {:pamela :db :added "0.2.0"}
  [solved deps]
  (if (empty? deps)
    solved
    (let [dep (first deps)
          needs (subvec dep 2)]
      ;; (println "solved:" solved "deps:" deps "\ndep:" dep "needs:" needs)
      ;; if all the needs are solved then this dep is solved
      (if (reduce and-fn (map (fn [need] (some #(= need %) solved)) needs))
        (recur (conj solved (first dep)) (subvec deps 1))
        ;; try again in a different order
        (recur solved (conj (subvec deps 1) dep))))))

(defn- model-load-order
  "Given the full vector of models return a vector
  of valid indexes to load (in dependency order)"
  {:pamela :db :added "0.2.0"}
  [models]
  (let [model-indexes (apply merge
                        (for [i (range (count models))
                              :let [m (get models i)]]
                          {(:name m) {:i i :depends (:depends m)}}))
        order (vec (for [k (keys model-indexes)
                         :let [v (get model-indexes k)]]
                     (if (nil? (:depends v))
                       (:i v)
                       (vec
                         (cons (:i v)
                           (cons :after
                             (for [d (:depends v)
                                   :let [m (first d)
                                         mi (:i (get model-indexes m))]]
                               mi)))))))
        nodeps (vec (filter integer? order))
        deps (vec (filter vector? order))
        solved (solve-dependency nodeps deps)]
    ;; (pp/pprint model-indexes)
    ;; (println "ORDER" order)
    ;; (println "SOLVED" solved)
    solved))

(defn start-db-pamela
  "Start the standalone Elasticsearch DB and load all models"
  {:pamela :db :added "0.2.0"}
  [& [options]]
  (let [{:keys [verbose database]} options
        verbose? (pos? (or verbose 0))]
    (if (running?)
      true
      (when (if database
              (start-db-remote options)
              (start-db options))
        (when-not (esi/exists? (db-conn) db-index)
          ;; create model mapping if needed
          (if verbose?
            (println "creating" db-index "index"))
          (esi/create (db-conn) db-index :mappings db-mapping-types))
        ;; NOTE: not deleting anything from the models ns here
        (let [models (list-models)
              order (model-load-order models)]
          (log/debug (str "loading models: " (vec (sort (map :name models)))))
          (doseq [i order]
            (let [model-data (models i)
                  {:keys [name source url id]} model-data
                  load-result nil ;; FIXME (models/load-pamela-string source url)
                  ]
              (if (empty? load-result) ;; model loaded correctly
                ;; FIXME
                ;; (let [model-var (pclass/get-model-var name)]
                ;;   (alter-meta! model-var assoc :id id))
                (log/error (str "unable to load model \""
                             (:name model-data) "\" result:" load-result))))))
        true))))

;; db helpers ------------------

;; convert model data from es
;; consider making this private?
(defn es-to-model
  "Convert ES DB data structure to model-data"
  {:pamela :db :added "0.2.0"}
  [hit]
  (let [id (:_id hit)
        data (:_source hit)
        url (if-let [url (:url data)] url)
        icon (if-let [icon (:icon data)] icon)
        model-data (-> data
                     (assoc :id id)
                     (assoc-if :url url)
                     (assoc-if :icon icon))]
    model-data))

(defn get-model
  "Read a given model from the database

  Should not return duplicates, but the results must be verified.
  n (:total hits)
  hit (first (:hits hits))
  id (:_id hit)"
  {:added "0.2.0"}
  [model]
  (when-not (running?)
    (throw (AssertionError. "database not running")))
  (when model
    (let [results (doc/search (db-conn) db-index "model"
                    :query (q/term :name model))
          hits (:hits results)
          models (mapv es-to-model (:hits hits))]
      models)))

(defn delete-model-by-id
  "Deletes given model (given the id) from the database"
  {:added "0.2.0"}
  [id & [options]]
  (let [verbose (:verbose options)]
    (when-not (running?)
      (throw (AssertionError. "database not running")))
    (if (and verbose (pos? verbose) (> verbose 1))
      (println "delete-model-by-id:" id))
    (doc/delete (db-conn) db-index "model" id)))

(defn delete-all-models
  "Deletes all the models from the database and models namespace"
  {:added "0.2.0"}
  [& [options]]
  (let [verbose (:verbose options)
        verbose? (pos? (or verbose 0))]
    (when-not (running?)
      (throw (AssertionError. "database not running")))
    (if verbose?
      (println "delete-all-models..."))
    (doseq [id (map :id (list-models))]
      (delete-model-by-id id options))
    ;; FIXME
    ;; (pclass/delete-all-models)
    ))

(defmacro with-db
  "Run PAMELA operations on database.  If the :database option
   is set then connect to that remote server, otherwise
   run a local, standalone DB.

  The DB will be started, body executed, then DB shutdown.
  This macro will catch any exceptions and ensure the DB
  is shutdown cleanly and then the process will exit."
  {:added "0.2.0"}
  [options & body]
  `(when (start-db-pamela ~options)
     (try
       ~@body
       (catch Exception e#
         (println "ERROR caught exception:" (.getMessage e#))
         ;; Currently not enabled because
         ;; java.lang.IllegalArgumentException: No matching method found: printStackTrace for class clojure.lang.Compiler$CompilerException, compiling:(/tmp/form-init60632624767957047.clj:1:71)
         ;; (.printStackTrace e# *out*)
         )
       (finally
         (stop-db-exit ~options)))))

;; actions -------------------------------------

(defn list-models
  "List models in the database"
  {:pamela :db :added "0.2.0"}
  [& [options]]
  (when-not (running?)
    (throw (AssertionError. "database not running")))
  (let [{:keys [verbose simple]} options
        verbose? (pos? (or verbose 0))
        results (try
                  (doc/search (db-conn) db-index "model" :query (q/match-all))
                  (catch Exception e nil))
        hits (:hits results)
        n (or (:total hits) 0)]
    (when verbose?
      (println (str "there " (if (= n 1) "is" "are") " " n
                 " model" (if-not (= n 1) "s") " in the db:")))
    (let [models (mapv es-to-model (:hits hits))]
      (if simple
        (vec (sort (map :name models)))
        models))))

(defn delete-model
  "Deletes given model from the database

  Will result in in error if {:model model-name} is
  not found unless {:db-only true}. NOTE: will delete
  entire database if model-name is :all"
  {:added "0.2.0"}
  [options]
  (when-not (running?)
    (throw (AssertionError. "database not running")))
  (let [{:keys [verbose model recursive db-only]} options
        verbose? (pos? (or verbose 0))]
    (if (or (= model :all) (= model ":all"))
      (delete-all-models options)
      (let [model-name (name model)
            model (symbol model-name)
            hits (get-model model-name)
            n (count hits)]
        (if verbose?
          (println "delete-model" model-name))
        (when (and (zero? n) (not db-only))
          (println "ERROR: model not found:" model-name))
        (when (> n 1)
          (println "WARNING:" n "copies of this model found:" model-name))
        (when (pos? n)
          (let [rdepends nil ;; FIXME (pclass/model-reverse-depends)
                rdeps (:rdepends (get rdepends model))]
            (when
                (if (pos? (count rdeps))
                  (if (and (not recursive) (not db-only))
                    (do
                      (println (str "ERROR: cannot delete model \"" model-name "\" without --recursive due to these reverse dependencies: " rdeps))
                      false)
                    (do
                      (if verbose?
                        (println "delete reverse dependencies:" rdeps))
                      (doseq [d rdeps]
                        (let [m (first d)]
                          (delete-model (assoc options :model m))))
                      true))
                  true)
              (doseq [id (map :id hits)]
                (delete-model-by-id id options)
                ;; FIXME
                ;; (if-not db-only
                ;;   (pclass/delete-model model))
                ))))))
    ))

(defn export-model
  "Export model source from the database"
  {:added "0.2.0"}
  [options]
  (when-not (running?)
    (throw (AssertionError. "database not running")))
  (let [{:keys [verbose model output recursive]} options
        verbose? (pos? (or verbose 0))
        models (get-model model)
        n (count models)
        model-data (first models)
        source (:source model-data)]
    (if-not model
      (println "ERROR: model to export not provided"))
    (when model-data
      (if-not (= n 1)
        (println "ERROR:"
          (str "there " (if (= n 1) "is" "are") " " n
            " model" (if-not (= n 1) "s") " in the db for model:" model)))
      (when (and (not (daemon/stdout? output)) (or (not recursive) (true? recursive)))
        ;; truncate the file if it is not recursive
        ;; OR on the first model of a recursive run (rest will be :append)
        (if verbose?
          (println "creating output file:" output))
        (spit output ""))
      (when recursive
        (let [depends (:depends model-data)
              needs (for [d depends :let [m (first d)]] m)]
          (doseq [need needs]
            (export-model (assoc options :model need :recursive :append)))))
      (when (= n 1)
        (if verbose?
          (println ";; exporting:" model))
        (if (daemon/stdout? output)
          (print source) ;; NOTE no \newline !!!
          (spit output source :append true))
        (flush) ;; ensure stdout is flushed
        ))))

(defn import-model
  "Import model into the database"
  {:added "0.2.0"}
  [options]
  (let [msg "import-model not implemented yet"]
    (println msg)
    (log/error msg)
    false))
  ;; (when-not (running?)
  ;;   (throw (AssertionError. "database not running")))
  ;; (let [{:keys [verbose]} options ;; model not required
  ;;       verbose? (pos? (or verbose 0))
  ;;       [url source] (get-input options)]
  ;;   (if verbose?
  ;;     (println "import-model" (count source) "bytes from" url))
  ;;   ;; FIXME if source is missing
  ;;   (when source
  ;;     (try
  ;;       (binding [pclass/*pclasses* (atom [])]
  ;;         (let [load-result nil ;; FIXME (models/load-pamela-string source url)
  ;;               ]
  ;;           (if-not (empty? load-result)
  ;;             (log/errorf "unable to import: %s" load-result)
  ;;             (let [new-models @pclass/*pclasses*]
  ;;               (if verbose?
  ;;                 (println "import-model found these new models:" new-models))
  ;;               (doseq [model new-models]
  ;;                 (let [model-name (name model)
  ;;                       hits (get-model model-name) ;; stale version in DB?
  ;;                       ;; NOTE map would be elided here as the value isn't used
  ;;                       _ (mapv #(delete-model-by-id (:id %) {:verbose 2}) hits)
  ;;                       model-var (pclass/get-model-var model)
  ;;                       model-meta (meta model-var)
  ;;                       {:keys [version depends doc]} model-meta
  ;;                       model-data (-> {:name model-name
  ;;                                       :url url ;; string
  ;;                                       :source source}
  ;;                                    (assoc-if :version version)
  ;;                                    (assoc-if :depends depends)
  ;;                                    (assoc-if :doc doc))
  ;;                       result (doc/create (db-conn) db-index "model" model-data)
  ;;                       id (:_id result)]
  ;;                   (if id
  ;;                     (do
  ;;                       (alter-meta! model-var assoc :id id)
  ;;                       (if (and verbose (pos? verbose) (> verbose 1))
  ;;                         (println "imported:" model-name "as id:" id)))
  ;;                     (log/errorf "could not create model: %s" result))))))))
  ;;         (catch Exception e
  ;;           (println "ERROR: invalid model:" (.. e getCause getMessage)))))))

(defn describe-model
  "Analyze and describe the given model"
  {:added "0.2.0"}
  [options]
  (let [msg "describe-model not implemented yet"]
    (println msg)
    (log/error msg)
    false))
  ;; (when-not (running?)
  ;;   (throw (AssertionError. "database not running")))
  ;; (let [{:keys [verbose model output]} options
  ;;       verbose? (pos? (or verbose 0))
  ;;       model-var (pclass/get-model-var model true)]
  ;;   (if-not model-var
  ;;     (println "ERROR: model not found:" model)
  ;;     (let [model-desc (pclass/describe-model model)]
  ;;       (if (daemon/stdout? output)
  ;;         (print model-desc)
  ;;         (spit output model-desc))))))
