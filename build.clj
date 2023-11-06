(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'dollabs/pamela)
(def build-folder "target")
(def class-dir (str build-folder "/classes"))

(def basis (b/create-basis {:project "deps.edn"}))
(def version "0.6.4")
;;(def version (format "1.6.%s" (b/git-count-revs nil)))
(def jar-file (format "target/%s-%s.jar" (name lib) version))
(def app-name "pamela")
(def uber-file (format "%s/%s-%s-standalone.jar" build-folder app-name version)) ; path for result uber file

(defn clean [_]
  (b/delete {:path "target"})
  (println (format "Build folder \"%s\" removed" build-folder)))

(defn jar [_]
  (b/write-pom {:class-dir class-dir
                :lib lib
                :version version
                :basis basis
                :src-dirs ["src"]})
  (b/copy-dir {:src-dirs ["src" "resources"]
               :target-dir class-dir})
  (b/jar {:class-dir class-dir
          :jar-file jar-file}))

(defn install [_]
  ;; Assumes that you already have the jar file and friends
  (b/install {:basis basis
              :lib lib
              :version version
              :jar-file jar-file
              :class-dir class-dir}))

(defn uber [_]
  (clean nil)

  (b/copy-dir {:src-dirs   ["src" "resources"]         ; copy resources
               :target-dir class-dir})

  (b/compile-clj {:basis     basis               ; compile clojure code
                  :src-dirs  ["src"]
                  :class-dir class-dir})

  (b/uber {:class-dir class-dir                ; create uber file
           :uber-file uber-file
           :basis     basis
           :main      'pamela.cli})                ; here we specify the entry point for uberjar

  (println (format "Uber file created: \"%s\"" uber-file)))
