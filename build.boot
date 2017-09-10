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

(def project 'dollabs/pamela)
(def version "0.6.2")
(def description "Probabalistic Advanced Modeling and Execution Learning Architecture (PAMELA)")
(def project-url "https://github.com/dollabs/pamela")
(def main 'pamela.cli)

(set-env!
  :source-paths #{"src" "test/clj"}
  :resource-paths #{"resources"}
  :dependencies   '[[org.clojure/clojure       "1.8.0"]
                    [org.clojure/data.codec    "0.1.0"]
                    [org.clojure/tools.cli     "0.3.5"]
                    [org.clojure/data.json     "0.2.6"]
                    ;; logging
                    [com.taoensso/timbre       "4.7.4"]
                    [org.slf4j/slf4j-api       "1.7.21"]
                    [com.fzakaria/slf4j-timbre "0.3.2"]
                    [org.clojure/tools.logging "0.3.1"]
                    ;; utilities
                    [environ                   "1.1.0"]
                    [instaparse                "1.4.7"]
                    [avenir                    "0.2.2"]
                    [me.raynes/fs              "1.4.6"]
                    [camel-snake-kebab         "0.4.0"]
                    [dollabs/plan-schema       "0.3.6"]
                    ;; testing
                    [adzerk/boot-test          "1.1.2" :scope "test"]
                    [criterium                 "0.4.4" :scope "test"]
                    ])

(require
  '[adzerk.boot-test :refer [test]])

(task-options!
  pom {:project     project
       :version     version
       :description description
       :url         project-url
       :scm         {:url project-url}
       :license     {"Apache-2.0" "http://opensource.org/licenses/Apache-2.0"}}
  aot {:namespace   #{main}}
  jar {:main        main}
  test {:namespaces #{'testing.pamela.cli 'testing.pamela.utils
                      'testing.pamela.parser 'testing.pamela.unparser
                      'testing.pamela.tpn 'testing.pamela.htn
                      'testing.pamela.plan-schema
                      }})

(deftask run
  "Run the project (build from source)."
  [A args ARG [str] "preface each app arg with -A"]
  (require [main :as 'app])
  (with-post-wrap [_]
    (apply (resolve 'app/-main) args)))

(deftask build-jar
  "Build the project locally as an uberjar."
  [d dir PATH #{str} "the set of directories to write to (target)."]
  (let [dir (if (seq dir) dir #{"target"})]
    (comp
      ;; do not include these files...
      (sift :include #{#"~$" #"\.xhtml$"  #"\.w3c$"} :invert true)
      (aot)
      (uber)
      (jar :file (str (name project) ".jar"))
      (target :dir dir))))

;; NOTE: Requires PAMELA_MODE=prod (or unset)
(deftask cli-test
  "Run the command line tests."
  []
  (let [cmd ["./bin/cli-test"]]
    (comp
      (build-jar)
      (with-post-wrap [_]
        (apply dosh cmd))))) ;; will throw exception on non-zero exit

(deftask all-tests
  "Run the Clojure and command line tests."
  []
  (comp
    (cli-test)                                              ;Call build-jar, which recreates target, which deletes target/gen-files.
    ; we need to preserve target/gen-files
    (test)
    ))

;; For Emacs if you Customize your Cider Boot Parameters to 'cider-boot'
;; then this task will be invoked upon M-x cider-jack-in-clojurescript
;; which is on C-c M-J
;; CIDER will then open two REPL buffers for you, one for CLJS
;; and another for CLJ. FFI:
;; https://cider.readthedocs.io/en/latest/up_and_running/#clojurescript-usage

;; This task is commented out here for users that have not copied
;; a profile.boot file to ~/.boot/ which defines the cider task:
;;
(deftask cider-boot
  "Cider boot params task"
  []
  ;; (cider))
  (comp
    (cider)
    (repl :server true)
    (wait)))
