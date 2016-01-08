;; Copyright © 2016 Dynamic Object Language Labs Inc.
;;
;; This software is licensed under the terms of the
;; Apache License, Version 2.0 which can be found in
;; the file LICENSE at the root of this distribution.

(ns pamela.server
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [cljs-http.client :as http]
            [cljs.core.async :refer [<!]]))

(defn info-commit
  "As server for the last commit info, and, upon success
  call the provided callback function
  (update-info committer timestamp)."
  {:added "0.2.2"}
  [update-info]
  (let [location (.-location js/document)
        protocol (.-protocol location)
        hostname (.-hostname location)
        port (.-port location)
        server (str hostname (if (pos? (count port)) (str ":" port)))
        uri "/info/commit"
        url (str protocol "//" server uri)]
    ;; (println "get-info from" url "...")
    (go (let [response (<! (http/get url))
              {:keys [success status body]} response]
          (if (and success (= status 200))
            (let [{:keys [user timestamp]} body]
              (println "info-commit:" body)
              (update-info user timestamp))
            (println "get-info FAILED"))))))
