; Copyright (C) 2013, 2014, 2015 Dr. Thomas Schank  (DrTom@schank.ch, Thomas.Schank@algocon.ch)
; Licensed under the terms of the GNU Affero General Public License v3.
; See the "LICENSE.txt" file provided with this software.

(ns cider-ci.builder.main
  (:require 
    [cider-ci.auth.core :as auth]
    [cider-ci.builder.repository :as repository]
    [cider-ci.builder.tasks :as tasks]
    [cider-ci.builder.web :as web]
    [cider-ci.utils.config-loader :as config-loader]
    [cider-ci.utils.debug :as debug]
    [cider-ci.utils.messaging :as messaging]
    [cider-ci.utils.nrepl :as nrepl]
    [cider-ci.utils.rdbms :as rdbms]
    [cider-ci.utils.map :refer [deep-merge]]
    [clojure.java.jdbc :as jdbc]
    [clojure.tools.logging :as logging]
    ))


(defonce conf (atom {}))
(defonce rdbms-ds (atom {}))

(defn read-config []
  (config-loader/read-and-merge
    conf ["conf_default.yml" 
          "conf.yml"])
  @conf)


(defn get-db-spec []
  (deep-merge 
    (or (-> @conf :database ) {} )
    (or (-> @conf :services :builder :database ) {} )))

(defn -main [& args]
  (read-config)
  (nrepl/initialize (-> @conf :services :builder :nrepl))
  (rdbms/initialize (get-db-spec))
  (messaging/initialize (:messaging @conf))
  (tasks/initialize)
  (auth/initialize (select-keys @conf [:session :basic_auth]))
  (web/initialize (-> @conf :services :builder :http))
  (repository/initialize {:basic_auth (:basic_auth @conf)
                          :http (-> @conf :services :repository :http)})
  @conf)


;### Debug ####################################################################
;(logging-config/set-logger! :level :debug)
;(logging-config/set-logger! :level :info)
;(debug/debug-ns *ns*)
