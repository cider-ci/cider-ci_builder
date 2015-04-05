; Copyright (C) 2013, 2014, 2015 Dr. Thomas Schank  (DrTom@schank.ch, Thomas.Schank@algocon.ch)
; Licensed under the terms of the GNU Affero General Public License v3.
; See the "LICENSE.txt" file provided with this software.

(ns cider-ci.builder.jobs.filter
  (:require 
    [cider-ci.builder.repository :as repository]
    [cider-ci.builder.jobs.tags :as tags]
    [cider-ci.builder.spec :as spec]
    [cider-ci.builder.tasks :as tasks]
    [cider-ci.builder.util :as util]
    [cider-ci.utils.debug :as debug]
    [cider-ci.utils.http :as http]
    [cider-ci.utils.messaging :as messaging]
    [cider-ci.utils.rdbms :as rdbms]
    [cider-ci.utils.with :as with]
    [cider-ci.utils.map :refer [deep-merge]]
    [clj-logging-config.log4j :as logging-config]
    [clj-yaml.core :as yaml]
    [clojure.java.jdbc :as jdbc]
    [clojure.tools.logging :as logging]
    [honeysql.core :as hc]
    [honeysql.helpers :as hh]
    ))


;### filter jobs ########################################################

(defn add-state-filter-to-query [query-atom name state tree-id]
  (reset! query-atom 
          (-> @query-atom
              (hh/merge-where 
                [" EXISTS " 
                 (-> (hh/select 1)
                     (hh/from :jobs)
                     (hh/merge-where [:= :jobs.name name])
                     (hh/merge-where [:= :jobs.state state])
                     (hh/merge-where [:= :jobs.tree_id tree-id]))]))))

(defn branch-filter-sql-part [conditions]
  (when-let [branch-filter-str (:branch conditions)]
    (if (re-matches #"^\^.*" branch-filter-str)
      (hh/merge-where [(keyword "~") :branches.name branch-filter-str])
      (hh/merge-where [:= :branches.name branch-filter-str]))))

(defn add-branch-filter-to-query [tree-id query-atom conditions]
  (logging/debug "add-branch-filter" [tree-id query-atom conditions])
  (when-let [where-condition (branch-filter-sql-part conditions)]
    (reset! 
      query-atom
      (-> @query-atom
          (hh/merge-where 
            [" EXISTS "  
             (-> where-condition 
                 (hh/select 1)
                 (hh/from :branches)
                 (hh/merge-join :commits [:= :branches.current_commit_id :commits.id])
                 (hh/merge-where [:= :commits.tree_id tree-id]))])))))

(defn add-self-name-filter-to-query [query-atom name tree-id]
  (logging/debug add-self-name-filter-to-query [query-atom name] tree-id)
  (reset! query-atom
          (-> @query-atom
              (hh/merge-where
                ["NOT EXISTS" 
                 (-> (hh/select 1)
                     (hh/from :jobs)
                     (hh/where [:= :jobs.name name])
                     (hh/merge-where [:= :jobs.tree_id tree-id]))]))))

(defn dependencies-fullfiled? [properties]
  (let [query-atom (atom (hh/select :true))
        tree-id (:tree_id properties)]
    (logging/debug {:properties properties :initial-sql (hc/format @query-atom)})
    (add-self-name-filter-to-query query-atom (:name properties) tree-id)
    (doseq [[other-name-sym {state :state}](->> properties :dependencies :jobs)]
      (add-state-filter-to-query query-atom (name other-name-sym) state tree-id))
    (add-branch-filter-to-query (:tree_id properties) query-atom (:dependencies properties))
    (logging/debug {:final-sql (hc/format @query-atom)})
    (->> (-> @query-atom
             (hc/format))
         (jdbc/query (rdbms/get-ds))
         first 
         :bool)))


;### Debug ####################################################################
;(logging-config/set-logger! :level :debug)
;(logging-config/set-logger! :level :info)
;(debug/debug-ns *ns*)
