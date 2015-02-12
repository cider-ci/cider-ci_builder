; Copyright (C) 2013, 2014, 2015 Dr. Thomas Schank  (DrTom@schank.ch, Thomas.Schank@algocon.ch)
; Licensed under the terms of the GNU Affero General Public License v3.
; See the "LICENSE.txt" file provided with this software.

(ns cider-ci.builder.executions
  (:require 
    [cider-ci.builder.repository :as repository]
    [cider-ci.builder.executions.tags :as tags]
    [cider-ci.builder.spec :as spec]
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


;### create execution #########################################################



(defn add-specification-id [params]
  (logging/info add-specification-id [params])
  (assoc params :specification_id 
         (-> params
             :specification
             spec/get-or-create-execution-specification 
             :id)))

(defn create [params]
  (logging/info create [params])
  (->> (jdbc/insert! 
         (rdbms/get-ds)
         :executions
         (select-keys params 
                      [:tree_id, :specification_id, 
                       :name, :description, :priority]))
       first
       (conj params)
       tags/add-execution-tags
       ))


;### filter executions ########################################################

(defn add-state-filter-to-query [query-atom name state]
  (reset! query-atom 
          (-> @query-atom
              (hh/merge-where 
                [" EXISTS " 
                 (-> (hh/select 1)
                     (hh/from :executions)
                     (hh/merge-where [:= :executions.name name])
                     (hh/merge-where [:= :executions.state state]))]))))

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

(defn add-self-name-filter-to-query [query-atom name]
  (logging/debug add-self-name-filter-to-query [query-atom name])
  (reset! query-atom
          (-> @query-atom
              (hh/merge-where
                ["NOT EXISTS" 
                 (-> (hh/select 1)
                     (hh/from :executions)
                     (hh/where [:= :executions.name name]))]))))

(defn dependencies-fullfiled? [properties]
  (let [query-atom (atom (hh/select :true))]
    (logging/debug {:properties properties :initial-sql (hc/format @query-atom)})
    (add-self-name-filter-to-query query-atom (:name properties))
    (doseq [[other-name-sym state](->> properties :depends :executions)]
      (add-state-filter-to-query query-atom (name other-name-sym) state))
    (add-branch-filter-to-query (:tree_id properties) query-atom (:depends properties))
    (logging/debug {:final-sql (hc/format @query-atom)})
    (->> (-> @query-atom
             (hc/format))
         (jdbc/query (rdbms/get-ds))
         first 
         :bool)))



;### available executions #####################################################

(defn available-executions [tree-id]
  (->> (repository/get-path-content tree-id "/.cider-ci.yml")
       :executions
       (into [])
       (map (fn [[name_sym properties]] (assoc properties 
                                               :name (name name_sym)
                                               :tree_id tree-id)))
       (filter dependencies-fullfiled?)
       ))


;### trigger executions #######################################################

(defn listen-to-branch-updates []
  (messaging/listen "branch.updated" 
                    (fn [msg] 
                      (logging/info msg))))

(defn trigger-constraints-fullfilled? [properties] 
    (let [query-atom (atom (hh/select :true))]
      (logging/debug "trigger-constraints-fullfilled?" {:properties properties :initial-sql (hc/format @query-atom)})
      (add-self-name-filter-to-query query-atom (:name properties))
      (add-branch-filter-to-query (:tree_id properties) query-atom (-> properties :trigger))
      (logging/debug "trigger-constraints-fullfilled?" {:final-sql (hc/format @query-atom)})
      (->> (-> @query-atom
               (hc/format))
           (jdbc/query (rdbms/get-ds))
           first 
           :bool)))
  
(defn trigger-executions [tree-id]
  (->> (repository/get-path-content tree-id "/.cider-ci.yml")
       :executions
       (into [])
       (map (fn [[name_sym properties]] (assoc properties 
                                               :name (name name_sym)
                                               :tree_id tree-id)))
       (filter #(-> % :trigger))
       (filter dependencies-fullfiled?)
       (filter trigger-constraints-fullfilled?)
       (map add-specification-id)
       (map create)
       ))


;(trigger-executions "6ead70379661922505b6c8c3b0acfce93f79fe3e")

;(available-executions "6ead70379661922505b6c8c3b0acfce93f79fe3e")

;### Debug ####################################################################
;(debug/debug-ns *ns*)
;(debug/debug-ns 'cider-ci.utils.http)
;(logging-config/set-logger! :level :debug)
;(logging-config/set-logger! :level :info)
