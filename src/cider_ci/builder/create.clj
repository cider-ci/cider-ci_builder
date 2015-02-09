; Copyright (C) 2013, 2014, 2015 Dr. Thomas Schank  (DrTom@schank.ch, Thomas.Schank@algocon.ch)
; Licensed under the terms of the GNU Affero General Public License v3.
; See the "LICENSE.txt" file provided with this software.

(ns cider-ci.builder.create
  (:require 
    [cider-ci.builder.repository :as repository]
    [cider-ci.builder.util :as util]
    [cider-ci.utils.debug :as debug]
    [cider-ci.utils.http :as http]
    [cider-ci.utils.messaging :as messaging]
    [cider-ci.utils.rdbms :as rdbms]
    [cider-ci.utils.with :as with]
    [clj-yaml.core :as yaml]
    [clojure.java.jdbc :as jdbc]
    [clojure.tools.logging :as logging]
    [honeysql.core :as hc]
    [honeysql.helpers :as hh]
    ))


(defn listen-to-branch-updates []
  (messaging/listen "branch.updated" 
                    (fn [msg] 
                      (logging/info msg))))
  

(defn initialize [&args])

(defn add-state-filter [query-atom name state]
  (reset! query-atom 
          (-> @query-atom
              (hh/merge-where 
                [" EXISTS " (-> (hh/select 1)
                                (hh/from :executions)
                                (hh/merge-where [:= :executions.name name])
                                (hh/merge-where [:= :executions.state state]))]))))

(defn add-branch-regex-filter [tree-id query-atom regex]
  (logging/debug add-branch-regex-filter [tree-id query-atom regex])
  (reset! query-atom
          (-> @query-atom
              (hh/merge-where 
                [" EXISTS "  (-> (hh/select 1)
                                 (hh/from :branches)
                                 (hh/merge-join :commits [:= :branches.current_commit_id :commits.id])
                                 (hh/merge-where [:= :commits.tree_id tree-id])
                                 (hh/merge-where [(keyword "~") :branches.name regex]))]))))


(defn add-dependency-filter [tree-id query-atom properties]
  (doseq [[other-name-sym state](->> properties :depends :executions)]
    (add-state-filter query-atom (name other-name-sym) state))
  (when-let [branch-regex (-> properties :depends :branch)]
    (add-branch-regex-filter tree-id query-atom branch-regex)))

(defn add-self-name-filter [query-atom name]
  (logging/debug add-self-name-filter [query-atom name])
  (reset! query-atom
          (-> @query-atom
              (hh/merge-where
                ["NOT EXISTS" (-> (hh/select 1)
                                  (hh/from :executions)
                                  (hh/where [:= :executions.name name]))]))))

(defn build-dependencies-fullfiled? [tree-id] 
  (fn [[self-name-sym properties]]
    (let [self-name (name self-name-sym)
          query-atom (atom (hh/select :true))]
      (logging/debug {:name self-name  :properties properties :initial-sql (hc/format @query-atom)})
      (add-self-name-filter query-atom self-name)
      (add-dependency-filter tree-id query-atom properties)
      (logging/debug {:final-sql (hc/format @query-atom)})
      (->> (-> @query-atom
               (hc/format))
           (jdbc/query (rdbms/get-ds))
           first 
           :bool))))

(defn add-branch-filter [tree-id query-atom conditions]
  (logging/info "add-branch-filter" [tree-id query-atom conditions])
  (when-let [branch-filter-str (:branch conditions)]
    (logging/info {:branch-filter-str branch-filter-str})
    (reset! query-atom
            (-> @query-atom
                (hh/merge-where 
                  [" EXISTS "  
                   (-> (if (re-matches #"^\^.*" branch-filter-str)
                         (hh/merge-where [(keyword "~") :branches.name branch-filter-str])
                         (hh/merge-where [:= :branches.name branch-filter-str]))
                       (hh/select 1)
                       (hh/from :branches)
                       (hh/merge-join :commits [:= :branches.current_commit_id :commits.id])
                       (hh/merge-where [:= :commits.tree_id tree-id])
                       )])))))

(defn build-trigger-constraints-fullfilled? [tree-id] 
  (fn [[self-name-sym properties]]
    (let [self-name (name self-name-sym)
          query-atom (atom (hh/select :true))]
      (logging/info "trigger-constraints-fullfilled?" {:name self-name  :properties properties :initial-sql (hc/format @query-atom)})
      (add-self-name-filter query-atom self-name)
      (add-branch-filter tree-id query-atom (-> properties :trigger))
      (logging/info "trigger-constraints-fullfilled?" {:final-sql (hc/format @query-atom)})
      (->> (-> @query-atom
               (hc/format))
           (jdbc/query (rdbms/get-ds))
           first 
           :bool))))

(defn trigger [tree-id]
  (->> (repository/get-path-content tree-id "/.cider-ci.yml")
       :executions
       (into [])
       (filter #(-> % second :trigger))
       (filter (build-dependencies-fullfiled? tree-id))
       (filter (build-trigger-constraints-fullfilled? tree-id))
       ))

(trigger "9678b18ef031f0ab219911a4594c526f7af8e2a7")
(trigger "cf3bcf353838e4af6ac0f7467cd0a2eee3502cdb")

(trigger "f2a6ecab55fad613b28bc105086e17988cf355e4")
