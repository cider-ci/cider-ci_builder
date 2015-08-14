; Copyright (C) 2013, 2014, 2015 Dr. Thomas Schank  (DrTom@schank.ch, Thomas.Schank@algocon.ch)
; Licensed under the terms of the GNU Affero General Public License v3.
; See the "LICENSE.txt" file provided with this software.

(ns cider-ci.builder.jobs.trigger
  (:require
    [cider-ci.builder.configfile :as configfile]
    [cider-ci.builder.jobs :as jobs]
    [cider-ci.builder.jobs.filter :as jobs.filter]
    [cider-ci.builder.jobs.tags :as tags]
    [cider-ci.builder.repository :as repository]
    [cider-ci.builder.spec :as spec]
    [cider-ci.builder.tasks :as tasks]
    [drtom.logbug.debug :as debug]
    [cider-ci.utils.http :as http]
    [cider-ci.utils.map :refer [deep-merge convert-to-array]]
    [cider-ci.utils.messaging :as messaging]
    [cider-ci.utils.rdbms :as rdbms]
    [drtom.logbug.catcher :as catcher]
    [clj-logging-config.log4j :as logging-config]
    [clj-yaml.core :as yaml]
    [clojure.java.jdbc :as jdbc]
    [clojure.tools.logging :as logging]
    [honeysql.format :as hsql-format]
    [honeysql.types :as hsql-types]
    [honeysql.helpers :as hsql-helpers]
    ))


;### include-exclude-filter ###################################################

(defprotocol Pattern
  (to-pattern [x]))

(extend-protocol Pattern
  java.lang.String
  (to-pattern [x] (re-pattern x))
  java.util.regex.Pattern
  (to-pattern [x] x)
  nil
  (to-pattern [x] nil)
  java.lang.Boolean
  (to-pattern [x] x))

(defn include-exclude-filter [include-match exclude-match coll]
  (->> coll
       (filter #(and include-match
                     (re-find (to-pattern include-match) (str %))))
       (filter #(or (not exclude-match)
                    (not (re-find (to-pattern exclude-match) (str %)))))))



;### trigger jobs #######################################################

(defn- job-trigger-fulfilled? [tree-id job trigger]
  (logging/debug 'job-trigger-fulfilled? [tree-id job trigger])
  (let [query (-> (-> (hsql-helpers/select true)
                      (hsql-helpers/from :jobs)
                      (hsql-helpers/merge-where [:= :tree_id tree-id])
                      (hsql-helpers/merge-where [:= :key (:job trigger)])
                      (hsql-helpers/merge-where [:in :state (:states trigger)])
                      (hsql-helpers/limit 1)) hsql-format/format) ]
    (logging/debug query)
    (->> query
         (jdbc/query (rdbms/get-ds))
         first
         boolean)))

(defn- branch-trigger-fulfilled? [tree-id job trigger]
  (let [query (-> (-> (hsql-helpers/select :name)
                      (hsql-helpers/from :branches)
                      (hsql-helpers/merge-join :commits [:= :branches.current_commit_id :commits.id])
                      (hsql-helpers/where [:= :commits.tree_id tree-id])) hsql-format/format)
        branch-names (->> query
                          (jdbc/query (rdbms/get-ds))
                          (map :name))]
    (->> branch-names
         (include-exclude-filter
           (:include-match trigger)
           (:exclude-match trigger))
         first
         boolean)))

(defn trigger-fulfilled? [tree-id job trigger]
  (case (:type trigger)
    "job" (job-trigger-fulfilled? tree-id job trigger)
    "branch" (branch-trigger-fulfilled? tree-id job trigger)
    (do (logging/warn "unhandled run-on" trigger) false)))

(defn some-job-trigger-fulfilled? [tree-id job]
  (let [triggers (:run-on job)]
    (if (= true triggers)
      true
      (some (fn [trigger]
              (trigger-fulfilled? tree-id job trigger)) triggers))))

(declare trigger-supermodules-jobs)
(defn- trigger-jobs [tree-id]
  (catcher/wrap-with-suppress-and-log-debug
    (->> (configfile/get-configfile tree-id)
         :jobs
         convert-to-array
         (filter #(-> % :run-on))
         (filter #(jobs.filter/dependencies-fulfilled? tree-id %))
         (filter #(some-job-trigger-fulfilled? tree-id %))
         (map #(jobs/create (assoc % :tree_id tree-id)))
         doall))
  (catcher/wrap-with-suppress-and-log-debug
    (trigger-supermodules-jobs tree-id))nil)

(defn- trigger-supermodules-jobs [tree-id]
  (->> (jdbc/query (rdbms/get-ds)
              ["SELECT DISTINCT supermodules_commits.tree_id FROM commits AS supermodules_commits
                JOIN submodules ON submodules.commit_id = supermodules_commits.id
                JOIN commits AS submodule_commits ON submodule_commits.id = submodules.submodule_commit_id
                WHERE submodule_commits.tree_id = ?" tree-id])
       (map :tree_id)
       (map trigger-jobs)
       doall) nil)



;### listen to branch updates #################################################

(defn- evaluate-branch-updated-message [msg]
  (catcher/wrap-with-log-warn
    (logging/debug 'evaluate-branch-updated-message {:msg msg})
    (-> (jdbc/query
          (rdbms/get-ds)
          ["SELECT tree_id FROM commits WHERE id = ? " (:current_commit_id msg)])
        first
        :tree_id
        trigger-jobs)))

(defn listen-to-branch-updates-and-fire-trigger-jobs []
  (messaging/listen "branch.updated" evaluate-branch-updated-message))

(defn listen-to-branch-updates-and-fire-trigger-jobs []
  (messaging/listen "branch.created" evaluate-branch-updated-message))



;### listen to job updates ##############################################

(defn evaluate-job-update [msg]
  (-> (jdbc/query
        (rdbms/get-ds)
        ["SELECT tree_id FROM jobs WHERE id = ? " (:id msg)])
      first
      :tree_id
      trigger-jobs))

(defn listen-to-job-updates-and-fire-trigger-jobs []
  (messaging/listen "job.updated"  evaluate-job-update))



;### initialize ###############################################################

(defn initialize []
  (listen-to-branch-updates-and-fire-trigger-jobs)
  (listen-to-job-updates-and-fire-trigger-jobs)
  )


;### Debug ####################################################################
;(debug/debug-ns 'cider-ci.utils.http)
;(logging-config/set-logger! :level :debug)
;(logging-config/set-logger! :level :info)
;(debug/debug-ns *ns*)
