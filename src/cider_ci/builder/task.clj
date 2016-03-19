; Copyright © 2013 - 2016 Dr. Thomas Schank <Thomas.Schank@AlgoCon.ch>
; Licensed under the terms of the GNU Affero General Public License v3.
; See the "LICENSE.txt" file provided with this software.

(ns cider-ci.builder.task
  (:require
    [cider-ci.builder.spec :as spec]
    [cider-ci.builder.util :as util]

    [cider-ci.utils.config :as config]
    [cider-ci.utils.duration :as duration]
    [cider-ci.utils.rdbms :as rdbms]

    [clojure.java.jdbc :as jdbc]

    [clojure.tools.logging :as logging]
    [logbug.debug :as debug]

  ))


(defn spec-map-to-array [spec-map]
  (map name
       (filter (complement nil?)
               (for [[k v] spec-map] (when v k)))))


;##############################################################################

(defn- check-aggregate-state [spec]
  (let [value (:aggregate-state spec)]
    (case value
      ("satisfy-last" "satisfy-any") []
      [(str "### Validation Error\n "
            "The value of `aggregate-state` must be either"
            "`satisfy-any` for `satisfy-last` but it is: `" value "`.")])))

(defn- validated-task-spec [spec]
  (loop [errors []
         pending-checks [check-aggregate-state]]
    (if-let [next-check (first pending-checks)]
      (recur (concat errors (apply next-check [spec]))
             (rest pending-checks))
      errors)))


;### normalize task-spec ######################################################

(defn- normalize-aggregate-succes [spec]
  (if (:aggregate-state spec)
    spec
    (assoc spec :aggregate-state "satisfy-any")))

(defn dispatch-storm-delay-default []
  (or (config/parse-config-duration-to-seconds
        :dispatch-storm-delay-default-duration)
      5))

(defn- normalize-dispatch-storm-delay [spec]
  (if-let [dispatch-storm-delay-duration (:dispatch-storm-delay-duration spec)]
    (-> spec (dissoc :dispatch-storm-delay-duration)
        (assoc :dispatch_storm_delay_seconds
               (-> dispatch-storm-delay-duration
                   duration/parse-string-to-seconds
                   Math/floor int)))
    (assoc spec :dispatch_storm_delay_seconds (dispatch-storm-delay-default))))

(defn- normalize-task-spec [raw-spec]
  (-> raw-spec
      clojure.walk/keywordize-keys
      (dissoc :job_id)
      normalize-aggregate-succes
      normalize-dispatch-storm-delay))


;##############################################################################

(defn create-db-task [raw-task-spec]
  (let [job-id (:job_id raw-task-spec)
        task-spec (normalize-task-spec raw-task-spec)
        db-task-spec (spec/get-or-create-task-spec task-spec)
        errors (validated-task-spec task-spec)
        task-row (conj (select-keys task-spec [:name :state :error :priority :dispatch_storm_delay_seconds])
                       {:job_id job-id
                        :traits (spec-map-to-array (or (:traits task-spec) {}))
                        :entity_errors errors
                        :exclusive_global_resources (spec-map-to-array (or (:exclusive-global-resources task-spec) {}))
                        :task_specification_id (:id db-task-spec)
                        :state (if (empty? errors) "pending" "aborted")
                        :id (util/idid2id job-id (:id db-task-spec))
                        })]
    (logging/debug task-row)
    (first (jdbc/insert! (rdbms/get-ds) "tasks" task-row))))


;### Debug ####################################################################
;(debug/debug-ns *ns*)
;(logging-config/set-logger! :level :debug)
;(logging-config/set-logger! :level :info)
