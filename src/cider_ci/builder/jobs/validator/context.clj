(ns cider-ci.builder.jobs.validator.context
  (:require
    [cider-ci.builder.jobs.validator.task :refer [validate-task!]]
    [cider-ci.builder.jobs.validator.script :refer [validate-script!]]

    [cider-ci.builder.jobs.validator.shared :refer :all]

    [cider-ci.utils.core :refer :all]
    [cider-ci.utils.rdbms :as rdbms]

    [clojure.java.jdbc :as jdbc]
    [clojure.set :refer :all]
    [clojure.data.json :as json]

    [clj-logging-config.log4j :as logging-config]
    [logbug.catcher :as catcher]
    [logbug.debug :as debug :refer [I> I>> identity-with-logging]]
    [clojure.tools.logging :as logging]
    )
  (:import
    [cider_ci.builder ValidationException]
    ))

(declare context-meta-spec)

(defn validate-context-type! [context chain]
  (when-not (map? context)
    (->> {:type "error"
          :description
          (str "The context " (format-chain chain)
               " must by a map, but it is " (type context))}
         (ValidationException. "Type Mismatch")
         throw)))

(defn validate-context! [context chain]
  (validate-context-type! context chain)
  (validate-accepted-keys! context context-meta-spec chain)
  (validate-values! context context-meta-spec chain))

(def context-meta-spec
  {
   :key {:validator validate-string!}
   :name {:validator validate-string!}
   :script_defaults {:validator validate-script!}
   :contexts {:validator (build-map-of-validator validate-context!)}
   :task_defaults {:validator validate-task!}
   :tasks {:validator (build-map-of-validator validate-task!)}
   })


;### Debug ####################################################################
;(logging-config/set-logger! :level :debug)
;(logging-config/set-logger! :level :info)
;(debug/wrap-with-log-debug #'create)
;(debug/debug-ns *ns*)
