(ns cider-ci.builder.jobs.validator.script.timeout
  (:require
    [cider-ci.builder.jobs.validator.job :refer [validate!]]
    [cider-ci.builder.jobs.normalizer :refer [normalize-job-spec]]

    [clojure.test :refer :all]
    [clj-yaml.core :as yaml]

    [clj-logging-config.log4j :as logging-config]
    [logbug.catcher :as catcher]
    [logbug.debug :as debug :refer [I> I>> identity-with-logging]]
    [clojure.tools.logging :as logging]
    )
  (:import
    [cider_ci.builder ValidationException]
    ))

(deftest test-validate!
  (testing "timeout"
    (testing "with valid argument passes"
      (let [valid-job-spec
            (->
                "
                key: job-key
                name: job-name
                tasks:
                  task1:
                    scripts:
                      script1:
                        timeout: 1 hour and 35 minutes
                "
                yaml/parse-string
                normalize-job-spec
                )]
        (is (validate! nil valid-job-spec))))

    (testing "with int type throws an ValidationException"
      (let [in-valid-job-spec
            (-> "
                key: job-key
                name: job-name
                tasks:
                  task1:
                    scripts:
                      script1:
                        timeout: 5
                "
                yaml/parse-string
                normalize-job-spec
                )]
        (is (thrown-with-msg?
              ValidationException #".*Type Mismatch.*"
              (validate! nil in-valid-job-spec)))))

    (testing "with a non valid duration string throws an ValidationException"
      (let [in-valid-job-spec
            (-> "
                key: job-key
                name: job-name
                tasks:
                  task1:
                    scripts:
                      script1:
                        timeout: 5 hourglasses
                "
                yaml/parse-string
                normalize-job-spec
                )]
        (is (thrown-with-msg?
              ValidationException #".*Invalid Duration String.*"
              (validate! nil in-valid-job-spec)))
      ))
    ))


;### Debug ####################################################################
;(logging-config/set-logger! :level :debug)
;(logging-config/set-logger! :level :info)
;(debug/wrap-with-log-debug #'create)
;(debug/debug-ns *ns*)
