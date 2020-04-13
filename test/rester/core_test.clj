(ns rester.core-test
  (:require [clojure.test :refer :all]
            [rester.core :refer [exec-tests]]
            [rester.utils :refer [like]]))

(deftest test-test-execution
  (testing "test suite execution"
    (let [test1 {:suite "suite1" :id 1 :name "get mock" :verb :get
                 :url "http://www.mocky.io/v2/5c1d57e03100006c00fdd1c3"
                 :expect {:status 200 :body "{status:\"OK\"}"}}
          test2 (-> test1 (assoc :id 2) (assoc-in [:expect :status] 201))]
      (is (empty? (exec-tests [] {})))
      (is (like [{:success true}] (exec-tests [test1] {})))
      (is (like [{:failure some?}] (exec-tests [test2] {})))
      (is (like [{:id 1 :success true} {:failure some?}]
                (exec-tests [(dissoc test1 :success) (dissoc test2 :failure)] {:concurrency 1}))))))
