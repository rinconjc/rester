(ns rester.loader-test
  (:require [clojure.test :refer :all]
            [rester.loader :refer :all]
            [rester.utils :refer :all])
  (:import clojure.lang.ExceptionInfo))

(defn some-like [x xs]
  (some #(when (like x %) %) xs))

(deftest test-parse-options
  (testing "options"
    (is (= {:before "test1"} (parse-options "before = test1")))))

(deftest test-to-test-case
  (testing "convert to test case"
    (let [min-sample {:suite "suit1" :name "test1"
                      :post "https://api.example.com/" :expect {:status 200}}]
      (is (= :post (:verb (to-test-case min-sample))))
      (is (= :post (:verb (to-test-case (assoc min-sample
                                               :options {:before ["other"] :skip "prod"})))))
      (is (thrown? ExceptionInfo (to-test-case (dissoc min-sample :name))))
      (is (thrown? ExceptionInfo (to-test-case (dissoc min-sample :post)))))))

(deftest test-rows-to-test-case
  (testing "convert rows to test case"
    (let [test1 ["suite1" "test1" "http://api.example.com"
                       "GET" "Content-Type:application/json" "" "" "200" ""
                 "Content-Type:application/json" "" "id=$.id" ""]
          test2 ["" "test2" "http://api.example.com"
                 "POST" "Content-Type:application/json" "" "" "200" ""
                 "Content-Type:application/json" "ignore" "id=$.id" ""]]
      (is (like [{:verb :get :expect {:status 200}
                  :options {:extractors {"id" "$.id"}}}
                 {:suite "suite1" :verb :post :options {:ignore true}}]
                (rows->test-cases [test1 test2]))))))

(deftest test-process-tests
  (testing "loading from csv"
    (let [ts (load-tests-from "example/sample-tests.csv" nil)
          {:keys[runnable ignored skipped]} (process-tests ts {})]
      (is (= 10 (count ts)))
      (is (= 9 (count runnable)))
      (is (= 1 (count ignored)))
      (is (like {:id 2 :ignored true} (some-like {:id 2} ignored)))
      (is (like {:id 4 :deps #{3}} (some-like {:id 4} runnable)))
      (is (like {:id 5 :deps #{3 4}} (some-like {:id 5} runnable)))
      (is (like {:id 9 :deps #{8}} (some-like {:id 9} runnable))))))

(deftest tests-in-yaml-format
  (testing "loading from yaml"
    (let [ts (load-tests-from "example/sample-tests.yaml" nil)
          {:keys[runnable ignored skipped]} (process-tests ts {})]
      (is (= 4 (count ts)))
      (is (= 4 (count runnable))))))
