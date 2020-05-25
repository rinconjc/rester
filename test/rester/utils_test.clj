(ns rester.utils-test
  (:require [clojure.test :refer :all]
            [rester.utils :refer :all]))

(deftest test-str->map
  (testing "simple pairs"
    (is (= {"a" "b" "c" "d"} (str->map "a:b,c : d" #"\s*:\s*"))))
  (testing "with missing values"
    (is (= {"a" "b"} (str->map "c,a:b" #"\s*:\s*")))))

(deftest test-cyclic?
  (testing "no cycles"
    (is (nil? (find-cycle {:a [:b :c] :b [:c]} :a)))
    (is (nil? (find-cycle {:a [:a]} :b))))
  (testing "cycles"
    (is (= #{:a} (find-cycle {:a [:a]} :a)))
    (is (= #{:b :d :f :a} (find-cycle {:a [:b :c] :b [:d] :d [:f] :f [:b]} :a)))))
