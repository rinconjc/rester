(ns rester.utils-test
  (:require [clojure.data.xml :refer [parse-str sexp-as-element]]
            [clojure.test :refer :all]
            [rester.utils
             :refer
             [cyclic?  parse-date-exp parse-options str->map]])
  (:import java.text.SimpleDateFormat
           java.util.Date))

(deftest test-str->map
  (testing "simple pairs"
    (is (= {"a" "b" "c" "d"} (str->map "a:b,c : d" #"\s*:\s*"))))
  (testing "with missing values"
    (is (= {"a" "b"} (str->map "c,a:b" #"\s*:\s*")))))

(deftest test-cyclic?
  (testing "no cycles"
    (is (nil? (cyclic? {:a [:b :c] :b [:c]} :a)))
    (is (nil? (cyclic? {:a [:a]} :b))))
  (testing "cycles"
    (is (= :a (cyclic? {:a [:a]} :a)))
    (is (= :b (cyclic? {:a [:b :c] :b [:d] :d [:f] :f [:b]} :a)))))

(deftest test-date-exps
  (testing "simple date names"
    (let [df (SimpleDateFormat. "yyyy-MM-dd")
          today (.format df (Date.))]
      (is (= today (parse-date-exp "now")))
      (is (= today (parse-date-exp "today")))
      (is (= (parse-date-exp "tomorrow") (parse-date-exp "today+1day")))
      (is (= (parse-date-exp "today") (parse-date-exp "today +2days -2days"))))))

(deftest test-parse-options
  (testing "options"
    (is (= {:before "test1"} (parse-options "before = test1")))))
