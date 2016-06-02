(ns rester.core-test
  (:require [clojure.data.xml :refer [indent-str sexp-as-element]]
            [clojure.test :refer :all]
            [rester.core :refer :all]))

(deftest diff-test
  (testing "diff scalars"
    (is (= 1 (diff* 1 4)))
    (is (= "a" (diff* "a" "b")))
    (is (nil? (diff* "a" "a")))
    (is (nil? (diff* 4 4))))

  (testing "diff maps"
    (is (= {:a "b"} (diff* {:a "b" :c "d"} {:a "x" :c "d"})))
    (is (= {:a "b"} (diff* {:a "b" :c "d"} {:a "x" :c "d"})))
    (is (= {"a" ["b"]} (diff* {"a" ["b"] "c" ["e"]} {"c" ["d" "e"] "a" ["g"]})))
    (is (nil? (diff* {"a" "b" "c" "d"} {"c" "d" "a" "b"})))
    (is (= "{\"name\" \"abc\"}" (diff* "{\"name\" \"abc\"}" {"name" "abc"}))))

  (testing "diff xmls"
    (is (nil? (diff* (sexp-as-element [:a {:name "abc" :other "10"} [:b [:c]] [:d]])
                     (sexp-as-element [:a {:name "abc" :other "10" :more "zzz"} [:x] [:b [:c]] [:d]]))))
    (is (= [(sexp-as-element [:b [:c]])] (diff* (sexp-as-element [:a {:name "abc" :other "10"} [:b [:c]] [:d]])
                            (sexp-as-element [:a {:name "abc" :other "10" :more "zzz"} [:x] [:d]]))))))
