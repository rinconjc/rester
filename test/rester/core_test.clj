(ns rester.core-test
  (:require [clojure.data.xml :refer [parse-str sexp-as-element]]
            [clojure.test :refer :all]
            [rester.core
             :refer
             [diff* exec-tests extract-data mk-ordered-iter prepare-test]]
            [rester.utils :refer [like]]))

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
                                                (sexp-as-element [:a {:name "abc" :other "10" :more "zzz"} [:x] [:d]]))))

    (let [xml1 (sexp-as-element
                [:Envelope
                 [:Body
                  [:QuerySurchargeResponse
                   [:QuerySurchargeResult
                    "<Response><ResponseCode>0</ResponseCode><CardType>MASTERCARD</CardType><SurchargeAmount>8</SurchargeAmount><SurchargePercentage>1.5</SurchargePercentage><SurchargeFixed></SurchargeFixed><DeclinedCode></DeclinedCode><DeclinedMessage></DeclinedMessage></Response>                        "]]]])
          xml2 (parse-str "<soap12:Envelope xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><soap12:Body><QuerySurchargeResponse xmlns=\"https://x.y.z/translator\"><QuerySurchargeResult><![CDATA[<Response><ResponseCode>0 </ResponseCode><CardType>MASTERCARD</CardType><SurchargeAmount>8</SurchargeAmount><SurchargePercentage>1.5 </SurchargePercentage><SurchargeFixed></SurchargeFixed><DeclinedCode></DeclinedCode><DeclinedMessage></DeclinedMessage>\n                            </Response>]]>\n                        </QuerySurchargeResult>\n                    </QuerySurchargeResponse>\n                </soap12:Body>\n            </soap12:Envelope>\n
")]
      (is (nil? (diff* xml1 xml2)))))

  (testing "diff with regex"
    (is (nil? (diff* "#\\d+" "0.01")))))


(deftest test-extract-data
  (testing "simple json path"
    (is (= {"id" 100} (extract-data {:body {:id 100 "name" "blah"}} {"id" "$.id"})))))

(deftest test-ordered-iterator
  (testing "test ordered iterator"
    (let [test1 {:id 1 :name "test1" :verb :GET :url "https://api.example.com/"
                 :headers {"Content-Type" "application/json"} }
          test2 (assoc test1 :id 2 :name "Test2")
          test3 (assoc test1 :id 3 :name "test3" :deps #{1} :var-deps #{1})
          test4 (assoc test1 :id 4 :name "test4" :deps #{3 2} :var-deps #{3})
          test5 (assoc test1 :id 5 :name "test5" :deps #{4 1 2})
          it-fn (mk-ordered-iter [test1 test2 test3 test4 test5])]
      (is (= [[test1 test2] nil] (it-fn)))
      (is (empty? (first (it-fn (assoc test2 :success true)))))
      (is (= [test3] (first (it-fn (assoc test1 :success true)))))
      (is (like [[test5] [{:id 4 :skipped some?}]]
                (it-fn (assoc test3 :failed true)))))))

(deftest test-prepare-test
  (testing "prepare test for execution"
    (let [test1 {:id 1 :name "test1" :verb :POST :url "https://api.example.com/$path$"
                 :headers {"Content-Type" "application/json" "Authorization" "$accessToken$"}
                 :params {"search" "$name$"}
                 :body "{name:\"$name$\"}"
                 :expect {:status 200 :body "{status:\"OK\",id:\"$id$\"}"}}]
      (is (like {:body {:name "Pikachu"}
                 :headers {"Authorization" "1234"}
                 :url "https://api.example.com/test"
                 :params {"search" "Pikachu"}
                 :expect {:body {:status "OK" :id "100"}}}
                (prepare-test test1 {"name" "Pikachu" "accessToken" "1234" "path" "test" "id" 100}))))))

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
