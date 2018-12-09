(ns rester.core-test
  (:require [clojure.data.xml :refer [parse-str sexp-as-element]]
            [clojure.test :refer :all]
            [rester.core
             :refer
             [ diff* extract-data]])
  (:import java.text.SimpleDateFormat
           java.util.Date))

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
