(ns rester.core-test
  (:require [clojure.data.xml :refer [indent-str sexp-as-element]]
            [clojure.test :refer :all]
            [rester.core :refer :all]
            [clojure.data.xml :refer [parse-str]]))

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

    (let [xml1 (parse-str "<soap12:Envelope xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><soap12:Body><QuerySurchargeResponse xmlns=\"https://api.sit.deft.com.au/translator\"><QuerySurchargeResult><![CDATA[<Response><ResponseCode>0</ResponseCode><CardType>MASTERCARD</CardType><SurchargeAmount>8</SurchargeAmount><SurchargePercentage>1.5</SurchargePercentage><SurchargeFixed></SurchargeFixed><DeclinedCode></DeclinedCode><DeclinedMessage></DeclinedMessage></Response>]]>                        </QuerySurchargeResult>                    </QuerySurchargeResponse>                </soap12:Body>            </soap12:Envelope>")
          xml2 (parse-str "<soap12:Envelope xmlns:soap12=\"http://www.w3.org/2003/05/soap-envelope\" xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"><soap12:Body><QuerySurchargeResponse xmlns=\"https://api.sit.deft.com.au/translator\"><QuerySurchargeResult><![CDATA[<Response><ResponseCode>0</ResponseCode><CardType>MASTERCARD</CardType><SurchargeAmount>8</SurchargeAmount><SurchargePercentage>1.5</SurchargePercentage><SurchargeFixed></SurchargeFixed><DeclinedCode> </DeclinedCode><DeclinedMessage></DeclinedMessage></Response>]]>                        </QuerySurchargeResult>                    </QuerySurchargeResponse>                </soap12:Body>            </soap12:Envelope>")]
      (is (nil? (diff* xml1 xml2)))))

  (testing "diff with regex"
    (is (nil? (diff* "#\\d+" "0.01")))))
