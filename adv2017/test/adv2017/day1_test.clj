(ns adv2017.day1-test
  (:require [adv2017.day1 :refer :all]
            [clojure.test :refer :all]))

(deftest captcha-test
  (testing "test-1"
    (is (= 3 (captcha [1 1 2 2])))
    (is (= 4 (captcha [1 1 1 1])))
    (is (= 0 (captcha [1 2 3 4])))
    (is (= 9 (captcha [9 1 2 1 2 1 2 9])))))
