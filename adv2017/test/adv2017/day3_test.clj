(ns adv2017.day3-test
  (:require [adv2017.day3 :refer :all]
            [clojure.test :refer :all]))

(deftest examples-test
  (testing "first-square"
    (is (= 0 (square->dist 1)))
    (is (= 3 (square->dist 12)))
    (is (= 2 (square->dist 23)))
    (is (= 31 (square->dist 1024)))))

