(ns adv2017.day6-test
  (:require [adv2017.day6 :refer :all]
            [clojure.test :refer :all]))

(deftest examples
  (testing "minimal example"
    (is (= [[0 2 7 0]
            [2 4 1 2]
            [3 1 2 3]
            [0 2 3 4]
            [1 3 4 1]
            [2 4 1 2]]
           (take 6 (redistributions [0 2 7 0]))))))

