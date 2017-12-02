(ns adv2017.day2-test
  (:require [adv2017.day2 :refer :all]
            [clojure.test :refer :all]))

(deftest checksum-tests
  (testing "examples"
    (is (= 18 (spreadsheet-checksum [[5 1 9 5]
                                     [7 5 3]
                                     [2 4 6 8]]
                                    minmax-checksum)))
    (is (= 9 (spreadsheet-checksum [[5 9 2 8]
                                    [9 4 7 3]
                                    [3 8 6 5]]
                                   evendiv-checksum)))))

