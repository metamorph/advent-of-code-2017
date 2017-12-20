(ns adv2017.day20-test
  (:require [adv2017.day20 :refer :all]
            [clojure.test :as t]))

(t/deftest what-abt-soe
  (t/testing "Trigger stack-overflow"
    (solve-1 2000)))
