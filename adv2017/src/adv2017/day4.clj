(ns adv2017.day4
  (:require [adv2017.day2 :as day2]
            [clojure.java.io :refer [reader resource]]
            [clojure.string :refer [split]]))

(defn anagrams? [w1 w2]
  (= (frequencies w1) (frequencies w2)))

(defn simple-checksum? [words]
  (= (count words) (count (set words))))

(defn anagram-checksum? [words]
  (empty? (filter #(apply anagrams? %) (day2/combinations words))))

(defn count-valid [checksum-pred]
  (with-open [reader (reader (resource "day4-input.txt"))]
    (->> (line-seq reader)
         (filter #(checksum-pred (split % #"\s+")))
         (count))))

(defn prn-solutions []
  (println "Valid checksums, part 1:" (count-valid simple-checksum?))
  (println "Valid checksums, part 2:" (count-valid anagram-checksum?)))
