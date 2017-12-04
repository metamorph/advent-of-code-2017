(ns adv2017.day4
  (:require [clojure.java.io :refer [reader resource]]
            [clojure.string :refer [split]]))

(defn simple-checksum? [words]
  (= (count words)
     (count (set words))))

(defn solution-1 []
  (with-open [reader (reader (resource "day4-input.txt"))]
    (->> (line-seq reader)
        (filter #(simple-checksum? (split % #"\s+")))
        (count))))
