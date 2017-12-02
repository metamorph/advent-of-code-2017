(ns adv2017.day2
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :refer [combinations]]
            [clojure.string :as str]))

(defn read-sheet
  "Read numbers as a vector of vectors (list of lines)"
  []
  (->> (io/resource "day2-input.txt")
       (io/reader)
       (line-seq)
       (map (fn [l] (map #(Integer/parseInt %) (str/split l #"\t"))))))

(defn minmax-checksum
  "Slow implementation"
  [xs] (Math/abs (- (apply min xs) (apply max xs))))

(defn evendiv-checksum [xs]
  (let [[a b] (first (filter
                      (fn [[a b]] (and (not= a b)
                                       (= 0 (rem (max a b) (min a b)))))
                      (combinations xs 2)))]
    (quot (max a b) (min a b))))

(defn spreadsheet-checksum [sheet checksum-fn]
  (reduce + 0 (map checksum-fn sheet)))

(defn checksum-1 []
  (spreadsheet-checksum (read-sheet) minmax-checksum))
(defn checksum-2 []
  (spreadsheet-checksum (read-sheet) evendiv-checksum))
