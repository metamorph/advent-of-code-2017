(ns adv2017.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-sheet
  "Read numbers as a vector of vectors (list of lines)"
  []
  (->> (io/resource "day2-input.txt")
       (io/reader)
       (line-seq)
       (map (fn [l] (map #(Integer/parseInt %) (str/split l #"\t"))))))

(defn minmax-checksum-slow
  "Slow implementation"
  [xs] (Math/abs (- (apply min xs) (apply max xs))))

(defn minmax-checksum
  [xs]
  (let [[min max] (reduce
                   (fn [[min max] n]
                     [(if (< n min) n min)
                      (if (> n max) n max)])
                   [Integer/MAX_VALUE Integer/MIN_VALUE] xs)]
    (- max min)))

(defn combinations [xs]
  (if (seq xs)
    (concat
     (map vector (repeat (first xs)) (rest xs))
     (combinations (rest xs)))
    []))

(defn evendiv-checksum [xs]
  (let [[a b] (first (filter
                      (fn [[a b]] (= 0 (rem (max a b) (min a b))))
                      (combinations xs)))]
    (quot (max a b) (min a b))))

(defn spreadsheet-checksum [sheet checksum-fn]
  (reduce + 0 (map checksum-fn sheet)))

(defn checksum-1 []
  (time (spreadsheet-checksum (read-sheet) minmax-checksum)))
(defn checksum-2 []
  (time (spreadsheet-checksum (read-sheet) evendiv-checksum)))
