(ns adv2017.day1
  (:require [clojure.java.io :as io]))

(defn captcha [ns]
  (let [partitions (->> (cycle ns)
                        (take (inc (count ns)))
                        (partition 2 1))
        matches    (map first (filter (fn [[a b]] (= a b)) partitions))]
    (reduce + 0 matches)))

(defn captcha-for-input []
  (->> (slurp (io/resource "day1-input.txt"))
       (clojure.string/trim)
       (seq)
       (map #(Integer/parseInt (str %)))
       (captcha)))

