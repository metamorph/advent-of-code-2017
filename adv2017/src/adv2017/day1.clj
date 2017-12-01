(ns adv2017.day1
  (:require [clojure.java.io :as io]))

(defn captcha
  "Calculate captcha. `ns` is a sequence of numbers,
  `dist` is the distance between two numbers to check for equality."
  ([ns] (captcha 1 ns))
  ([dist ns]
   (let [partitions (->> (cycle ns)
                         (take (+ dist (count ns)))
                         (partition (inc dist) 1))
         matches    (map first (filter #(= (first %1) (last %1)) partitions))]
     (reduce + 0 matches))))

(defn read-input
  "Read the data."
  []
  (->> (slurp (io/resource "day1-input.txt"))
       (clojure.string/trim)
       (seq)
       (map #(Integer/parseInt (str %)))))

(defn captcha-for-input-1 []
  (let [input (read-input)]
    (captcha input)))

(defn captcha-for-input-2 []
  (let [input (read-input)]
    (captcha (quot (count input) 2) input)))

