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

(defn captcha-2
  "Another version where we joins two lists instead of using `partition`."
  ([ns] (captcha-2 1 ns))
  ([dist ns]
   (as-> ns $
     ;; Rotate the list `dist` number of elements and get a list with the same length as `ns`.
     (take (count $) (drop dist (cycle $)))
     ;; Join that list with the original (creates a list of pairs).
     (map vector ns $)
     ;; Select all pairs where both elements are the same.
     (filter #(= (first %1) (last %1)) $)
     ;; Select the first element of each pair (from the `ns` list)
     (map first $)
     ;; Calculate the sum of all of those values.
     (reduce + 0 $))))

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

