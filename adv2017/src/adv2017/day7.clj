(ns adv2017.day7
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as s]))

(defn parse-line [l]
  (when-let [[_ n w tail] (re-find #"([a-z]+)\s+\((\d+)\)(.*)", l)]
    (if-let [[_ peers] (re-find #"\s+->\s+(.*)" tail)]
      [n (Integer/parseInt w) (set (map s/trim (s/split peers #",")))]
      [n (Integer/parseInt w) #{}])))

(defn to-sets-reducer [[full non-b]
                       [n _ peers]]
  [(conj full n) (sets/union non-b peers)])

(defn brute-force-bottom [entries]
  (let [[full-set non-bottom-set] (reduce to-sets-reducer [#{} #{}] entries)]
    (sets/difference full-set non-bottom-set)))

(defn read-input
  "Read data-file and return list where each entry is:
  `[name weight [neighbours]]`"
  []
  (->> (io/resource "day7-input.txt")
       (slurp)
       (s/split-lines)
       (map parse-line)))
