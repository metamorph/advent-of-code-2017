(ns adv2017.day13
  (:require [clojure.java.io :as io]))

(def test-fw [[0 3]
              [1 4]
              [4 6]
              [6 4]])

(defn line->vec [l]
  (let [[_ l r] (re-find #"(\d+):\s+(\d+)" l)]
    [(Integer/parseInt l) (Integer/parseInt r)]))

(defn read-input []
  (->> (io/reader (io/resource "day13-input.txt"))
       (line-seq)
       (map line->vec)))

(defn cycle-length
  "Get the length of a cycle for a given `range`.
  The cycle-length is the number of picoseconds between
  passing the same distance."
  [range] (* 2 (dec range)))

(defn at-pos?
  "Check if a sensor with range (`r`) is at the position
  `pos` at a given `time`."
  [time pos r]
  (zero? (rem (+ pos time) (cycle-length r))))

(defn severity [l r] (* l r))

(defn trip-severity [fw]
  (let [hit-ranges (filter #(apply at-pos? 0 %) fw)]
    (reduce (fn [acc lr] (+ acc (apply severity lr)))
            0 hit-ranges)))

