(ns adv2017.day13
  (:require [clojure.java.io :as io]))

(def test-fw [[0 3]
              [1 2]
              [4 4]
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
  [pos time r]
  (zero? (rem (+ pos time)
              (cycle-length r))))

(defn severity [l r] (* l r))

(defn trip-severity
  "Calculcate the severity for passing through the fw at pos 0."
  [fw]
  (let [hit-ranges (filter #(apply at-pos? 0 %) fw)]
    (reduce (fn [acc lr] (+ acc (apply severity lr)))
            0 hit-ranges)))

(defn with-delayed-entry
  "Move all firewall layers forward."
  [fw delay] (map (fn [[l r]] [(+ delay l) r]) fw))

(defn clear-passage?
  "Check if the passage is clear at pos 0 at a given delay."
  [fw delay]
  (->> (with-delayed-entry fw delay)
       (filter #(apply at-pos? 0 %))
       (empty?)))


(defn clear-passage-delay
  "Calculate the minimum delay needed to pass through the firewall
  undetected."
  [fw]
  (first (filter #(clear-passage? fw %) (iterate inc 0))))

