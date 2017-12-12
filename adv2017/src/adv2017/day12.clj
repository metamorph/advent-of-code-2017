(ns adv2017.day12
  (:require [clojure.java.io :as io]
            [clojure.set :as s]
            [clojure.string :as str]))

(def test-input {0 '(2)
                 1 '(1)
                 2 '(0 3 4)
                 3 '(2 4)
                 4 '(2 3 6)
                 5 '(6)
                 6 '(4 5)})

(defn reduce-line
  "Add a line to `m`."
  [m l]
  (if-let [[_ n refs] (re-find #"(\d+)\s+<->\s+(.*)" l)]
    (assoc m
           (Integer/parseInt n)
           (map #(Integer/parseInt (str/trim %)) (str/split refs #",")))
    m))

(defn read-input []
  (->> (io/resource "day12-input.txt")
       (io/reader)
       (line-seq)
       (reduce reduce-line {})))

(defn numbers-in-group
  "Get the numbers in a group."
  ([seen state n]
   (let [xs (filter (complement seen) (get state n '()))]
     (cons n (mapcat #(numbers-in-group (set (concat seen xs [n])) state %) xs))))
  ([state n] (numbers-in-group #{} state n)))

(defn groups
  "Count the number of distinct groups."
  [state]
  (loop [group-count  0
         state        state
         [pid & tail] (keys state)]
    (if pid
      (let [group (numbers-in-group state pid)]
        (recur (inc group-count)
               state
               (filter (complement (set group)) tail)))
      ;; else
      group-count)))

