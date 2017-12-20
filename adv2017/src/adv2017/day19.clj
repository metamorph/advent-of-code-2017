(ns adv2017.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn parse-input [lines]
  (mapv vec lines))

(defn test-input []
  (parse-input ["     |          "
                "     |  +--+    "
                "     A  |  C    "
                " F---|----E|--+ "
                "     |  |  |  D "
                "     +B-+  +--+ "]))

(defn getv [map [x y]]
  (get (get map y) x))

(defn neighbours [[x y :as pos]]
  (map #(mapv + pos %) [[-1 0]
                        [1 0]
                        [0 -1]
                        [0 1]]))

(defn find-start [map]
  (let [x (.indexOf (get map 0) \|)]
    [x 0]))

(defn puzzle-input []
  (->> (io/resource "day19-input.txt")
       (slurp)
       (str/split-lines)
       (parse-input)))

(defn solve-1 [input]
  (let [[x y :as start] (find-start input)]
    start))

