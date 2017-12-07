(ns adv2017.day7
  (:require [clojure.java.io :as io]
            [clojure.set :as sets]
            [clojure.string :as s]
            [clojure.set :as set]))

(defn parse-line [l]
  (when-let [[_ n w tail] (re-find #"([a-z]+)\s+\((\d+)\)(.*)", l)]
    (if-let [[_ peers] (re-find #"\s+->\s+(.*)" tail)]
      [n (Integer/parseInt w) (map s/trim (s/split peers #","))]
      [n (Integer/parseInt w) []])))

(defn read-input []
  (->> (io/resource "day7-input.txt")
       (slurp)
       (s/split-lines)
       (map parse-line)
       (reduce #(let [[n w c] %2] (assoc %1 n {:weight w
                                               :children c})) {})))

(defn test-input []
  (let [data
        "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)"]
    (->> data
         (s/split-lines)
         (map parse-line)
         (reduce #(let [[n w c] %2] (assoc %1 n {:weight w
                                                 :children c})) {}))))

(defn find-root [entries]
  (let [full-set (set (keys entries))
        child-set (reduce #(set/union %1 (:children %2)) #{}
                          (vals entries))]
    (first (set/difference full-set child-set))))

(defn weight-for [tree n]
  (let [w (get-in tree [n :weight])
        children (get-in tree [n :children])]
    (apply + w (map #(weight-for tree %) children))))

(defn weights-from [tree node]
  (let [{:keys [children]} (get tree node)
        weights (map (fn [c] [c (weight-for tree c)])
                     children)]
    weights))




