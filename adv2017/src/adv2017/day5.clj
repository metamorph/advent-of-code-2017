(ns adv2017.day5
  (:require [clojure.java.io :as io]))

(defn take-a-step
  [[idx col] visit-fn]
  (let [v-at-pos (get col idx)
        new-idx  (+ v-at-pos idx)]
    [new-idx (update col idx visit-fn)]))

(defn steps-to-exit [input visit-fn]
  (loop [steps 1
         state [0 input]]
    (let [[idx col :as new-state] (take-a-step state visit-fn)]
      (if (or (neg? idx)
              (<= (count col) idx))
        steps
        (recur (inc steps) new-state)))))

(defn strange-visit-fn [offset]
  (if (<= 3 offset)
    (dec offset)
    (inc offset)))

(defn read-input []
  (->> (io/resource "day5-input.txt")
       (io/reader)
       (line-seq)
       (map #(Integer/parseInt %))
       (into [])))

(defn solve-1 []
  (time (steps-to-exit (read-input) inc)))
(defn solve-2 []
  (time (steps-to-exit (read-input) strange-visit-fn)))
