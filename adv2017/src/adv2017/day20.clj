(ns adv2017.day20
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn parse-particle [line]
  (if-let [[_ _ px py pz _ vx vy vz _ ax ay az]
           (re-find #"p=<(([-]*\d+),([-]*\d+),([-]*\d+))>, v=<(([-]*\d+),([-]*\d+),([-]*\d+))>, a=<(([-]*\d+),([-]*\d+),([-]*\d+))>" line)]
    {:position (map #(Integer/parseInt %) (list px py pz))
     :velocity (map #(Integer/parseInt %) (list vx vy vz))
     :acceleration (map #(Integer/parseInt %) (list ax ay az))}))

(defn parse-input
  "Parse particles. Return
  `{:id N
    :position [x y z]
    :velocity [x y z]
    :acceleration [x y x]}`"
  []
  (let [entries (->> (io/resource "day20-input.txt")
                     (slurp)
                     (str/split-lines)
                     (map parse-particle))]
    (map (fn [e i] (assoc e :id i))
         entries
         (iterate inc 0))))

(defn tick [{:as particle
             :keys [position velocity acceleration]}]
  (let [v (mapv #(+ %1 %2) velocity acceleration)
        p (mapv #(+ %1 %2) position v)]
    (-> particle
        (assoc :velocity v)
        (assoc :position p))))

(defn ticks [n particles]
  (if (zero? n)
    particles
    (recur (dec n) (mapv tick particles))))

(defn with-distance [{:as particle
                      :keys [position]}]
  (assoc particle :distance
         (apply + (map #(Math/abs %) position))))

(defn solve-1 [iterations]
  (->> (parse-input)
       (ticks iterations)
       (map with-distance)
       (sort-by :distance)
       (first)))

(defn without-collisions [particles]
  (let [groups (group-by :position particles)]
    (filterv
     (fn [{:keys [position]}]
       (= 1 (count (get groups position)))) particles)))

(defn solve-2 [iterations]
  (loop [n 0
         particles (parse-input)]
    (if (= n iterations)
      particles
      (recur (inc n) (without-collisions (mapv tick particles))))))





