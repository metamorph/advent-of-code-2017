(ns adv2017.day11)

;; Thank you: https://www.redblobgames.com/grids/hexagons/
(defn move [[x y z :as pos] direction]
  (let [delta (case direction
                "n" [0 1 -1]
                "s" [0 -1 1]
                "ne" [1 0 -1]
                "se" [1 -1 0]
                "nw" [-1 1 0]
                "sw" [-1 0 1])]
    (map + pos delta)))

(defn dist [[x y z :as pos]]
  (max (Math/abs x) (Math/abs y) (Math/abs z)))

(defn dist-back [steps]
  (dist (reduce move [0 0 0] steps)))

(defn furthest-away [steps]
  (->> (reductions move [0 0 0] steps)
       (map dist)
       (apply max)))

(defn input-steps []
  (map clojure.string/trim (-> (clojure.java.io/resource "day11-input.txt")
       (slurp)
       (clojure.string/split #"\s*,\s*"))))
