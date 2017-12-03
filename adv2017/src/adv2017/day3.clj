(ns adv2017.day3)

(defn dist-seq
  "Lazy seq of number of steps to take.
  `'(1 1 2 2 3 3 4 4 5 5 6 6 ..)`"
  []
  (mapcat #(repeat 2 %) (iterate inc 1)))

(defn steps-seq
  "Lazy seq of steps to take.
  `'(:right :up :left :left :down :down :right ...)`"
  []
  (mapcat #(repeat %1 %2)
          ;; all the distances to move
          (dist-seq)
          ;; directions (counter-clockwise)
          (cycle [:right :up :left :down])))

(defn move
  "Take one step from `coord` in `direction`.
  Return a new coord `[x y]`."
  [[x y :as coord] direction]
  (case direction
    :right (map + coord [0 1])
    :up    (map + coord [1 0])
    :left  (map + coord [0 -1])
    :down  (map + coord [-1 0])))

(defn square->dist
  "Calculate the distance to square 1 from `square`."
  [square]
  (time
   (let [coords (reductions move [0 0] (steps-seq))
         [x y]  (first (drop (dec square) coords))]
     (+ (Math/abs x) (Math/abs y)))))

(defn results []
  (let [dist 277678]
    (println "Distance for" dist ":" (square->dist dist))))
