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
    :right (map + coord [1 0])
    :up    (map + coord [0 1])
    :left  (map + coord [-1 0])
    :down  (map + coord [0 -1])))

(defn coordinates
  "Get an infinite list of coordinates by moving in a counter-clockwise circle."
  []
  (reductions move [0 0] (steps-seq)))

(defn square->dist
  "Calculate the distance to square 1 from `square`."
  [square]
  (let [coords (coordinates)
        [x y]  (first (drop (dec square) coords))]
    (+ (Math/abs x) (Math/abs y))))

(defn neighbours
  "Get the neighbours (as seq of `[x y]` coordinates)
  of `coord`."
  [[x y :as coord]]
  (let [diffs (for [x     [-1 0 1]
                    y     [-1 0 1]
                    :when (not= x y 0)] [x y])]
    (map #(map + coord %) diffs)))

(defn next-state
  "Given a state `{[x y] v1 ...}` and an `[x y]` coordinate -
  calculate the new state with `xy` included."
  [state xy]
  (let [adjecent (neighbours xy)
        value    (reduce + 0 (map (fn [xy] (get state xy 0)) adjecent))]
    (assoc state xy value)))

(defn grid-values
  "Values in the crid (calculated by summing values from neighbours)"
  []
  (let [coords      (coordinates)
        incr-states (reductions next-state
                                {[0 0] 1}
                                (drop 1 (coordinates)))]
    (map #(get %1 %2 0) incr-states coords)))

(defn results
  "Print the results."
  []
  (time
   (let [input      277678
         solution-1 (square->dist input)
         solution-2 (first (drop-while #(< % input) (grid-values)))]
     (println "Distance for" input ":" solution-1)
     (println "Value larger than" solution-1 "in adjecent-values grid:" solution-2))))
