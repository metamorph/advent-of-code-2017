(ns adv2017.day6)

(defn redistribute
  "Redistribute the highest element in `col` by spreading
  it out (cycle) on top of other elements."
  [col]
  ;; Find the max-index
  (let [indexed (map vector (iterate inc 0) col)
        [start v] (apply max-key (cons last indexed))]
    (loop [col (assoc col start 0)
           idx (inc start)
           remaining v]
      (if (zero? remaining)
        col
        (recur (update col (mod idx (count col)) inc)
               (inc idx)
               (dec remaining))))))

(defn redistributions [init]
  (iterate redistribute init))


