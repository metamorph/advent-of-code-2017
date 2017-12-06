(ns adv2017.day6)

(defn redistribute
  "Redistribute the highest element in `col` by spreading
  it out (cycle) on top of other elements."
  [col]
  ;; Find the max-index
  (let [maxv (apply max col)
        start (.indexOf col maxv)]
    (loop [col (assoc col start 0)
           idx (inc start)
           remaining maxv]
      (if (zero? remaining)
        col
        (recur (update col (mod idx (count col)) inc)
               (inc idx)
               (dec remaining))))))

(defn redistributions [init]
  (iterate redistribute init))

(def input-1 [4 10 4 1 8 4 9 14 5 1 14 15 0 15 3 5])

(defn solve-1 [input]
  (reduce (fn [acc col]
            (if (contains? acc col)
              (reduced [(count acc) col])
              (conj acc col)))
          #{}
          (redistributions input)))

(defn solve-2 [input]
  (let [redists (redistributions input)
        [idx v] (solve-1 input)
        col (drop (inc idx) redists)]
    (inc (count (take-while #(not= v %) col)))))

