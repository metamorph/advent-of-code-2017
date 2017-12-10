(ns adv2017.day10)

(def test-list
  [0 1 2 3 4])
(def test-input
  [3 4 1 5])

(def day10-marks (take 256 (iterate inc 0)))
(def day10-input [197, 97, 204, 108, 1, 29, 5, 71, 0, 50, 2, 255, 248, 78, 254, 63])

(defn sub-list
  [v idx len]
  {:pre [(>= (count v) len)]}
  (take len (drop idx (cycle v))))

(defn insert-list
  [v l idx]
  (let [[to-ins head?] (split-at (- (count v) idx) l)
        result             (concat (take idx v)
                                   to-ins
                                   (drop (+ idx (count to-ins)) v))]
    (if (seq head?)
      (insert-list result head? 0)
      result)))

(defn map-step [marks idx len skip]
  (let [sub (reverse (sub-list marks idx len))
        result (insert-list marks sub idx)
        next-idx (mod (+ idx len skip) (count marks))]
    [result next-idx]))

(defn apply-knot-hash [marks lens]
  (reduce (fn [[marks idx] [len skip]]
            (map-step marks idx len skip))
          [marks 0]
          (map vector lens (iterate inc 0))))
