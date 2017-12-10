(ns adv2017.day10)

(def test-list
  [0 1 2 3 4])
(def test-input
  [3 4 1 5])

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

