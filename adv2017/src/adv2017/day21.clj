(ns adv2017.day21)

;; Matrix operations

(defn flip
  "Flip a matrix."
  [m])

(defn rotations
  "Get the rotations of a matrix."
  [m])

(defn matrix-variations
  "Get all the variations of a matrix."
  [m])

(defn split-matrix
  "Split a matrix in smaller parts.
  Returns a new matrix where each 'cell' is
  a sub-matrix."
  [m])

(defn join-matrix
  "Take matrix-of-matrices, and join into one."
  [ms])

(defn enhance [m rules])

(defn process-matrix
  "Grow the `matrix` `iterations` times using the `rules`.
  Returns the final matrix."
  [iterations matrix rules]
  (if (zero? iterations)
    matrix
    (recur (dec iterations)
           (-> (split-matrix matrix)
               (enhance rules)
               (join-matrix))
           rules)))

(defn solve-1 [iterations matrix rules]
  (let [processed (process-matrix iterations matrix rules)
        on-filter (fn [c] (= c \#))]
    (count (filter on-filter (flatten processed)))))

