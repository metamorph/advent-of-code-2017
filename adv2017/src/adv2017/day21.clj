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

(defn make-rule
  "Read a spec ('pattern to match' -> 'stuff to replace with')
  and return a pair of matrices [pattern replacement]."
  [spec]
  (let [[_ p r] (re-find #"(.*)\s+=>\s+(.*)" spec)
        plines (clojure.string/split p #"/")
        rlines (clojure.string/split r #"/")]
    [(mapv vec plines) (mapv vec rlines)]))

(defn read-rules
  "Creates a map of rules (key: pattern, value: replacement)."
  [lines] (into {} (map make-rule lines)))

(def initial-pattern [(mapv vec ".#.")
                       (mapv vec "..#")
                       (mapv vec "###")])


;; (defn solve-1 [iterations matrix rules]
;;   (let [processed (process-matrix iterations matrix rules)
;;         on-filter (fn [c] (= c \#))]
;;     (count (filter on-filter (flatten processed)))))

