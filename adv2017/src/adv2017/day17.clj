(ns adv2017.day17)

(def puzzle-input 367)

(defn insert-at [xs idx v]
  (let [[head tail] (split-at idx xs)]
    (into [] (concat head (vector v) tail))))

(defn step [size idx len]
  (mod (+ idx len) size))

(defn solve-1 [step-size iterations]
  (let [[xs idx]
        (loop [it 0
               v 1
               xs [0]
               idx 0]
          (if (= it iterations)
            [xs idx]
            (let [next-idx (inc (step (count xs) idx step-size))]
              (recur (inc it) (inc v) (insert-at xs next-idx v) next-idx))))]
    (get xs (inc idx))))

(defn solve-2
  "Skip mutating a list, simply iterate over a growing length, and record
  indexes where values will be inserted. Store the last value inserted at position `1`.
  After all iterations this will be our value (since we're never inserting at position `0`.)"
  [step-size iterations]
  (loop [value-at-1 nil
         it 0
         v 1
         len 1
         idx 0]
    (if (= it iterations)
      value-at-1
      (let [next-idx (inc (step len idx step-size))]
        (recur (if (= 1 next-idx) v value-at-1)
               (inc it)
               (inc v)
               (inc len)
                next-idx)))))


