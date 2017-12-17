(ns adv2017.day17)

(def puzzle-input 367)

(defn insert-at [xs idx v]
  (let [[head tail] (split-at idx xs)]
    (into [] (concat head (vector v) tail))))

(defn step [xs idx len]
  (mod (+ idx len) (count xs)))

(defn solve-1 [step-size iterations]
  (let [[xs idx]
        (loop [it 0
               v 1
               xs [0]
               idx 0]
          (if (= it iterations)
            [xs idx]
            (let [next-idx (inc (step xs idx step-size))]
              (recur (inc it) (inc v) (insert-at xs next-idx v) next-idx))))]

    (println "Value after 2017:" (get xs (inc idx)))))
