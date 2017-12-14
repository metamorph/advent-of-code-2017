(ns adv2017.day14
  (:require [adv2017.day10 :as day10]
            [clojure.set :as s]
            [clojure.string :as str]))

(def puzzle-input "oundnydw")
(def ^:dynamic *grid-size* 128)

(defn left-pad [l c s]
  (if (not= (count s) l)
    (str/replace (format (str "%" l "s") s) #"\s" c)
    s))

(defn hex->binary "Convert a hex string to binary" [s]
  (let [i (BigInteger. (str s) 16)
        b (Integer/toBinaryString i)]
    (left-pad 4 "0" b)))

(defn hexrow->binary [s]
  (apply str (map hex->binary s)))

(defn grid-rows [input]
  (->> (range 0 *grid-size*)
       (pmap #(format "%s-%d" input %))
       (pmap day10/solve-2)
       (pmap hexrow->binary)))

(defn row->used [r]
  (count (filter #(= \1 %) r)))

(defn solve-1 [input]
  (let [rows (grid-rows input)]
    (reduce + 0 (map row->used rows))))

(defn partition-succ
  "Partitions a seq of numbers into groups of consecutive numbers.
  (1 2 5 7 8 9) -> ((1 2) (5) (7 8 9))"
  [xs]
  (apply conj ;; Dont forget to add the last group to the result
         (reduce (fn [[groups [p & _ :as current]] n]
                   (if p
                     (if (= (inc p) n)
                       [groups (cons n current)]
                       [(conj groups current) (list n)])
                     [groups (cons n current)]))
                 [nil '()] xs)))

(defn count-regions [rows]
  (first (reduce
          (fn [[group-count prev-regions] xs]
            (let [regions (map set (partition-succ xs))]
              ;; Add all `prev-regions` that do not overlap with `regions`
              ;; to `group-count`.
              )
            )
          [0 '()] rows)))
