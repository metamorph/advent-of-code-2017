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

(defn used-slots [row]
  (map last
       (filter #(= \1 (first %))
               (map vector row (iterate inc 0)))))

(defn partition-succ
  "Partitions a seq of numbers into groups of consecutive numbers.
  (1 2 5 7 8 9) -> ((1 2) (5) (7 8 9))."
  [xs]
  (if (seq xs)
    (apply conj ;; Dont forget to add the last group to the result
           (reduce (fn [[groups [p & _ :as current]] n]
                     (if p
                       (if (= (inc p) n)
                         [groups (cons n current)]
                         [(conj groups current) (list n)])
                       [groups (cons n current)]))
                   [nil '()] xs))
    xs))

(defn overlaps? [s1 s2]
  (seq (s/intersection s1 s2)))

(defn stray-sets
  [c1 c2]
  (if (seq c2)
    (filter (fn [s1]
              (every? (fn [s2] (not (overlaps? (set s1) (set s2)))) c2) ) c1)
    c1))

(defn count-regions [rows]
  (first
   (reduce
    (fn [[group-count prev-regions] xs]
      (let [regions (partition-succ xs)]
        (if (seq prev-regions)
          ;; Add all `prev-regions` that do not overlap with `regions`
          ;; to `group-count`.

          (let [closed-groups (stray-sets prev-regions regions)]
             [(+ group-count (count closed-groups)) regions])

          ;; Empty previous line
          [group-count regions])
        ))
    [0 '()]
    ;; An empty row to close the final line.
    (concat rows [[]]))))

(defn solve-2 [input]
  (let [rows (map used-slots (grid-rows input))]
    (count-regions rows)))

(defn test-regions []
  (let [rows '("1100100"
               "0001100"
               "0010000"
               "1100110"
               "0000000"
               "0001100"
               "0111000")]
    (count-regions (map used-slots rows))))
