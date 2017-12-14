(ns adv2017.day14
  (:require [adv2017.day10 :as day10]
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

;; (defn row->indices [r]
;;   (map first
;;        (filter #(= (last %) \1)
;;                (map vector (iterate inc 0) r))))

;; (defn partition-consecutives
;;   "Partitions a seq of numbers into groups.
;;   (1 2 5 7 8 9) -> ((1 2) (5) (7 8 9))"
;;   [xs]
;;   (reduce
;;    (fn [] )
;;    xs))
