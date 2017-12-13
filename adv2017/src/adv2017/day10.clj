(ns adv2017.day10
  (:require [clojure.string :as str]))

(def test-list
  [0 1 2 3 4])
(def test-input
  [3 4 1 5])

(def day10-marks (take 256 (iterate inc 0)))
(def day10-input [197, 97, 204, 108, 1, 29, 5, 71, 0, 50, 2, 255, 248, 78, 254, 63])

(def puzzle-input "197,97,204,108,1,29,5,71,0,50,2,255,248,78,254,63")
(def suffix '(17 31 73 47 23))
(defn str->ascii [s] (map int s))

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

(defn apply-knot-hash
  ([marks lens] (apply-knot-hash marks lens 0 (iterate inc 0)))
  ([marks lens start-index step-seq]
   (reduce (fn [[marks idx] [len skip]]
                    (map-step marks idx len skip))
                  [marks start-index]
                  (map vector lens step-seq))))

(defn sparse-hash [marks lens iterations]
  (loop [step-seq (iterate inc 0)
         idx 0
         marks marks
         pass 0]
    (if (= pass iterations)
      marks
      (let [[m i] (apply-knot-hash marks lens idx (take (count lens) step-seq))]
        (recur (drop (count lens) step-seq) i m (inc pass))))))

(defn dense-hash [sparse-hash]
  (map #(apply bit-xor %) (partition 16 sparse-hash)))

(defn byte->hex [b]
  (format "%02x" b))


(defn solve-2 [s]
  (let [lens (concat (str->ascii s) suffix)
        sparse (sparse-hash day10-marks lens 64)
        dense  (dense-hash sparse)]
    (str/join (map byte->hex dense))))
