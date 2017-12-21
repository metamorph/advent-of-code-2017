(ns adv2017.day19
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn getv [{:keys [maze]} [x y]]
  (get (get maze y) x))

(defn find-start [maze]
  (let [x (.indexOf (get maze 0) \|)]
    [x 0]))

(defn parse-input [lines]
  (let [maze (mapv vec lines)]
    {:letters (list)
     :maze maze
     :direction [0 1]
     :position (find-start maze)}))

(defn test-input []
  (parse-input ["     |          "
                "     |  +--+    "
                "     A  |  C    "
                " F---|----E|--+ "
                "     |  |  |  D "
                "     +B-+  +--+ "]))

(defn find-direction [{:as state
                       :keys [direction]}
                      [x y :as position]]
  (cond
    (zero? x) ;; horizontal
    (if (= (getv (mapv + position [0 1])) ))

    (zero? y) ;; vertical
  )

(defn step [{:as                   state
             [x y :as position]    :position
             [dx dy :as direction] :direction}]
  (let [new-pos (mapv + position direction)
        v       (getv state new-pos)]
    (if (nil? v)
      nil ;; done - dead end.
      (->
       (cond
         (= v \+)
         (assoc state :direction (find-direction state new-pos))

         (re-matches #"[A-Z]" (str v))
         (update state :letters conj v)

         :else
         state)
       (assoc :position new-pos)))))

(defn puzzle-input []
  (->> (io/resource "day19-input.txt")
       (slurp)
       (str/split-lines)
       (parse-input)))


