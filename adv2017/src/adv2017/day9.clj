(ns adv2017.day9
  (:require [clojure.java.io :as io]))

(defn make-drop-!-xf
  "Returns a transducer that will drop any `!` and a
  following char." []
  (let [drop-next? (atom false)]
    (filter
     (fn [c] (if @drop-next?
               (do (reset! drop-next? false)
                   false)
               (if (= \! c)
                 (do
                   (reset! drop-next? true)
                   false)
                 true))))))

(defn make-drop-garbage-xf
  "Returns a transducer that will drop all garbage from a stream
  of chars." []
  (let [drop-next? (atom false)]
    (filter
     (fn [c]
       (cond
         (= \< c) (do (reset! drop-next? true)
                      false)

         (= \> c) (do (reset! drop-next? false)
                      false)
         :else    (not @drop-next?))))))

(defn group-score [chars]
  (time
   (let [cleaned-up (eduction (comp (make-drop-!-xf)
                                    (make-drop-garbage-xf)) chars)]
     (last
      (reduce (fn [[opened sum :as state] c]
                (case c
                  \{ [(inc opened) sum]
                  \} [(dec opened) (+ sum opened)]
                  state))
              [0 0]
              cleaned-up)))))

(defn count-garbage [chars]
  (time
   (let [cleaned-up (eduction (make-drop-!-xf) chars)]
     (last
      (reduce (fn [[count? sum :as state] c]
                (cond
                  (and (not count?) (= \< c)) [true sum]
                  (= \> c)                    [false sum]
                  count?                      [count? (inc sum)]
                  :else                       state))
              [false 0]
              cleaned-up)))))

(defn read-input []
  (->> (io/resource "day9-input.txt")
       (slurp)))
