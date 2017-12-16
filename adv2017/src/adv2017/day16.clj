(ns adv2017.day16
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn spin [xs n]
  (let [[head tail] (split-at (- (count xs) n) xs)]
    (into [] (concat tail head))))

(defn exchange [xs a b]
  (let [av (get xs a)
        bv (get xs b)]
    (-> (assoc xs b av)
        (assoc a bv))))

(defn partner [xs v1 v2]
  (let [a (.indexOf xs v1)
        b (.indexOf xs v2)]
    (exchange xs a b)))
(defn partner-1 [xs v1 v2]
  (replace {v1 v2 v2 v1} xs))

(defn token->fn [s]
  (condp re-find s
    #"s(\d+)"           :>> #(let [n (Integer/parseInt (last %))] (fn [xs] (spin xs n)))
    #"x(\d+)/(\d+)"     :>> #(let [[_ a b] %
                                   a       (Integer/parseInt a)
                                   b       (Integer/parseInt b)] (fn [xs] (exchange xs a b)))
    #"p([a-p])/([a-p])" :>> #(let [[_ a b] %
                                   a       (first a)
                                   b       (first b)] (fn [xs] (partner-1 xs a b)))))

(defn input->tokens [input]
  (str/split input #","))

(defn read-input []
  (-> (io/resource "day16-input.txt")
      (slurp)
      (str/trim)))

(defn make-dance-line []
  (into [] (map char) (range (int \a) (inc (int \p)))))

(defn apply-dance [xs fns]
  (reduce (fn [xs f] (f xs)) xs fns))

(defn mem-apply-dance []
  (let [cached (atom {})]
    (fn [a b]
      (if-let [v (get @cached a)]
        v
        (let [v (apply-dance a b)]
          (swap! cached assoc a v)
          v)))))

(defn solve-1 []
  (let [xs  (make-dance-line)
        fns (->> (read-input)
                (input->tokens)
                (map token->fn))]
    (apply str (apply-dance xs fns))))

(defn solve-2 [iterations]
  (let [dance-fn (mem-apply-dance)](apply str
          (loop [it  0
                 xs  (make-dance-line)
                 fns (->> (read-input) (input->tokens) (map token->fn))]
            (if (= it iterations)
              xs
              (recur (inc it) (dance-fn xs fns) fns))))))

(defn -main [& args]
  (println (solve-2 1000)))
