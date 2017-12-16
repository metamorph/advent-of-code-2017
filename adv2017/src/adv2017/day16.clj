(ns adv2017.day16
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn spin [xs n]
  (let [[head tail] (split-at (- (count xs) n) xs)]
    (into [] (concat tail head))))

(defn exchange [xs a b]
  (-> (assoc xs b (get xs a))
      (assoc a (get xs b))))

(defn partner [xs v1 v2]
  (replace {v1 v2
            v2 v1} xs))

(defn token->fn [s]
  (condp re-find s
    #"s(\d+)"           :>> #(let [n (Integer/parseInt (last %))] (fn [xs] (spin xs n)))
    #"x(\d+)/(\d+)"     :>> #(let [[_ a b] %
                                   a       (Integer/parseInt a)
                                   b       (Integer/parseInt b)] (fn [xs] (exchange xs a b)))
    #"p([a-p])/([a-p])" :>> #(let [[_ a b] %
                                   a       (first a)
                                   b       (first b)] (fn [xs] (partner xs a b)))))

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

(defn memoized-apply-dance []
  (let [cache (atom {:called 0
                     :cached {}})]
    (fn [xs fns]
      (swap! cache update :called inc)
      (if-let [v (get-in @cache [:cached xs])]
        (do (println "Found match in cache. Call:" (:called @cache))
            v)
        (let [v (apply-dance xs fns)]
          (swap! cache assoc-in [:cached xs] (apply-dance xs fns))
          v)))))

(defn solve-1 []
  (let [xs  (make-dance-line)
        fns (->> (read-input)
                 (input->tokens)
                 (map token->fn))]
    (apply str (apply-dance xs fns))))

(defn solve-2 [iterations]
  (let [dance-fn (memoized-apply-dance)]
    (apply str
           (loop [it  0
                  xs  (make-dance-line)
                  fns (->> (read-input) (input->tokens) (map token->fn))]
             (if (= it iterations)
               xs
               (recur (inc it) (dance-fn xs fns) fns))))))

(defn -main [& args]
  (println (solve-2 1000000000)))
