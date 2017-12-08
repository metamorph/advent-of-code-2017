(ns adv2017.day8
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(def test-lines
  ["b inc 5 if a > 1"
   "a inc 1 if b < 5"
   "c dec -10 if a >= 1"
   "c inc -20 if c == 10"])

(defn input-lines
  "Read input file as lines."
  []
  (->> (io/resource "day8-input.txt")
       (slurp)
       (s/split-lines)))

(defn parse-instr
  "TODO: Maybe try antlr instead, just for fun."
  [l]
  (if-let [[_ reg op v pred-reg pred-op pred-v]
           (re-find #"(\w+)\s+(\w+)\s+([-]*\d+)\s+if\s+(\w+)\s+(\W+)\s+([-]*\d+)" l)]
    {:reg   reg
     :op    op
     :value (Integer/parseInt v)
     :pred  {:reg   pred-reg
             :op    pred-op
             :value (Integer/parseInt pred-v)}}))

(defn make-pred
  "Creates a predicate that checks if the registry is in a specific state"
  [{:keys [reg op value]}]
  (let [f (case op
            ">"  >
            "<"  <
            ">=" >=
            "<=" <=
            "==" =
            "!=" not=)]
    (fn [registry]
      (f (get registry reg 0) value))))

(defn make-op
  "Creates an operation to apply to the state."
  [{:keys [reg op value]}]
  (let [f (case op
            "inc" +
            "dec" -)]
    (fn [registry]
      (let [v (get registry reg 0)]
        (assoc registry reg (f v value))))))

(defn reducer-fn
  "Apply the `instr`(uction) to the registry, and return the new registry."
  [registry instr]
  (let [pred-fn (make-pred (:pred instr))
        op-fn   (make-op instr)]
    (if (pred-fn registry)
      (op-fn registry)
      registry)))

(defn apply-instr
  "Apply a list of instructions on an empty registry."
  [instructions]
  (reduce reducer-fn {} instructions))

(defn solve-1 [lines]
  (apply max (vals (apply-instr (map parse-instr lines)))))

(defn solve-2 [lines]
  (let [instructions (map parse-instr lines)
        states       (reductions reducer-fn {} instructions)
        max-values   (map #(apply max %) (filter some? (map vals states)))]
    (apply max max-values)))

