(ns adv2017.day18
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn make-ref-fn
  "Create a function that will either lookup a value from a ref,
  or just return a value directly."
  [v]
  (if (re-matches #"[-]*\d+" v)
    ;; Either return the hard value
    (constantly (Integer/parseInt v))
    ;; Or lookup the value from the state
    (let [reg (keyword v)]
      #(get-in % [reg :value] 0))))

(defn parse [lines]
  (map (fn [l]
         (let [[instr reg arg] (str/split (str/trim l) #"\s+")]
           (list (keyword instr)
                 (keyword reg)
                 (when arg (make-ref-fn arg))
                 arg)))
       lines))

(defn make-state [lines]
  (let [instructions (into [] (parse lines))]
    {:instructions instructions
     :index 0}))

(defn apply-instr [state [op reg f & r]]
  (let [break? (atom false)]
   (case op
     :snd (-> (assoc-in state [reg :played] (get-in state [reg :value] 0))
              (update :index inc))
     :set (-> (assoc-in state [reg :value] (f state))
              (update :index inc))
     :add (-> (update-in state [reg :value] #(+ (f state) (or % 0)))
              (update :index inc))
     :mul (-> (update-in state [reg :value] #(* (f state) (or % 0)))
              (update :index inc))
     :mod (-> (update-in state [reg :value] #(mod (or % 0) (f state)))
              (update :index inc))
     :rcv (-> (update state reg (fn [{:keys [value played]
                                      :as   e}]
                                  (if-not (zero? value)
                                    (do
                                      (println "Recovering value @" reg "->" played)
                                      (reset! break? true)
                                      (assoc e :value (or played 0)))
                                    e)))
              (update :index #(if @break? -10 (inc %))))
     ;; jump
     :jgz (if (< 0 (get-in state [reg :value]))
            (update state :index #(+ % (f state)))
            (update state :index inc)))))

(defn current-instr [{:keys [index instructions]}]
  (when (<= 0 index (dec (count instructions)))
    (get instructions index)))

(defn play [lines]
  (loop [state (make-state lines)]
    (if-let [instr (current-instr state)]
      (recur (apply-instr state instr))
      (dissoc state :instructions))))

(def test-lines ["set a 1"
                 "add a 2"
                 "mul a a"
                 "mod a 5"
                 "snd a"
                 "set a 0"
                 "rcv a"
                 "jgz a -1"
                 "set a 1"
                 "jgz a -2"])

(def puzzle-lines (-> (io/resource "day18-input.txt") (slurp) (str/split-lines)))
