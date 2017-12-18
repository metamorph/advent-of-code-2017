(ns adv2017.day18
  (:require [clojure.string :as str]))

(defn make-ref-fn
  "Create a function that will either lookup a value from a ref,
  or just return a value directly."
  [v]
  (if (re-matches #"\d+" v)
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

(defn apply-instr [state [op reg arg & _]]
  (case op
    :snd (assoc-in state [reg :played] (get-in state [reg :value] 0))
    :set (assoc-in state [reg :value] arg)
    :add (update-in state [reg :value] #(+ arg (or % 0)))
    :mul (update-in state [reg :value] #(* arg (or % 0)))
    :mod (update-in state [reg :value] #(mod (or % 0) arg))
    :rcv (update state reg (fn [{:keys [value played]
                                 :as   e}]
                             (if-not (zero? value)
                               (assoc e :value (or played 0))
                               e)))
    ;; jump
    :jgz (if (< 0 (get-in state [reg :value]))
           (update state :index #(+ % arg))
           state)))
