(ns adv2017.day18
  (:require [clojure.core.async :as async]
            [clojure.java.io :as io]
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

(defn make-state [pid lines]
  (let [instructions (into [] (parse lines))]
    {:instructions instructions
     :index 0
     :pid pid}))

(defn make-state-2 [pid in out lines]
  (let [instructions (into [] (parse lines))]
    {:p {:value pid}
     :instructions instructions
     :index 0
     :in in
     :out out
     :pid pid}))

(defn with-inc-index [s]
  (update s :index inc))

(defn break [reason s]
  (-> (assoc s :break? true)
      (assoc :reason reason)))

(defn on-snd [s r]
  (println "SND" r)
  (with-inc-index
    (let [v (get-in s [r :value] 0)]
      (assoc-in s [r :played] v))))

(defn reg-or-value [state r]
  (if (keyword? r)
    (get-in state [r :value] 0)
    r))

(defn on-snd-2 [{:as state
               :keys [out pid]} r]
  (println pid " : Blocking put")
  (async/>!! out (reg-or-value state r))
  (with-inc-index (update state :snd (fn [v] (+ (or v 0) 1)))))

(defn on-set [s r v]
  (with-inc-index
    (assoc-in s [r :value] v)))

(defn on-add [s r v]
  (with-inc-index
    (update-in s [r :value] #(+ (or % 0) v))))

(defn on-mul [s r v]
  (with-inc-index
    (update-in s [r :value] #(* (or % 0) v))))

(defn on-mod [s r v]
  (with-inc-index
    (update-in s [r :value] #(mod (or % 0) v))))

(defn on-rcv [s r]
  (let [v      (get-in s [r :value] 0)
        played (get-in s [r :played] 0)]
    (println "RCV" r)
    (if (not= 0 v)
      (break (str "RCV called on " (name r) ": " played)
             (on-set s r played))
      (with-inc-index s))))

(defn on-rcv-2 [{:as state
                 :keys [in pid]} r]
  (println pid " : Blocking to take...")
  (let [[v c] (async/alts!! [in (async/timeout 10000)])]
    (if v
      (with-inc-index (assoc-in state [r :value] v))
      (break "No value on RCV" state))))

(defn on-jgz [s r v]
  (let [rv (get-in s [r :value] 0)]
    (if (< 0 rv)
      (update s :index #(+ v %))
      (with-inc-index s))))

(defn step [{:keys [pid instructions index] :as state}]
  (if (<= 0 index (dec (count instructions)))

    (let [[op reg f & r] (get instructions index)]
      (let [new-state (case op
                        :snd (on-snd state reg)
                        :set (on-set state reg (f state))
                        :add (on-add state reg (f state))
                        :mul (on-mul state reg (f state))
                        :mod (on-mod state reg (f state))
                        :rcv (on-rcv state reg)
                        :jgz (on-jgz state reg (f state)))]
        new-state))


      (-> (assoc state :break? true)
          (assoc :reason "Out of bound"))))

(defn step-2 [{:keys [pid instructions index] :as state}]
  (if (<= 0 index (dec (count instructions)))

    (let [[op reg f & r] (get instructions index)]
      (let [new-state (case op
                        :snd (on-snd-2 state reg)
                        :set (on-set state reg (f state))
                        :add (on-add state reg (f state))
                        :mul (on-mul state reg (f state))
                        :mod (on-mod state reg (f state))
                        :rcv (on-rcv-2 state reg)
                        :jgz (on-jgz state reg (f state)))]
        new-state))


    (-> (assoc state :break? true)
        (assoc :reason "Out of bound"))))

(defn solve-1 [lines]
  (loop [state (make-state 0 lines)]
    (if (:break? state)
      (dissoc state :instructions)
      (recur (step state)))))

(defn solve-2 [lines]
  (let [pid-0-in (async/chan 9999)
        pid-0-out (async/chan 9999)
        pid-0-state (make-state-2 0 pid-0-in pid-0-out lines)
        pid-1-state (make-state-2 1 pid-0-out pid-0-in lines)

        pid-0 (async/thread (loop [state pid-0-state]
                              (if (:break? state)
                                (dissoc state :instructions)
                                (recur (step-2 state)))))

        pid-1 (async/thread (loop [state pid-1-state]
                              (if (:break? state)
                                (dissoc state :instructions)
                                (recur (step-2 state)))))]

    (let [[v1 c] (async/alts!! [pid-0 pid-1])]
      (let [v2 (async/<!! (if (= c pid-0) pid-1 pid-0))]
        (println "== COMPLETED ==")
        (println (:snd v1) " => " (dissoc v1  :instructions))
        (println (:snd v2) " => " (dissoc v2 :instructions))))))

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

(def test-lines-2 ["snd 1"
                   "snd 2"
                   "snd p"
                   "rcv a"
                   "rcv b"
                   "rcv c"
                   "rcv d"])

(def puzzle-lines (-> (io/resource "day18-input.txt") (slurp) (str/split-lines)))
