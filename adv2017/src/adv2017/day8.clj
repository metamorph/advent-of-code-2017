(ns adv2017.day8)

(def test-lines
  ["b inc 5 if a > 1"
   "a inc 1 if b < 5"
   "c dec -10 if a >= 1"
   "c inc -20 if c == 10"])

(defn parse-line
  "TODO: Maybe try antlr instead, just for fun."
  [l]
  (if-let [[_ reg op v pred-reg pred-op pred-v]
           (re-find #"(\w+)\s+(\w+)\s+(\d+)\s+if\s+(\w+)\s+(\W+)\s+(\d+)" l)]
    {:reg   reg
     :op    op
     :value (Integer/parseInt v)
     :pred  {:reg   pred-reg
             :op    pred-op
             :value (Integer/parseInt pred-v)}}))

(defn make-pred
  "Creates a predicate that checks if the registry is in a specific state"
  [{:keys [reg op value]}]
  (let [f (case op ">" >
                "<" <
                ">=" >=
                "<=" <=
                "==" =
                "!=" not=)]
    (fn [registry]
      (f (get registry reg 0) value))))
