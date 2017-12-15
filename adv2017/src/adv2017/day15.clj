(ns adv2017.day15)

(def real-config {:gen-a {:seed 516
                          :factor 16807}
                  :gen-b {:seed 190
                          :factor 48271}
                  :iterations 40000000})

(def test-config (-> real-config
                     (assoc-in [:gen-a :seed] 65)
                     (assoc-in [:gen-b :seed] 8921)))

(defn with-step2-iterations [cfg] (assoc cfg :iterations 5000000))

(defn int->16bits [i]
  (bit-and i 0xffff))

(defn make-generator [seed factor]
  (drop 1 (iterate #(rem (* factor %) 2147483647) seed)))

(defn make-generator-fn [factor]
  (fn [v] (rem (* factor v) 2147483647)))

(defn solve-1-1 [{:keys              [iterations]
                  {seed-a   :seed
                   factor-a :factor} :gen-a
                  {seed-b   :seed
                   factor-b :factor} :gen-b}]
  (let [a-seq (make-generator-fn factor-a)
        b-seq (make-generator-fn factor-b)]
    (loop [result 0
           its    0
           a-value (a-seq seed-a)
           b-value (b-seq seed-b)]

      (if (= its iterations)
        result
        (if (= (int->16bits a-value)
               (int->16bits b-value))
          (recur (inc result) (inc its) (a-seq a-value) (b-seq b-value))
          (recur result (inc its) (a-seq a-value) (b-seq b-value)))))))

(defn solve-1 [{:keys [iterations]
                {seed-a   :seed
                 factor-a :factor} :gen-a
                {seed-b   :seed
                 factor-b :factor} :gen-b}]
  (count
   (filter (fn [[a b]] (= (int->16bits a)
                          (int->16bits b)))
           (take iterations
                 (pmap vector
                       (make-generator seed-a factor-a)
                       (make-generator seed-b factor-b))))))

(defn solve-2 [{:keys [iterations]
                {seed-a   :seed
                 factor-a :factor} :gen-a
                {seed-b   :seed
                 factor-b :factor} :gen-b
                :as cfg}]
  (println "Running with config:" cfg)
  (count
   (filter (fn [[a b]] (= (int->16bits a)
                          (int->16bits b)))
           (take iterations
                 (pmap vector
                       (filter #(zero? (mod % 4)) (make-generator seed-a factor-a))
                       (filter #(zero? (mod % 8)) (make-generator seed-b factor-b)))))))

