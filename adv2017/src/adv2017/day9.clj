(ns adv2017.day9)

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
  (let [cleaned-up (into '() (comp (make-drop-!-xf)
                                   (make-drop-garbage-xf)) chars)]

    ))
