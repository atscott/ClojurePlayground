(ns Player
  (:gen-class))

(defn compute-moves [dx dy]
  (loop [dx dx dy dy moves '()]
    (if (and (= 0 dy) (= 0 dx))
      moves
      (let [[newDx xMove] (cond
                            (= 0 dx) [0 nil]
                            (> dx 0) [(dec dx) \E]
                            :else [(inc dx) \W])
            [newDy yMove] (cond
                            (= 0 dy) [0 nil]
                            (> dy 0) [(dec dy) \S]
                            :else [(inc dy) \N])]
        (recur newDx newDy (cons (str yMove xMove) moves))))))

(defn compute-moves-awesome [dx dy]
  (let [absX (Math/abs dx)
        absY (Math/abs dy)
        nilNormalize (repeat (Math/abs (- absX absY)) nil)
        xDir (concat
               (repeat absX (if (neg? dx) \W \E))
               (when (< absX absY) nilNormalize))
        yDir (concat
               (repeat absY (if (neg? dy) \N \S))
               (when (< absY absX) nilNormalize))
        moves (map list yDir xDir)]
    (doseq [move moves] (println (apply str move)))))

(defn -main [& args]
  (let [[lx ly tx ty] (doall (repeatedly 4 read))
        moves (compute-moves-awesome (- lx tx) (- ly ty))]
    (while true
      (doseq [move moves]
        (println move)))))
