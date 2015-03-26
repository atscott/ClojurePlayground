(ns Player
  (:gen-class))

(defn compute-moves [dx dy moves]
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
      (recur newDx newDy (cons (str yMove xMove) moves)))))

(defn -main [& args]
  (let [[lx ly tx ty] (doall (repeatedly 4 read))
        moves (compute-moves (- lx tx) (- ly ty) '())]
    (while true
      (doseq [move moves]
        (println move)))))

